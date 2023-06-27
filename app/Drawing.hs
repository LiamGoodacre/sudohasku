module Drawing
  ( appDraw,
    appAttrMap,
  )
where

import Brick qualified as Brick
import Brick.Widgets.Center qualified as Brick
import Brick.Widgets.Table qualified as Brick.Table
import Control.Lens ((%~), (.~))
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Domain
import Graphics.Vty qualified as Vty
import VtyOptic qualified as Vty

data CellState t = CellState
  { cellGiven :: !t,
    cellMatch :: !t,
    cellSelect :: !t,
    cellFocus :: !t
  }
  deriving stock (Foldable, Functor, Traversable)

instance Applicative CellState where
  pure v = CellState v v v v
  l <*> r =
    CellState
      { cellGiven = cellGiven l (cellGiven r),
        cellMatch = cellMatch l (cellMatch r),
        cellSelect = cellSelect l (cellSelect r),
        cellFocus = cellFocus l (cellFocus r)
      }

instance Universe t => Universe (CellState t) where
  universe = sequence $ pure universe

cellStateFeatureNames :: CellState String
cellStateFeatureNames =
  CellState
    { cellGiven = "given",
      cellMatch = "match",
      cellSelect = "select",
      cellFocus = "focus"
    }

cellStateName :: CellState Bool -> Brick.AttrName
cellStateName cs =
  let flag :: String -> Bool -> Brick.AttrName
      flag m b = Brick.attrName $ show (m, b)
   in Brick.attrName "cell" <> fold (flag <$> cellStateFeatureNames <*> cs)

cellStateStyle :: CellState Bool -> Vty.Attr -> Vty.Attr
cellStateStyle CellState {..} = do
  let (.==>.) b v = if b then v else id
  compose $
    reverse
      [ Vty._attrBackColor .~ Vty.SetTo Vty.white,
        cellMatch .==>. (`Vty.withStyle` Vty.bold),
        cellMatch .==>. (Vty._attrBackColor .~ Vty.SetTo Vty.green),
        cellSelect .==>. (Vty._attrBackColor .~ Vty.SetTo Vty.cyan),
        Vty._attrBackColor . Vty._SetTo %~ (cellFocus .==>. Vty.brightISO),
        Vty._attrForeColor .~ Vty.SetTo if cellGiven then Vty.black else Vty.blue
      ]

cellStateAttrMap :: [(Brick.AttrName, Vty.Attr)]
cellStateAttrMap =
  universe @(CellState Bool) <&> \cellState ->
    (cellStateName cellState, cellStateStyle cellState Vty.defAttr)

drawMarks :: Set Digit -> Set Digit -> Brick.Widget names
drawMarks highs lows = do
  let go marks =
        Brick.hLimit 5 $ Brick.hCenter do
          Brick.strWrap $
            marks
              & foldMap (show . digitToInt)
              & List.splitAt 5
              & (\(l1, l2) -> l1 <> " " <> l2)

  Brick.padLeftRight 1 $
    Brick.vBox
      [ Brick.vLimit 2 (Brick.padTop Brick.Max $ go highs)
          & Brick.withDefAttr (Brick.attrName "highs"),
        Brick.vLimit 2 (Brick.padBottom Brick.Max $ go lows)
          & Brick.withDefAttr (Brick.attrName "lows")
      ]

drawBigDigit :: Digit -> Brick.Widget names
drawBigDigit =
  Brick.vBox . map Brick.str . \case
    D1 -> ["   ┐   ", "   │   ", "   │   ", "   ┴   "]
    D2 -> ["  ┌─┐  ", "  ┌─┘  ", "  │    ", "  └─┘  "]
    D3 -> ["  ┌─┐  ", "   ─┤  ", "    │  ", "  └─┘  "]
    D4 -> ["  ┐ ┌  ", "  └─┤  ", "    │  ", "    ┴  "]
    D5 -> ["  ┌─┐  ", "  └─┐  ", "    │  ", "  └─┘  "]
    D6 -> ["  ┌─┐  ", "  ├─┐  ", "  │ │  ", "  └─┘  "]
    D7 -> ["  ┌─┐  ", "    │  ", "    │  ", "    ┴  "]
    D8 -> ["  ┌─┐  ", "  ├─┤  ", "  │ │  ", "  └─┘  "]
    D9 -> ["  ┌─┐  ", "  └─┤  ", "    │  ", "  └─┘  "]

drawBands ::
  (Band -> Band -> Brick.Widget names) ->
  Brick.Widget names
drawBands f =
  Brick.hBox $
    bands <&> \col ->
      Brick.vBox $
        bands <&> \row ->
          f col row

drawCell :: Game -> CellLoc Digit -> Brick.Widget names
drawCell game loc = do
  let val = Map.lookup loc (grid $ sudoku game)

  let highMarks = Map.lookup loc (highs $ sudoku game)
  let lowMarks = Map.lookup loc (lows $ sudoku game)

  let matching =
        match game & any \matchDigit ->
          case val of
            Just (Given given) -> given == matchDigit
            Just (Input (Just input)) -> input == matchDigit
            _ ->
              or
                [ fold highMarks & Set.member matchDigit,
                  fold lowMarks & Set.member matchDigit
                ]

  let isGiven = case val of
        Just (Given _) -> True
        _ -> False

  let cellState :: CellState Bool
      cellState =
        CellState
          { cellGiven = isGiven,
            cellSelect = Set.member loc (selected game),
            cellMatch = matching,
            cellFocus = focussed game == loc
          }

  let cellAttrs = cellStateName cellState

  let content = case val of
        Just (Given n) -> drawBigDigit n
        Just (Input (Just n)) -> drawBigDigit n
        _ -> drawMarks (fold highMarks) (fold lowMarks)

  Brick.withDefAttr cellAttrs content

padGrid :: Brick.Widget names -> Brick.Widget names
padGrid = Brick.padRight (Brick.Pad 2) . Brick.padBottom (Brick.Pad 1)

padGroup :: Brick.Widget names -> Brick.Widget names
padGroup = Brick.padTop (Brick.Pad 1) . Brick.padLeft (Brick.Pad 2)

drawGrid :: Game -> Brick.Widget names
drawGrid game =
  padGrid $ drawBands \bigCol bigRow ->
    padGroup $ drawBands \lilCol lilRow -> do
      drawCell game $ bandedGridCellLocation bigCol bigRow lilCol lilRow

drawHelp :: Brick.Widget names
drawHelp = do
  let entries :: [(String, String, String)]
      entries =
        [ ("ACTION\n", "BINDING", "MODE"),
          ("Halt", "'Ctrl c'", "*"),
          ("Toggle help", "'?'", "*"),
          ("Undo", "'u'", "*"),
          ("Redo", "'r'", "*"),
          ("Set mode to Insert", "'i', 'Esc', 'Ctrl ['", "*"),
          ("Toggle enter digit", "'1-9'", "Insert"),
          ("Remove entered digit", "'d'", "Insert"),
          ("Set mode to Mark Highs", "'m'", "*"),
          ("Toggle high mark digit", "'1-9'", "Mark Highs"),
          ("Remove high marks", "'d'", "Mark Highs"),
          ("Set mode to Mark Lows", "'M'", "*"),
          ("Toggle low mark digit", "'1-9'", "Mark Lows"),
          ("Remove low marks", "'d'", "Mark Lows"),
          ("Set mode to Highlight", "'/'", "*"),
          ("Highlight focussed", "'/'", "Highlight"),
          ("Highlight digit", "'1-9'", "Highlight"),
          ("Set mode to Jump", "'g'", "*"),
          ("Jump to box", "'1-9'", "Jump"),
          ("No Highlight", "'!'", "*"),
          ("Toggle select cell", "'x'", "*"),
          ("Clear selection", "'X'", "*"),
          ("Move focus North", "'k'", "*"),
          ("Move focus East", "'l'", "*"),
          ("Move focus South", "'j'", "*"),
          ("Move focus West", "'h'", "*"),
          ("Select & move North", "'K'", "*"),
          ("Select & move East", "'L'", "*"),
          ("Select & move South", "'J'", "*"),
          ("Select & move West", "'H'", "*")
        ]

  let tableRows =
        entries <&> \(d, k, m) ->
          [ Brick.str $ " " <> d <> " ",
            Brick.str $ " " <> k <> " ",
            Brick.str $ " " <> m <> " "
          ]

  let helpTable =
        Brick.Table.renderTable
          ( Brick.Table.table tableRows
              & Brick.Table.rowBorders False
              & Brick.Table.surroundingBorder False
          )
          & Brick.padTopBottom 1
          & Brick.padLeftRight 1
          & Brick.withDefAttr (Brick.attrName "help-table")

  let window =
        Brick.Table.renderTable
          ( Brick.Table.table
              [ [Brick.str "Help"],
                [helpTable]
              ]
              & Brick.Table.alignCenter 0
              & Brick.Table.surroundingBorder False
              & Brick.Table.rowBorders False
          )
          & Brick.withDefAttr (Brick.attrName "help")

  Brick.centerLayer window

screenDraw :: Game -> Brick.Widget names
screenDraw game =
  Brick.vBox
    [ Brick.hBox
        [ Brick.str $ "Mode = " <> show (mode game),
          Brick.str ", ",
          Brick.str $ "Matching = " <> maybe "none" (show . digitToInt) (match game)
        ],
      drawGrid game
        & Brick.withDefAttr
          case mode game of
            Insert -> Brick.attrName "mode-normal"
            Mark Highs -> Brick.attrName "mode-high-mark"
            Mark Lows -> Brick.attrName "mode-low-mark"
            Highlight -> Brick.attrName "mode-highlight"
            Jump -> Brick.attrName "mode-jump",
      Brick.str $ "Last action = " <> maybe "none" show (lastAction game)
    ]

logoDraw :: Brick.Widget names
logoDraw =
  Brick.withDefAttr (Brick.attrName "logo") do
    Brick.vBox $
      map
        Brick.str
        [ " ┌─┐ ┐ ┌ ┬─┐ ┌─┐ ┐ ┌ ┌─┐ ┌─┐ ┐ ┐ ┐ ┌ ",
          " └─┐ │ │ │ │ │ │ ├─┤ ├─┤ └─┐ ├─┘ │ │ ",
          "   │ │ │ │ │ │ │ │ │ │ │   │ ├─┐ │ │ ",
          " └─┘ └─┘ ┴─┘ └─┘ ┘ └ ┘ └ └─┘ ┘ ┘ └─┘ "
        ]

appDraw :: Game -> [Brick.Widget names]
appDraw game =
  fold
    [ if showHelp game then [drawHelp] else [],
      [ Brick.center do
          Brick.vBox
            [ Brick.hCenter logoDraw,
              Brick.hCenter (screenDraw game)
            ]
      ]
    ]

compose :: [a -> a] -> a -> a
compose = foldr (.) id

modeAttrMap :: [(Brick.AttrName, Vty.Attr)]
modeAttrMap =
  [ ( Brick.attrName "mode-normal",
      Vty.defAttr
        `Vty.withBackColor` Vty.red
    ),
    ( Brick.attrName "mode-high-mark",
      Vty.defAttr
        `Vty.withBackColor` Vty.brightBlue
    ),
    ( Brick.attrName "mode-low-mark",
      Vty.defAttr
        `Vty.withBackColor` Vty.blue
    ),
    ( Brick.attrName "mode-highlight",
      Vty.defAttr
        `Vty.withBackColor` Vty.green
    ),
    ( Brick.attrName "mode-jump",
      Vty.defAttr
        `Vty.withBackColor` Vty.magenta
    )
  ]

helpAttrMap :: [(Brick.AttrName, Vty.Attr)]
helpAttrMap =
  [ ( Brick.attrName "help",
      Vty.defAttr
        `Vty.withForeColor` Vty.brightWhite
        `Vty.withBackColor` Vty.cyan
    ),
    ( Brick.attrName "help-table",
      Vty.defAttr
        `Vty.withForeColor` Vty.black
        `Vty.withBackColor` Vty.brightWhite
    )
  ]

logoAttrMap :: [(Brick.AttrName, Vty.Attr)]
logoAttrMap =
  [ ( Brick.attrName "logo",
      Vty.defAttr
        `Vty.withForeColor` Vty.brightWhite
        `Vty.withBackColor` Vty.cyan
    )
  ]

appAttrMap :: Brick.AttrMap
appAttrMap =
  Brick.attrMap Vty.defAttr $
    fold
      [ logoAttrMap,
        helpAttrMap,
        modeAttrMap,
        cellStateAttrMap
      ]
