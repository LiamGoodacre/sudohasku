module Drawing
  ( appDraw,
    appAttrMap,
  )
where

import Brick qualified as Brick
import Brick.Widgets.Center qualified as Brick
import Brick.Widgets.Table qualified as Brick.Table
import Control.Lens qualified as Lens
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Domain
import Graphics.Vty qualified as Vty

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

drawCell :: SudokuState -> CellLoc Digit -> Brick.Widget names
drawCell sudokuState loc = do
  let val = Map.lookup loc (grid sudokuState)

  let highMarks = Map.lookup loc (highs sudokuState)
  let lowMarks = Map.lookup loc (lows sudokuState)

  let matching =
        match sudokuState & any \matchDigit ->
          case val of
            Just (Given given) -> given == matchDigit
            Just (Input (Just input)) -> input == matchDigit
            _ ->
              or
                [ fold highMarks & Set.member matchDigit,
                  fold lowMarks & Set.member matchDigit
                ]

  let givenOrInputOrBlank = case val of
        Just (Given _) -> CellContentGiven
        Just (Input (Just _)) -> CellContentInput
        _ -> CellContentBlank

  let cellState :: CellState Identity
      cellState =
        CellState
          { cellContent = pure givenOrInputOrBlank,
            cellSelected = pure (Set.member loc (selected sudokuState)),
            cellMatched = pure matching,
            cellFocussed = pure (focussed sudokuState == loc)
          }

  let cellAttrs = cellStateAttrName cellState

  let content = case val of
        Just (Given n) -> drawBigDigit n
        Just (Input (Just n)) -> drawBigDigit n
        _ -> drawMarks (fold highMarks) (fold lowMarks)

  Brick.withDefAttr cellAttrs content

padGrid :: Brick.Widget names -> Brick.Widget names
padGrid = Brick.padRight (Brick.Pad 2) . Brick.padBottom (Brick.Pad 1)

padGroup :: Brick.Widget names -> Brick.Widget names
padGroup = Brick.padTop (Brick.Pad 1) . Brick.padLeft (Brick.Pad 2)

drawGrid :: SudokuState -> Brick.Widget names
drawGrid sudokuState =
  padGrid $ drawBands \bigCol bigRow ->
    padGroup $ drawBands \lilCol lilRow -> do
      drawCell sudokuState $ bandedGridCellLocation bigCol bigRow lilCol lilRow

drawHelp :: Brick.Widget names
drawHelp = do
  let entries :: [(String, String, String)]
      entries =
        [ ("ACTION\n", "BINDING", "MODE"),
          ("Halt", "'Ctrl c'", "*"),
          ("Toggle help", "'?'", "*"),
          ("Set Mode Normal", "'i', 'Esc', 'Ctrl ['", "*"),
          ("Toggle enter digit", "'1-9'", "Normal"),
          ("Remove entered digit", "'d'", "Normal"),
          ("Set Mode Mark Highs", "'m'", "*"),
          ("Toggle high mark digit", "'1-9'", "Mark Highs"),
          ("Remove high marks", "'d'", "Mark Highs"),
          ("Set Mode Mark Lows", "'M'", "*"),
          ("Toggle low mark digit", "'1-9'", "Mark Lows"),
          ("Remove low marks", "'d'", "Mark Lows"),
          ("Set Mode Highlight", "'/'", "*"),
          ("Highlight focussed", "'/'", "Highlight"),
          ("Highlight digit", "'1-9'", "Highlight"),
          ("Set Mode Jump", "'g'", "*"),
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

screenDraw :: SudokuState -> Brick.Widget names
screenDraw sudokuState =
  Brick.vBox
    [ Brick.hBox
        [ Brick.str $ "Mode = " <> show (mode sudokuState),
          Brick.str ", ",
          Brick.str $ "Matching = " <> maybe "none" (show . digitToInt) (match sudokuState)
        ],
      drawGrid sudokuState
        & Brick.withDefAttr
          case mode sudokuState of
            Normal -> Brick.attrName "mode-normal"
            Mark Highs -> Brick.attrName "mode-high-mark"
            Mark Lows -> Brick.attrName "mode-low-mark"
            Highlight -> Brick.attrName "mode-highlight"
            Jump -> Brick.attrName "mode-jump",
      Brick.str $ "Last action = " <> maybe "none" show (lastAction sudokuState)
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

appDraw :: SudokuState -> [Brick.Widget names]
appDraw sudokuState =
  fold
    [ if showHelp sudokuState then [drawHelp] else [],
      [ Brick.center do
          Brick.vBox
            [ Brick.hCenter logoDraw,
              Brick.hCenter (screenDraw sudokuState)
            ]
      ]
    ]

data CellContent = CellContentGiven | CellContentBlank | CellContentInput

instance Universe CellContent where
  universe = [CellContentGiven, CellContentBlank, CellContentInput]

data CellState f = CellState
  { cellContent :: f CellContent,
    cellSelected :: f Bool,
    cellMatched :: f Bool,
    cellFocussed :: f Bool
  }

traverseCellState :: Applicative h => (forall i. g i -> h i) -> CellState g -> h (CellState Identity)
traverseCellState t cs = do
  cellContent <- Identity <$> t (cellContent cs)
  cellSelected <- Identity <$> t (cellSelected cs)
  cellMatched <- Identity <$> t (cellMatched cs)
  cellFocussed <- Identity <$> t (cellFocussed cs)
  pure CellState {..}

cellStateAttrName :: CellState Identity -> Brick.AttrName
cellStateAttrName CellState {..} =
  foldMap
    Brick.attrName
    [ case runIdentity cellContent of
        CellContentGiven -> "given"
        CellContentBlank -> "blank"
        CellContentInput -> "input",
      case runIdentity cellSelected of
        False -> "noselect"
        True -> "select",
      case runIdentity cellMatched of
        False -> "nomatch"
        True -> "match",
      case runIdentity cellFocussed of
        False -> "nofocus"
        True -> "focus",
      "cell"
    ]

data Pat a where
  All :: Universe a => Pat a
  Any :: [a] -> Pat a

anyCellState :: CellState Pat
anyCellState =
  CellState
    { cellContent = All,
      cellSelected = All,
      cellMatched = All,
      cellFocussed = All
    }

resolvePat :: Pat a -> [a]
resolvePat All = universe
resolvePat (Any xs) = xs

withCell :: CellState Pat -> CellState Pat
withCell = id

withContent :: [CellContent] -> CellState Pat -> CellState Pat
withContent xs cs = cs {cellContent = Any xs}

withSelected :: [Bool] -> CellState Pat -> CellState Pat
withSelected xs cs = cs {cellSelected = Any xs}

withMatched :: [Bool] -> CellState Pat -> CellState Pat
withMatched xs cs = cs {cellMatched = Any xs}

withFocussed :: [Bool] -> CellState Pat -> CellState Pat
withFocussed xs cs = cs {cellFocussed = Any xs}

data Styling t where
  ForeColour :: Styling Vty.Color
  ForeBright :: Styling Bool
  BackColour :: Styling Vty.Color
  BackBright :: Styling Bool
  IsBold :: Styling Bool

-- import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
-- deriveGEq ''Styling
-- deriveGCompare ''Styling

instance GEq Styling where
  geq ForeColour ForeColour = Just Lens.Refl
  geq ForeBright ForeBright = Just Lens.Refl
  geq BackColour BackColour = Just Lens.Refl
  geq BackBright BackBright = Just Lens.Refl
  geq IsBold IsBold = Just Lens.Refl
  geq _ _ = Nothing

instance GCompare Styling where
  gcompare ForeColour ForeColour = GEQ
  gcompare ForeColour _ = GLT
  gcompare _ ForeColour = GGT
  gcompare ForeBright ForeBright = GEQ
  gcompare ForeBright _ = GLT
  gcompare _ ForeBright = GGT
  gcompare BackColour BackColour = GEQ
  gcompare BackColour _ = GLT
  gcompare _ BackColour = GGT
  gcompare BackBright BackBright = GEQ
  gcompare BackBright _ = GLT
  gcompare _ BackBright = GGT
  gcompare IsBold IsBold = GEQ

type Style = DMap Styling Identity

infix 0 .==

(.==) :: Styling t -> t -> Style -> Style
(.==) k c s = DMap.insert k (Identity c) s

compose :: [a -> a] -> a -> a
compose = foldr (.) id

brightISO :: Bool -> Vty.Color -> Vty.Color
brightISO True (Vty.ISOColor n) = Vty.ISOColor (mod n 8 + 8)
brightISO _ c = c

styleAttr :: (Style -> Style) -> Vty.Attr -> Vty.Attr
styleAttr onStyle = do
  let stylings :: DMap Styling Identity
      stylings = onStyle DMap.empty

  let foreBright :: Bool
      foreBright = any runIdentity (DMap.lookup ForeBright stylings)

  let backBright :: Bool
      backBright = any runIdentity (DMap.lookup BackBright stylings)

  compose
    [ DMap.lookup ForeColour stylings & maybe id \(Identity foreColour) ->
        (`Vty.withForeColor` brightISO foreBright foreColour),
      DMap.lookup BackColour stylings & maybe id \(Identity backColour) ->
        (`Vty.withBackColor` brightISO backBright backColour),
      DMap.lookup IsBold stylings & maybe id \(Identity isBold) -> case isBold of
        False -> (`Vty.withStyle` Vty.defaultStyleMask)
        True -> (`Vty.withStyle` Vty.bold)
    ]

rule :: a -> [b -> b] -> (a, b -> b)
rule a b = (a, compose b)

cascade ::
  [(CellState Pat -> CellState Pat, Style -> Style)] ->
  [(Brick.AttrName, Vty.Attr)]
cascade xs = do
  let expanded :: [(Brick.AttrName, Style -> Style)]
      expanded = do
        (csp, sty) <- xs
        cs <- traverseCellState resolvePat (csp anyCellState)
        [(cellStateAttrName cs, sty)]

  let onAttrs :: (Style -> Style) -> Vty.Attr
      onAttrs st = styleAttr st Vty.defAttr

  Map.toList $ Map.fromListWith (.) expanded <&> onAttrs

cellAttrMap :: [(Brick.AttrName, Vty.Attr)]
cellAttrMap =
  cascade
    [ rule
        withCell
        [BackColour .== Vty.white],
      rule
        (withContent [CellContentGiven])
        [ForeColour .== Vty.black],
      rule
        (withContent [CellContentBlank, CellContentInput])
        [ForeColour .== Vty.blue],
      rule
        (withFocussed [True])
        [BackBright .== True],
      rule
        (withMatched [True])
        [ IsBold .== True,
          BackColour .== Vty.green
        ],
      rule
        (withSelected [True])
        [BackColour .== Vty.cyan]
    ]

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
        cellAttrMap
      ]
