module Main where

import Brick qualified as Brick
import Brick.Widgets.Center qualified as Brick
import Brick.Widgets.Table qualified as Brick.Table
import Control.Lens (Lens', (%~), (.~), (^?))
import Control.Lens qualified as Lens
import Control.Monad.State (evalState, state)
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Graphics.Vty qualified as Vty
import System.Environment qualified as Env
import System.Random qualified as Random
import System.Random.Shuffle qualified as Shuffle

class Universe t where
  universe :: [t]

instance Universe Bool where
  universe = [False, True]

data Names
  deriving stock (Show, Eq, Ord)

data Digit = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving stock (Show, Eq, Ord, Enum, Bounded)

digits :: [Digit]
digits = [D1 .. D9]

instance Universe Digit where
  universe = digits

digitToInt :: Digit -> Int
digitToInt = succ . fromEnum

digitFromInt :: Int -> Digit
digitFromInt i = digits !! mod (pred i) 9

surroundingDigits :: Digit -> (Digit, Digit)
surroundingDigits (digitToInt -> d) =
  ( digitFromInt $ pred d,
    digitFromInt $ succ d
  )

nextDigit :: Digit -> Digit
nextDigit = snd . surroundingDigits

prevDigit :: Digit -> Digit
prevDigit = fst . surroundingDigits

data Band = B1 | B2 | B3
  deriving stock (Show, Eq, Ord, Enum, Bounded)

bands :: [Band]
bands = [B1, B2, B3]

bandToInt :: Band -> Int
bandToInt = succ . fromEnum

bandFromInt :: Int -> Band
bandFromInt i = bands !! mod (pred i) 3

instance Universe Band where
  universe = bands

newtype Col v = Col v
  deriving newtype (Show, Eq, Ord, Bounded)

newtype Row v = Row v
  deriving newtype (Show, Eq, Ord, Bounded)

data Loc v = Loc (Col v) (Row v)
  deriving stock (Show, Eq, Ord, Bounded)

data Cell = Given Digit | Input (Maybe Digit)
  deriving stock (Show, Eq, Ord)

type Grid = Map (Loc Digit) Cell

type Marks = Map (Loc Digit) (Set Digit)

data HighsOrLows = Highs | Lows
  deriving stock (Show, Eq, Ord)

data Mode = Normal | Mark HighsOrLows | Highlight | Jump
  deriving stock (Show, Eq, Ord)

data Direction = North | East | South | West
  deriving stock (Show, Eq, Ord)

data Selection = Expand | Reset | Ignore
  deriving stock (Show, Eq, Ord)

data Action
  = ToggleHelp
  | ClearSelect
  | ToggleSelect
  | SwitchMode Mode
  | NoHighlight
  | Move Selection Direction
  | Enter Digit
  | Remove
  deriving stock (Show, Eq, Ord)

data SudokuState = MkSudokuState
  { showHelp :: Bool,
    grid :: Grid,
    highs :: Marks,
    lows :: Marks,
    selected :: Set (Loc Digit),
    focussed :: Loc Digit,
    mode :: Mode,
    match :: Maybe Digit,
    lastAction :: Maybe Action
  }
  deriving stock (Show, Eq, Ord)

onLocCol :: Lens' (Loc v) v
onLocCol t (Loc (Col c) r) = t c <&> \v -> Loc (Col v) r

onLocRow :: Lens' (Loc v) v
onLocRow t (Loc c (Row r)) = t r <&> \v -> Loc c (Row v)

onGiven :: Lens.Prism' Cell Digit
onGiven = Lens.prism' Given \case
  Given v -> Just v
  _ -> Nothing

onInput :: Lens.Prism' Cell (Maybe Digit)
onInput = Lens.prism' Input \case
  Input v -> Just v
  _ -> Nothing

onShowHelp :: Lens' SudokuState Bool
onShowHelp t s = t (showHelp s) <&> \v -> s {showHelp = v}

onGrid :: Lens' SudokuState Grid
onGrid t s = t (grid s) <&> \v -> s {grid = v}

onFocus :: Lens' SudokuState (Loc Digit)
onFocus t s = t (focussed s) <&> \v -> s {focussed = v}

onSelected :: Lens' SudokuState (Set (Loc Digit))
onSelected t s = t (selected s) <&> \v -> s {selected = v}

onHighs :: Lens' SudokuState Marks
onHighs t s = t (highs s) <&> \v -> s {highs = v}

onLows :: Lens' SudokuState Marks
onLows t s = t (lows s) <&> \v -> s {lows = v}

onMode :: Lens' SudokuState Mode
onMode t s = t (mode s) <&> \v -> s {mode = v}

onMatch :: Lens' SudokuState (Maybe Digit)
onMatch t s = t (match s) <&> \v -> s {match = v}

onLastAction :: Lens' SudokuState (Maybe Action)
onLastAction t s = t (lastAction s) <&> \v -> s {lastAction = v}

onHighLows :: HighsOrLows -> Lens' SudokuState Marks
onHighLows = \case
  Highs -> onHighs
  Lows -> onLows

shiftFocus :: Direction -> SudokuState -> SudokuState
shiftFocus = \case
  North -> onFocus . onLocRow %~ prevDigit
  East -> onFocus . onLocCol %~ nextDigit
  South -> onFocus . onLocRow %~ nextDigit
  West -> onFocus . onLocCol %~ prevDigit

boxToCenterMidLoc :: Digit -> Loc Digit
boxToCenterMidLoc (digitToInt -> d) = do
  let (bigRow, bigCol) = divMod (d - 1) 3
  bandedGridLocation
    (bandFromInt (bigCol + 1))
    (bandFromInt (bigRow + 1))
    B2
    B2

jumpFocus :: Digit -> SudokuState -> SudokuState
jumpFocus d = onFocus .~ boxToCenterMidLoc d

updateSelection :: Selection -> SudokuState -> SudokuState
updateSelection = \case
  Expand -> \st -> st & onSelected %~ Set.insert (focussed st)
  Reset -> onSelected .~ Set.empty
  Ignore -> id

onMarks :: HighsOrLows -> Set (Loc Digit) -> Lens.Traversal' SudokuState (Set Digit)
onMarks highLow targets =
  onHighLows highLow
    . Lens.itraversed
    . Lens.indices (`Set.member` targets)

updateMarks :: HighsOrLows -> Digit -> SudokuState -> SudokuState
updateMarks highLow digit st = do
  let targets = Set.insert (focussed st) (selected st)

  let cellDigitMark :: Lens.Traversal' SudokuState Bool
      cellDigitMark = onMarks highLow targets . Lens.contains digit

  st & cellDigitMark .~ not (st & Lens.andOf cellDigitMark)

enter :: Digit -> SudokuState -> SudokuState
enter digit st = do
  let targets = Set.insert (focussed st) (selected st)

  let cellDigit :: Lens.Traversal' SudokuState (Maybe Digit)
      cellDigit =
        onGrid
          . Lens.itraversed
          . Lens.indices (`Set.member` targets)
          . onInput

  let g = st & Lens.allOf cellDigit (== Just digit)

  st & cellDigit .~ if g then Nothing else Just digit

updateMatch :: Maybe Digit -> SudokuState -> SudokuState
updateMatch digit st =
  st & onMatch .~ digit

removeNormal :: SudokuState -> SudokuState
removeNormal st = do
  let targets = Set.insert (focussed st) (selected st)

  let cellDigit :: Lens.Traversal' SudokuState (Maybe Digit)
      cellDigit =
        onGrid
          . Lens.itraversed
          . Lens.indices (`Set.member` targets)
          . onInput

  st & cellDigit .~ Nothing

removeMarks :: HighsOrLows -> SudokuState -> SudokuState
removeMarks highLow st = do
  let targets = Set.insert (focussed st) (selected st)

  let cellMark :: Lens.Traversal' SudokuState (Set Digit)
      cellMark = onMarks highLow targets

  st & cellMark .~ Set.empty

removeHighlight :: SudokuState -> SudokuState
removeHighlight st = st

matchFocus :: SudokuState -> SudokuState
matchFocus st = do
  let maybeDigit = do
        cell <- st ^? onGrid . Lens.itraversed . Lens.index (focussed st)
        case cell of
          Given d -> Just d
          Input i -> i

  st & updateMatch maybeDigit

actOn :: Action -> Brick.EventM Names SudokuState ()
actOn = \case
  ToggleHelp ->
    Brick.modify \st -> st & onShowHelp %~ not
  ClearSelect ->
    Brick.modify \st -> st & onSelected .~ Set.empty
  ToggleSelect ->
    Brick.modify \st -> st & onSelected . Lens.contains (focussed st) %~ not
  SwitchMode m -> do
    Brick.modify \st ->
      st
        & onMode .~ m
        & case (m, mode st) of
          (Highlight, Highlight) -> matchFocus
          _ -> id
  Move selection direction -> do
    Brick.modify \st ->
      st
        & updateSelection selection
        & shiftFocus direction
  Enter digit -> do
    Brick.modify \st ->
      case mode st of
        Normal -> st & enter digit
        Mark highLow -> st & updateMarks highLow digit
        Highlight -> st & updateMatch (Just digit)
        Jump -> st & jumpFocus digit
  Remove -> do
    Brick.modify \st ->
      case mode st of
        Normal -> st & removeNormal
        Mark highLow -> st & removeMarks highLow
        Highlight -> st & removeHighlight
        Jump -> st
  NoHighlight -> do
    Brick.modify \st -> st & updateMatch Nothing

act :: Action -> Brick.EventM Names SudokuState ()
act action = do
  actOn action
  Brick.modify \st -> st & onLastAction .~ Just action

smallDigitToString :: Digit -> String
smallDigitToString = show . succ . fromEnum

drawSmallDigit :: Maybe Digit -> Brick.Widget Names
drawSmallDigit = Brick.str . maybe " " smallDigitToString

drawMarks :: Set Digit -> Set Digit -> Brick.Widget Names
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

drawBigDigit :: Digit -> Brick.Widget Names
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
  (Band -> Band -> Brick.Widget Names) ->
  Brick.Widget Names
drawBands f =
  Brick.hBox $
    bands <&> \col ->
      Brick.vBox $
        bands <&> \row ->
          f col row

bandedGridLocation :: Band -> Band -> Band -> Band -> Loc Digit
bandedGridLocation bigCol bigRow lilCol lilRow = do
  let col = Col $ digitFromInt $ ((bandToInt bigCol - 1) * 3) + bandToInt lilCol
  let row = Row $ digitFromInt $ ((bandToInt bigRow - 1) * 3) + bandToInt lilRow
  Loc col row

drawCell :: SudokuState -> Loc Digit -> Brick.Widget Names
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

padGrid :: Brick.Widget Names -> Brick.Widget Names
padGrid = Brick.padRight (Brick.Pad 2) . Brick.padBottom (Brick.Pad 1)

padGroup :: Brick.Widget Names -> Brick.Widget Names
padGroup = Brick.padTop (Brick.Pad 1) . Brick.padLeft (Brick.Pad 2)

drawGrid :: SudokuState -> Brick.Widget Names
drawGrid sudokuState =
  padGrid $ drawBands \bigCol bigRow ->
    padGroup $ drawBands \lilCol lilRow -> do
      drawCell sudokuState $ bandedGridLocation bigCol bigRow lilCol lilRow

drawHelp :: Brick.Widget Names
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

screenDraw :: SudokuState -> Brick.Widget Names
screenDraw sudokuState =
  Brick.vBox
    [ Brick.hBox
        [ Brick.str $ "Mode = " <> show (mode sudokuState),
          Brick.str ", ",
          Brick.str $ "Matching = " <> maybe "none" smallDigitToString (match sudokuState)
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

logoDraw :: Brick.Widget Names
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

appDraw :: SudokuState -> [Brick.Widget Names]
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

keyCharDigit :: Char -> Maybe Digit
keyCharDigit = \case
  '1' -> Just D1
  '2' -> Just D2
  '3' -> Just D3
  '4' -> Just D4
  '5' -> Just D5
  '6' -> Just D6
  '7' -> Just D7
  '8' -> Just D8
  '9' -> Just D9
  _ -> Nothing

appHandleEvent :: Brick.BrickEvent Names e -> Brick.EventM Names SudokuState ()
appHandleEvent = \case
  Brick.VtyEvent vtyEvent -> case vtyEvent of
    Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> Brick.halt
    Vty.EvKey (Vty.KChar '?') _ -> act ToggleHelp
    Vty.EvKey Vty.KEsc _ -> act (SwitchMode Normal)
    Vty.EvKey (Vty.KChar '[') [Vty.MCtrl] -> act (SwitchMode Normal)
    Vty.EvKey (Vty.KChar 'i') _ -> act (SwitchMode Normal)
    Vty.EvKey (Vty.KChar 'm') _ -> act (SwitchMode (Mark Highs))
    Vty.EvKey (Vty.KChar 'M') _ -> act (SwitchMode (Mark Lows))
    Vty.EvKey (Vty.KChar '/') _ -> act (SwitchMode Highlight)
    Vty.EvKey (Vty.KChar 'g') _ -> act (SwitchMode Jump)
    Vty.EvKey (Vty.KChar '!') _ -> act NoHighlight
    Vty.EvKey (Vty.KChar 'x') _ -> act ToggleSelect
    Vty.EvKey (Vty.KChar 'X') _ -> act ClearSelect
    Vty.EvKey (Vty.KChar 'd') _ -> act Remove
    Vty.EvKey (Vty.KChar 'k') _ -> act (Move Ignore North)
    Vty.EvKey (Vty.KChar 'l') _ -> act (Move Ignore East)
    Vty.EvKey (Vty.KChar 'j') _ -> act (Move Ignore South)
    Vty.EvKey (Vty.KChar 'h') _ -> act (Move Ignore West)
    Vty.EvKey (Vty.KChar 'K') _ -> act (Move Expand North)
    Vty.EvKey (Vty.KChar 'L') _ -> act (Move Expand East)
    Vty.EvKey (Vty.KChar 'J') _ -> act (Move Expand South)
    Vty.EvKey (Vty.KChar 'H') _ -> act (Move Expand West)
    Vty.EvKey (Vty.KChar c) _
      | Just d <- keyCharDigit c -> act (Enter d)
    _ -> pure ()
  _ -> pure ()

app :: Brick.App SudokuState e Names
app =
  Brick.App
    { Brick.appDraw = appDraw,
      Brick.appChooseCursor = \_ _ -> Nothing,
      Brick.appHandleEvent = appHandleEvent,
      Brick.appStartEvent = pure (),
      Brick.appAttrMap = const appAttrMap
    }

initGrid :: r -> (Digit -> r) -> [[r]]
initGrid _____ is =
  [ [_____, is D6, _____, is D1, _____, _____, is D7, is D4, is D9],
    [_____, is D7, is D4, is D5, is D9, _____, is D6, is D1, is D2],
    [_____, is D2, _____, _____, _____, _____, is D3, is D5, _____],
    [_____, is D9, is D6, is D8, is D7, _____, _____, _____, is D5],
    [is D5, _____, _____, _____, _____, _____, _____, is D7, is D6],
    [is D7, _____, is D3, _____, _____, is D6, _____, _____, _____],
    [is D6, is D5, is D8, is D4, _____, _____, _____, _____, _____],
    [_____, _____, _____, _____, is D1, is D5, is D8, _____, _____],
    [is D9, is D1, _____, is D6, is D8, _____, _____, _____, is D4]
  ]

initMarks :: Marks
initMarks = Map.fromList do
  row <- digits
  col <- digits
  [(Loc (Col col) (Row row), Set.empty)]

data ShufflingSeeds a = ShufflingSeeds
  { _digitsSeed :: a,
    _bigColSeed :: a,
    _bigRowSeed :: a,
    _lilColSeed :: a,
    _lilRowSeed :: a
  }
  deriving stock (Functor, Foldable, Traversable)

instance Applicative ShufflingSeeds where
  pure x = ShufflingSeeds x x x x x
  l <*> r =
    ShufflingSeeds
      { _digitsSeed = _digitsSeed l (_digitsSeed r),
        _bigColSeed = _bigColSeed l (_bigColSeed r),
        _bigRowSeed = _bigRowSeed l (_bigRowSeed r),
        _lilColSeed = _lilColSeed l (_lilColSeed r),
        _lilRowSeed = _lilRowSeed l (_lilRowSeed r)
      }

shufflingSeeds :: Random.StdGen -> ShufflingSeeds Random.StdGen
shufflingSeeds = evalState $ sequence (pure $ state Random.split)

shuffleUniverse :: forall a. (Ord a, Universe a) => Random.StdGen -> a -> a
shuffleUniverse seed = do
  let unsafeIndex = (Map.!)
  -- under the assumption that universe does indeed contain all possible
  -- inhabitants of a, the map will be total
  unsafeIndex $ Map.fromList do
    let items = universe @a
    zip
      items
      (Shuffle.shuffle' items (length items) seed)

shuffleLoc :: ShufflingSeeds Random.StdGen -> Loc Digit -> Loc Digit
shuffleLoc ShufflingSeeds {..} = do
  let unsafeIndex = (Map.!)
  -- the map will be total as every loc is covered by the bands
  unsafeIndex $ Map.fromList do
    bigCol <- bands
    bigRow <- bands
    lilCol <- bands
    lilRow <- bands
    pure
      ( bandedGridLocation bigCol bigRow lilCol lilRow,
        bandedGridLocation
          (shuffleUniverse _bigColSeed bigCol)
          (shuffleUniverse _bigRowSeed bigRow)
          (shuffleUniverse _lilColSeed lilCol)
          (shuffleUniverse _lilRowSeed lilRow)
      )

shuffledGrid :: ShufflingSeeds Random.StdGen -> Grid -> Grid
shuffledGrid seeds initialGrid =
  initialGrid
    & traverse . onGiven %~ shuffleUniverse (_digitsSeed seeds)
    & Map.mapKeys (shuffleLoc seeds)

initSudokuState :: ShufflingSeeds Random.StdGen -> SudokuState
initSudokuState seeds =
  MkSudokuState
    { showHelp = True,
      grid = shuffledGrid seeds $ Map.fromList do
        (row, initRow) <- zip digits (initGrid [Input Nothing] \d -> [Given d])
        (col, initCell) <- zip digits initRow
        cell <- initCell
        [(Loc (Col col) (Row row), cell)],
      highs = initMarks,
      lows = initMarks,
      selected = Set.empty,
      focussed = Loc (Col D5) (Row D5),
      mode = Normal,
      match = Nothing,
      lastAction = Nothing
    }

main :: IO ()
main = do
  seed <-
    Env.lookupEnv "SEED" >>= \case
      Just v -> pure $ Random.mkStdGen (read v)
      Nothing -> Random.initStdGen
  let seeds = shufflingSeeds seed
  let builder = Vty.mkVty Vty.defaultConfig
  initialVty <- builder
  _ <- Brick.customMain initialVty builder Nothing app (initSudokuState seeds)
  pure ()
