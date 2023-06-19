module Domain where

import Control.Lens (Lens', (%~), (.~), (^?))
import Control.Lens qualified as Lens
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Set qualified as Set

class Universe t where
  -- all possible inhabitants of `t`
  universe :: [t]

instance Universe Bool where
  universe = [False, True]

--

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

data PathPos t = PathPos
  { posBigCol :: t,
    posBigRow :: t,
    posLilCol :: t,
    posLilRow :: t
  }
  deriving stock (Functor, Foldable, Traversable)

instance Applicative PathPos where
  pure x = PathPos x x x x
  l <*> r =
    PathPos
      { posBigCol = posBigCol l (posBigCol r),
        posBigRow = posBigRow l (posBigRow r),
        posLilCol = posLilCol l (posLilCol r),
        posLilRow = posLilRow l (posLilRow r)
      }

--

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

--

newtype CellCol v = CellCol v
  deriving newtype (Show, Eq, Ord, Bounded)
  deriving stock (Functor, Foldable, Traversable)

newtype CellRow v = CellRow v
  deriving newtype (Show, Eq, Ord, Bounded)
  deriving stock (Functor, Foldable, Traversable)

data CellLoc v = CellLoc (CellCol v) (CellRow v)
  deriving stock (Show, Eq, Ord, Bounded)
  deriving stock (Functor, Foldable, Traversable)

data Cell = Given Digit | Input (Maybe Digit)
  deriving stock (Show, Eq, Ord)

type Grid = Map (CellLoc Digit) Cell

type Marks = Map (CellLoc Digit) (Set Digit)

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
    selected :: Set (CellLoc Digit),
    focussed :: CellLoc Digit,
    mode :: Mode,
    match :: Maybe Digit,
    lastAction :: Maybe Action
  }
  deriving stock (Show, Eq, Ord)

--

onCellLocCol :: Lens' (CellLoc v) v
onCellLocCol t (CellLoc (CellCol c) r) = t c <&> \v -> CellLoc (CellCol v) r

onCellLocRow :: Lens' (CellLoc v) v
onCellLocRow t (CellLoc c (CellRow r)) = t r <&> \v -> CellLoc c (CellRow v)

onCellGiven :: Lens.Prism' Cell Digit
onCellGiven = Lens.prism' Given \case
  Given v -> Just v
  _ -> Nothing

onCellInput :: Lens.Prism' Cell (Maybe Digit)
onCellInput = Lens.prism' Input \case
  Input v -> Just v
  _ -> Nothing

onShowHelp :: Lens' SudokuState Bool
onShowHelp t s = t (showHelp s) <&> \v -> s {showHelp = v}

onGrid :: Lens' SudokuState Grid
onGrid t s = t (grid s) <&> \v -> s {grid = v}

onFocus :: Lens' SudokuState (CellLoc Digit)
onFocus t s = t (focussed s) <&> \v -> s {focussed = v}

onSelected :: Lens' SudokuState (Set (CellLoc Digit))
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

onMarks :: HighsOrLows -> Set (CellLoc Digit) -> Lens.Traversal' SudokuState (Set Digit)
onMarks highLow targets =
  onHighLows highLow
    . Lens.itraversed
    . Lens.indices (`Set.member` targets)

--

bandedGridCellLocation :: Band -> Band -> Band -> Band -> CellLoc Digit
bandedGridCellLocation bigCol bigRow lilCol lilRow = do
  let col = CellCol $ digitFromInt $ ((bandToInt bigCol - 1) * 3) + bandToInt lilCol
  let row = CellRow $ digitFromInt $ ((bandToInt bigRow - 1) * 3) + bandToInt lilRow
  CellLoc col row

boxToCenterMidCellLoc :: Digit -> CellLoc Digit
boxToCenterMidCellLoc (digitToInt -> d) = do
  let (bigRow, bigCol) = divMod (d - 1) 3
  bandedGridCellLocation
    (bandFromInt (bigCol + 1))
    (bandFromInt (bigRow + 1))
    B2
    B2

--

shiftFocus :: Direction -> SudokuState -> SudokuState
shiftFocus = \case
  North -> onFocus . onCellLocRow %~ prevDigit
  East -> onFocus . onCellLocCol %~ nextDigit
  South -> onFocus . onCellLocRow %~ nextDigit
  West -> onFocus . onCellLocCol %~ prevDigit

jumpFocus :: Digit -> SudokuState -> SudokuState
jumpFocus d = onFocus .~ boxToCenterMidCellLoc d

updateSelection :: Selection -> SudokuState -> SudokuState
updateSelection = \case
  Expand -> \st -> st & onSelected %~ Set.insert (focussed st)
  Reset -> onSelected .~ Set.empty
  Ignore -> id

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
          . onCellInput

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
          . onCellInput

  st & cellDigit .~ Nothing

removeMarks :: HighsOrLows -> SudokuState -> SudokuState
removeMarks highLow st = do
  let targets = Set.insert (focussed st) (selected st)

  let cellMark :: Lens.Traversal' SudokuState (Set Digit)
      cellMark = onMarks highLow targets

  st & cellMark .~ Set.empty

matchFocus :: SudokuState -> SudokuState
matchFocus st = do
  let maybeDigit = do
        cell <- st ^? onGrid . Lens.itraversed . Lens.index (focussed st)
        case cell of
          Given d -> Just d
          Input i -> i

  st & updateMatch maybeDigit

runAction :: Action -> SudokuState -> SudokuState
runAction = \case
  ToggleHelp -> onShowHelp %~ not
  ClearSelect -> onSelected .~ Set.empty
  ToggleSelect -> \st -> st & onSelected . Lens.contains (focussed st) %~ not
  SwitchMode m -> \st ->
    st
      & onMode .~ m
      & case (m, mode st) of
        (Highlight, Highlight) -> matchFocus
        _ -> id
  Move selection direction ->
    shiftFocus direction . updateSelection selection
  Enter digit -> \st ->
    st & case mode st of
      Normal -> enter digit
      Mark highLow -> updateMarks highLow digit
      Highlight -> updateMatch (Just digit)
      Jump -> jumpFocus digit
  Remove -> \st ->
    st & case mode st of
      Normal -> removeNormal
      Mark highLow -> removeMarks highLow
      Highlight -> id
      Jump -> id
  NoHighlight -> updateMatch Nothing
