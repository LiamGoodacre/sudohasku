module Domain
  ( Action (..),
    Band (..),
    Cell (..),
    CellCol (..),
    CellLoc (..),
    CellRow (..),
    Digit (..),
    Direction (..),
    Grid,
    HighsOrLows (..),
    Marks,
    Mode (..),
    Selection (..),
    Sudoku (..),
    Game (..),
    Universe (..),
    bandedGridCellLocation,
    bands,
    digitToInt,
    digits,
    onCellGiven,
    onLastAction,
    onHistory,
    onFuture,
    undoableAction,
    runAction,
    toBox,
  )
where

import Control.Lens (Lens', Traversal', (%~), (.~), (^?))
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

onCellLocCol :: Lens' (CellLoc v) v
onCellLocCol t (CellLoc (CellCol c) r) = t c <&> \v -> CellLoc (CellCol v) r

onCellLocRow :: Lens' (CellLoc v) v
onCellLocRow t (CellLoc c (CellRow r)) = t r <&> \v -> CellLoc c (CellRow v)

data Cell = Given Digit | Input (Maybe Digit)
  deriving stock (Show, Eq, Ord)

onCellGiven :: Lens.Prism' Cell Digit
onCellGiven = Lens.prism' Given \case
  Given v -> Just v
  _ -> Nothing

onCellInput :: Lens.Prism' Cell (Maybe Digit)
onCellInput = Lens.prism' Input \case
  Input v -> Just v
  _ -> Nothing

type Grid = Map (CellLoc Digit) Cell

type Marks = Map (CellLoc Digit) (Set Digit)

data HighsOrLows = Highs | Lows
  deriving stock (Show, Eq, Ord)

data Mode = Insert | Mark HighsOrLows | Highlight | Jump
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
  | Undo
  | Redo
  deriving stock (Show, Eq, Ord)

data Sudoku = MkSudoku
  { grid :: Grid,
    highs :: Marks,
    lows :: Marks
  }
  deriving stock (Show, Eq, Ord)

onGrid :: Lens' Sudoku Grid
onGrid t s = t (grid s) <&> \v -> s {grid = v}

onHighs :: Lens' Sudoku Marks
onHighs t s = t (highs s) <&> \v -> s {highs = v}

onLows :: Lens' Sudoku Marks
onLows t s = t (lows s) <&> \v -> s {lows = v}

onHighsOrLows :: HighsOrLows -> Lens' Sudoku Marks
onHighsOrLows = \case
  Highs -> onHighs
  Lows -> onLows

onHighsAndLows :: Traversal' Sudoku Marks
onHighsAndLows t s =
  (\h l -> s {highs = h, lows = l})
    <$> t (highs s)
    <*> t (lows s)

onMarks :: Set (CellLoc Digit) -> Traversal' Marks (Set Digit)
onMarks targets =
  Lens.itraversed . Lens.indices (`Set.member` targets)

data Game = MkGame
  { showHelp :: Bool,
    mode :: Mode,
    focussed :: CellLoc Digit,
    selected :: Set (CellLoc Digit),
    match :: Maybe Digit,
    sudoku :: Sudoku,
    lastAction :: Maybe Action,
    history :: [Sudoku],
    future :: [Sudoku]
  }
  deriving stock (Show, Eq, Ord)

onShowHelp :: Lens' Game Bool
onShowHelp t s = t (showHelp s) <&> \v -> s {showHelp = v}

onMode :: Lens' Game Mode
onMode t s = t (mode s) <&> \v -> s {mode = v}

onFocussed :: Lens' Game (CellLoc Digit)
onFocussed t s = t (focussed s) <&> \v -> s {focussed = v}

onSelected :: Lens' Game (Set (CellLoc Digit))
onSelected t s = t (selected s) <&> \v -> s {selected = v}

onMatch :: Lens' Game (Maybe Digit)
onMatch t s = t (match s) <&> \v -> s {match = v}

onSudoku :: Lens' Game Sudoku
onSudoku t s = t (sudoku s) <&> \v -> s {sudoku = v}

onLastAction :: Lens' Game (Maybe Action)
onLastAction t s = t (lastAction s) <&> \v -> s {lastAction = v}

onHistory :: Lens' Game [Sudoku]
onHistory t s = t (history s) <&> \v -> s {history = v}

onFuture :: Lens' Game [Sudoku]
onFuture t s = t (future s) <&> \v -> s {future = v}

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

shiftFocus :: Direction -> Game -> Game
shiftFocus = \case
  North -> onFocussed . onCellLocRow %~ prevDigit
  East -> onFocussed . onCellLocCol %~ nextDigit
  South -> onFocussed . onCellLocRow %~ nextDigit
  West -> onFocussed . onCellLocCol %~ prevDigit

jumpFocus :: Digit -> Game -> Game
jumpFocus d = onFocussed .~ boxToCenterMidCellLoc d

updateSelection :: Selection -> Game -> Game
updateSelection = \case
  Expand -> \st -> st & onSelected %~ Set.insert (focussed st)
  Reset -> onSelected .~ Set.empty
  Ignore -> id

updateMarks :: HighsOrLows -> Digit -> Game -> Game
updateMarks highLow digit st = do
  let targets = Set.insert (focussed st) (selected st)

  let cellDigitMark :: Traversal' Game Bool
      cellDigitMark = onSudoku . onHighsOrLows highLow . onMarks targets . Lens.contains digit

  st & cellDigitMark .~ not (st & Lens.andOf cellDigitMark)

bandBox :: Band -> Band -> Set (CellLoc Digit)
bandBox =
  bands & foldMap \lilCol ->
    bands & foldMap \lilRow ->
      \bigCol bigRow ->
        Set.singleton do
          bandedGridCellLocation
            bigCol
            bigRow
            lilCol
            lilRow

toBox :: CellLoc Digit -> Set (CellLoc Digit)
toBox (CellLoc (CellCol col) (CellRow row)) = do
  let bigCol = div (digitToInt col - 1) 3 + 1
  let bigRow = div (digitToInt row - 1) 3 + 1
  bandBox
    (bandFromInt bigCol)
    (bandFromInt bigRow)

toRow :: CellLoc Digit -> Set (CellLoc Digit)
toRow (CellLoc _ row) =
  Set.fromList $ digits <&> \col -> CellLoc (CellCol col) row

toCol :: CellLoc Digit -> Set (CellLoc Digit)
toCol (CellLoc col _) =
  Set.fromList $ digits <&> \row -> CellLoc col (CellRow row)

enter :: Digit -> Game -> Game
enter digit st = do
  let enterTargets = Set.insert (focussed st) (selected st)
  let unmarkTargets = enterTargets & foldMap (toBox <> toRow <> toCol)

  let cellDigit :: Traversal' Game (Maybe Digit)
      cellDigit =
        onSudoku
          . onGrid
          . Lens.itraversed
          . Lens.indices (`Set.member` enterTargets)
          . onCellInput

  let cellMarks :: Traversal' Game Bool
      cellMarks =
        onSudoku
          . onHighsAndLows
          . onMarks unmarkTargets
          . Lens.contains digit

  let g = st & Lens.allOf cellDigit (== Just digit)

  st
    & cellDigit .~ (if g then Nothing else Just digit)
    & cellMarks %~ (if g then id else const False)

updateMatch :: Maybe Digit -> Game -> Game
updateMatch digit st =
  st & onMatch .~ digit

removeInsert :: Game -> Game
removeInsert st = do
  let targets = Set.insert (focussed st) (selected st)

  let cellDigit :: Traversal' Game (Maybe Digit)
      cellDigit =
        onSudoku
          . onGrid
          . Lens.itraversed
          . Lens.indices (`Set.member` targets)
          . onCellInput

  st & cellDigit .~ Nothing

removeMarks :: HighsOrLows -> Game -> Game
removeMarks highLow st = do
  let targets = Set.insert (focussed st) (selected st)

  let cellMark :: Traversal' Game (Set Digit)
      cellMark = onSudoku . onHighsOrLows highLow . onMarks targets

  st & cellMark .~ Set.empty

matchFocus :: Game -> Game
matchFocus st = do
  let maybeDigit = do
        cell <- st ^? onSudoku . onGrid . Lens.itraversed . Lens.index (focussed st)
        case cell of
          Given d -> Just d
          Input i -> i

  st & updateMatch maybeDigit

undoableAction :: Action -> Bool
undoableAction = \case
  Enter {} -> True
  Remove -> True
  _ -> False

runAction :: Action -> Game -> Game
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
      Insert -> enter digit
      Mark highLow -> updateMarks highLow digit
      Highlight -> updateMatch (Just digit)
      Jump -> jumpFocus digit
  Remove -> \st ->
    st & case mode st of
      Insert -> removeInsert
      Mark highLow -> removeMarks highLow
      Highlight -> id
      Jump -> id
  NoHighlight -> updateMatch Nothing
  Undo ->
    \game ->
      case history game of
        [] -> game
        (prev : hist) ->
          game
            & onHistory .~ hist
            & onFuture %~ (sudoku game :)
            & onSudoku .~ prev
  Redo ->
    \game ->
      case future game of
        [] -> game
        (next : futu) ->
          game
            & onFuture .~ futu
            & onHistory %~ (sudoku game :)
            & onSudoku .~ next
