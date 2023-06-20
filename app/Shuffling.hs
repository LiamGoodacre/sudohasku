module Shuffling
  ( ShufflingSeeds (..),
    shuffledGrid,
    shufflingSeeds,
  )
where

import Control.Lens ((%~))
import Control.Monad.State (evalState, state)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Domain
import System.Random qualified as Random
import System.Random.Shuffle qualified as Shuffle

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

data ShufflingSeeds a = ShufflingSeeds
  { digitsSeed :: a,
    bigColSeed :: a,
    bigRowSeed :: a,
    lilColSeed :: a,
    lilRowSeed :: a
  }
  deriving stock (Functor, Foldable, Traversable)

instance Applicative ShufflingSeeds where
  pure x = ShufflingSeeds x x x x x
  l <*> r =
    ShufflingSeeds
      { digitsSeed = digitsSeed l (digitsSeed r),
        bigColSeed = bigColSeed l (bigColSeed r),
        bigRowSeed = bigRowSeed l (bigRowSeed r),
        lilColSeed = lilColSeed l (lilColSeed r),
        lilRowSeed = lilRowSeed l (lilRowSeed r)
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

shuffleCellLoc :: ShufflingSeeds Random.StdGen -> CellLoc Digit -> CellLoc Digit
shuffleCellLoc ShufflingSeeds {..} = do
  let unsafeIndex = (Map.!)
  -- the map will be total as every loc is covered by the bands
  unsafeIndex $ Map.fromList do
    sequence (pure bands) <&> \PathPos {..} ->
      ( bandedGridCellLocation posBigCol posBigRow posLilCol posLilRow,
        bandedGridCellLocation
          (shuffleUniverse bigColSeed posBigCol)
          (shuffleUniverse bigRowSeed posBigRow)
          (shuffleUniverse lilColSeed posLilCol)
          (shuffleUniverse lilRowSeed posLilRow)
      )

shuffledGrid :: ShufflingSeeds Random.StdGen -> Grid -> Grid
shuffledGrid seeds initialGrid =
  initialGrid
    & traverse . onCellGiven %~ shuffleUniverse (digitsSeed seeds)
    & Map.mapKeys (shuffleCellLoc seeds)
