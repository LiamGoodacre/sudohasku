module Main
  ( main,
  )
where

import App qualified
import Brick qualified as Brick
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Domain
import Download qualified
import Graphics.Vty qualified as Vty
import Shuffling qualified
import System.Environment qualified as Env
import System.Random qualified as Random

makeFallbackTable :: r -> (Digit -> r) -> [[r]]
makeFallbackTable _____ is =
  [ [is D8, _____, _____, _____, _____, _____, is D7, is D9, _____],
    [_____, is D1, _____, _____, _____, _____, _____, _____, is D2],
    [is D7, is D4, _____, _____, is D2, _____, _____, _____, is D5],
    [is D2, is D9, _____, _____, is D6, is D8, _____, is D3, _____],
    [_____, _____, _____, _____, is D7, _____, _____, _____, _____],
    [_____, is D3, _____, is D2, is D9, _____, _____, is D4, is D8],
    [is D6, _____, _____, _____, is D3, _____, _____, is D7, is D1],
    [is D9, _____, _____, _____, _____, _____, _____, is D2, _____],
    [_____, is D7, is D8, _____, _____, _____, _____, _____, is D6]
  ]

fallbackGrid :: Grid
fallbackGrid = Map.fromList do
  let fallbackTable :: [[[Cell]]]
      fallbackTable = makeFallbackTable [Input Nothing] \d -> [Given d]
  (row, initRow) <- zip digits fallbackTable
  (col, initCell) <- zip digits initRow
  cell <- initCell
  [(CellLoc (CellCol col) (CellRow row), cell)]

main :: IO ()
main = do
  seed <-
    Env.lookupEnv "SEED" >>= \case
      Just v -> pure $ Random.mkStdGen (read v)
      Nothing -> Random.initStdGen
  let seeds = Shuffling.shufflingSeeds seed

  someGrid <- do
    rawGrid <- Download.downloadAPuzzle <&> fromMaybe fallbackGrid
    pure $ Shuffling.shuffledGrid seeds rawGrid

  let builder = Vty.mkVty Vty.defaultConfig
  initialVty <- builder
  _ <-
    Brick.customMain
      initialVty
      builder
      Nothing
      App.app
      (initGame someGrid)
  pure ()
