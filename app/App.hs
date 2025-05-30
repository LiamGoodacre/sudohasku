module App
  ( app,
  )
where

import Brick qualified
import Control.Monad.IO.Class qualified as MonadIO
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Domain
import Download qualified
import Drawing qualified
import Game.Inputs
import Menu.Inputs
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

generateGrid :: IO Grid
generateGrid = do
  seed <-
    Env.lookupEnv "SEED" >>= \case
      Just v -> pure $ Random.mkStdGen (read v)
      Nothing -> Random.initStdGen
  let seeds = Shuffling.shufflingSeeds seed
  rawGrid <- Download.downloadAPuzzle <&> fromMaybe fallbackGrid
  pure $ Shuffling.shuffledGrid seeds rawGrid

appHandleEvent :: Brick.BrickEvent names e -> Brick.EventM names AppState ()
appHandleEvent brickEvent =
  Brick.get >>= \case
    AppStateMenu m -> do
      (m', result) <- Brick.nestEventM m (menuInputs brickEvent)
      case result of
        Menuing -> Brick.put $ AppStateMenu m'
        StartPlaying -> do
          grid <- MonadIO.liftIO generateGrid
          Brick.put $ AppStateGame (initGame grid)
    AppStateGame g -> do
      (g', result) <- Brick.nestEventM g (gameInputs brickEvent)
      case result of
        Playing -> Brick.put $ AppStateGame g'
        Exited -> Brick.put initAppState

app :: Brick.App AppState e Void
app =
  Brick.App
    { Brick.appDraw = Drawing.appDraw,
      Brick.appChooseCursor = \_ _ -> Nothing,
      Brick.appHandleEvent = appHandleEvent,
      Brick.appStartEvent = pure (),
      Brick.appAttrMap = const Drawing.appAttrMap
    }
