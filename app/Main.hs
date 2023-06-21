module Main
  ( main,
  )
where

import Brick qualified as Brick
import Control.Lens ((%~), (.~))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Void (Void)
import Domain
import Drawing qualified
import Graphics.Vty qualified as Vty
import Shuffling qualified
import System.Environment qualified as Env
import System.Random qualified as Random

act :: Action -> Brick.EventM names Game ()
act action =
  Brick.modify \st -> do
    let next =
          Domain.runAction action st
            & onLastAction .~ Just action

    if undoableAction action && sudoku next /= sudoku st
      then
        next
          & onFuture .~ []
          & onHistory %~ (sudoku st :)
      else next

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

appHandleEvent :: Brick.BrickEvent names e -> Brick.EventM names Game ()
appHandleEvent = \case
  Brick.VtyEvent vtyEvent -> case vtyEvent of
    Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> Brick.halt
    Vty.EvKey (Vty.KChar '?') _ -> act ToggleHelp
    Vty.EvKey Vty.KEsc _ -> act (SwitchMode Insert)
    Vty.EvKey (Vty.KChar '[') [Vty.MCtrl] -> act (SwitchMode Insert)
    Vty.EvKey (Vty.KChar 'i') _ -> act (SwitchMode Insert)
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
    Vty.EvKey (Vty.KChar 'u') _ -> act Undo
    Vty.EvKey (Vty.KChar 'r') _ -> act Redo
    Vty.EvKey (Vty.KChar c) _
      | Just d <- keyCharDigit c -> act (Enter d)
    _ -> pure ()
  _ -> pure ()

app :: Brick.App Game e Void
app =
  Brick.App
    { Brick.appDraw = Drawing.appDraw,
      Brick.appChooseCursor = \_ _ -> Nothing,
      Brick.appHandleEvent = appHandleEvent,
      Brick.appStartEvent = pure (),
      Brick.appAttrMap = const Drawing.appAttrMap
    }

initGrid :: r -> (Digit -> r) -> [[r]]
initGrid _____ is =
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

initMarks :: Marks
initMarks = Map.fromList do
  sequence (CellLoc (CellCol digits) (CellRow digits))
    <&> \loc -> (loc, Set.empty)

initGame :: Shuffling.ShufflingSeeds Random.StdGen -> Game
initGame seeds =
  MkGame
    { showHelp = True,
      selected = Set.empty,
      focussed = CellLoc (CellCol D5) (CellRow D5),
      mode = Insert,
      match = Nothing,
      lastAction = Nothing,
      sudoku =
        MkSudoku
          { grid = Shuffling.shuffledGrid seeds $ Map.fromList do
              (row, initRow) <- zip digits (initGrid [Input Nothing] \d -> [Given d])
              (col, initCell) <- zip digits initRow
              cell <- initCell
              [(CellLoc (CellCol col) (CellRow row), cell)],
            highs = initMarks,
            lows = initMarks
          },
      history = [],
      future = []
    }

main :: IO ()
main = do
  seed <-
    Env.lookupEnv "SEED" >>= \case
      Just v -> pure $ Random.mkStdGen (read v)
      Nothing -> Random.initStdGen
  let seeds = Shuffling.shufflingSeeds seed
  let builder = Vty.mkVty Vty.defaultConfig
  initialVty <- builder
  _ <- Brick.customMain initialVty builder Nothing app (initGame seeds)
  pure ()
