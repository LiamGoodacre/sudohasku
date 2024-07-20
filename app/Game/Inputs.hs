module Game.Inputs
  ( GameOutput (..),
    gameInputs,
  )
where

import Brick qualified
import Control.Lens ((%~), (.~))
import Data.Function ((&))
import Game.Domain
import Graphics.Vty qualified as Vty

data GameOutput = Playing | Exited

act :: Action -> Brick.EventM names Game GameOutput
act action = do
  Brick.modify \st -> do
    let next =
          runAction action st
            & onLastAction .~ Just action

    if undoableAction action && sudoku next /= sudoku st
      then
        next
          & onFuture .~ []
          & onHistory %~ (sudoku st :)
      else next
  pure Playing

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

gameInputs :: Brick.BrickEvent names e -> Brick.EventM names Game GameOutput
gameInputs = \case
  Brick.VtyEvent vtyEvent -> case vtyEvent of
    Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> Brick.halt >> pure Exited
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
    Vty.EvKey (Vty.KChar 'q') _ -> pure Exited
    Vty.EvKey (Vty.KChar c) _
      | Just d <- keyCharDigit c -> act (Enter d)
    _ -> pure Playing
  _ -> pure Playing
