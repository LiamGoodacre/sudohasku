module Menu.Inputs
  ( menuInputs,
  )
where

import Brick qualified
import Graphics.Vty qualified as Vty
import Menu.Domain

menuInputs :: Brick.BrickEvent names e -> Brick.EventM names Menu ()
menuInputs = \case
  Brick.VtyEvent vtyEvent -> case vtyEvent of
    Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> Brick.halt
    Vty.EvKey (Vty.KChar 'j') _ -> pure ()
    Vty.EvKey (Vty.KChar 'k') _ -> pure ()
    Vty.EvKey Vty.KEnter _ -> pure ()
    _ -> pure ()
  _ -> pure ()
