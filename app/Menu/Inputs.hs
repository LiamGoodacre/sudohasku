module Menu.Inputs
  ( menuInputs,
  )
where

import Brick qualified
import Control.Lens ((%~))
import Data.Function ((&))
import Graphics.Vty qualified as Vty
import Menu.Domain

menuInputs :: Brick.BrickEvent names e -> Brick.EventM names Menu ()
menuInputs = \case
  Brick.VtyEvent vtyEvent -> case vtyEvent of
    Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> Brick.halt
    Vty.EvKey (Vty.KChar 'j') _ ->
      Brick.modify \st ->
        st & onMenuItemActive %~ nextMenuItem
    Vty.EvKey (Vty.KChar 'k') _ ->
      Brick.modify \st ->
        st & onMenuItemActive %~ prevMenuItem
    Vty.EvKey Vty.KEnter _ -> pure ()
    _ -> pure ()
  _ -> pure ()
