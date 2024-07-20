module Menu.Inputs
  ( MenuOutput (..),
    menuInputs,
  )
where

import Brick qualified
import Control.Lens ((%~), use)
import Data.Function ((&))
import Graphics.Vty qualified as Vty
import Menu.Domain

data MenuOutput = Menuing | StartPlaying

menuInputChooseItem :: Brick.EventM names Menu MenuOutput
menuInputChooseItem = do
  choice <- use onMenuItemActive
  case choice of
    Play -> pure StartPlaying
    End -> do
      Brick.halt
      pure Menuing

menuInputs :: Brick.BrickEvent names e -> Brick.EventM names Menu MenuOutput
menuInputs = \case
  Brick.VtyEvent vtyEvent -> case vtyEvent of
    Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> do
      Brick.halt
      pure Menuing
    Vty.EvKey (Vty.KChar 'j') _ -> do
      Brick.modify \st ->
        st & onMenuItemActive %~ nextMenuItem
      pure Menuing
    Vty.EvKey (Vty.KChar 'k') _ -> do
      Brick.modify \st ->
        st & onMenuItemActive %~ prevMenuItem
      pure Menuing
    Vty.EvKey Vty.KEnter _ -> menuInputChooseItem
    Vty.EvKey (Vty.KChar ' ') _ -> menuInputChooseItem
    _ -> pure Menuing
  _ -> pure Menuing
