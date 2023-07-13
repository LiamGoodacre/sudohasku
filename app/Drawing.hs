module Drawing
  ( appDraw,
    appAttrMap,
  )
where

import Base.Drawing
import Brick qualified as Brick
import Data.Foldable (fold)
import Domain
import Game.Drawing
import Graphics.Vty qualified as Vty
import Menu.Drawing

appDraw :: AppState -> [Brick.Widget names]
appDraw = \case
  AppStateMenu menu ->
    menuDraw menu
  AppStateGame game ->
    gameDraw game

appAttrMap :: Brick.AttrMap
appAttrMap =
  Brick.attrMap Vty.defAttr $
    fold
      [ logoAttrMap,
        menuAttrMap,
        gameAttrMap
      ]
