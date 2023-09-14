module Menu.Drawing
  ( menuDraw,
    menuAttrMap,
  )
where

import Brick qualified as Brick
import Brick.Widgets.Center qualified as Brick
import Data.Function ((&))
import Graphics.Vty qualified as Vty
import Menu.Domain

logoDraw :: Brick.Widget names
logoDraw =
  Brick.withDefAttr (Brick.attrName "logo") do
    Brick.vBox $
      map
        Brick.str
        [ " ┌─┐ ┐ ┌ ┬─┐ ┌─┐ ┐ ┌ ┌─┐ ┌─┐ ┐ ┐ ┐ ┌ ",
          " └─┐ │ │ │ │ │ │ ├─┤ ├─┤ └─┐ ├─┘ │ │ ",
          "   │ │ │ │ │ │ │ │ │ │ │   │ ├─┐ │ │ ",
          " └─┘ └─┘ ┴─┘ └─┘ ┘ └ ┘ └ └─┘ ┘ ┘ └─┘ "
        ]

renderMenuItem :: MenuItem -> Brick.Widget names
renderMenuItem = \case
  Play ->
    Brick.vBox $
      map
        Brick.str
        [ " ┌┐┐     ",
          " ├┘│┌┐┐┌ ",
          " ┘ └└┴└┤ ",
          " └─────┘ "
        ]
  End ->
    Brick.vBox $
      map
        Brick.str
        [ " ┌┐   ┐ ",
          " ├ ┌┐┌┤ ",
          " └┘┘└└┴ "
        ]

menuDraw :: Menu -> [Brick.Widget names]
menuDraw menu =
  [ Brick.center do
      Brick.vBox
        [ Brick.hCenter logoDraw,
          Brick.hCenter (Brick.str "MENU"),
          Brick.hCenter $
            Brick.vBox $
              menuItems & fmap \item ->
                ( if item == menuItemActive menu
                    then Brick.withDefAttr (Brick.attrName "menu-item-active")
                    else id
                )
                  $ renderMenuItem item
        ]
  ]

menuAttrMap :: [(Brick.AttrName, Vty.Attr)]
menuAttrMap =
  [ ( Brick.attrName "menu-item-active",
      Vty.defAttr
        `Vty.withForeColor` Vty.brightWhite
        `Vty.withBackColor` Vty.green
    )
  ]
