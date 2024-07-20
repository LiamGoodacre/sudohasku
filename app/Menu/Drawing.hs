module Menu.Drawing
  ( menuDraw,
    menuAttrMap,
  )
where

import Base.Drawing qualified as Theme
import Base.Drawing (theme)
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

renderMenuItem :: (Char -> Char) -> MenuItem -> Brick.Widget names
renderMenuItem f = \case
  Play ->
    Brick.vBox $
      map
        (Brick.str . map f)
        [ " ┌┐┐     ",
          " ├┘│┌┐┐┌ ",
          " ┘ └└┴└┤ ",
          " └─────┘ "
        ]
  End ->
    Brick.vBox $
      map
        (Brick.str . map f)
        [ " ┌┐   ┐  ",
          " ├ ┌┐┌┤  ",
          " └┘┘└└┴. "
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
                            $ renderMenuItem Theme.thickenChar item
                    else Brick.withDefAttr (Brick.attrName "menu-item-inactive")
                            $ renderMenuItem id item
                )
        ]
  ]

menuAttrMap :: [(Brick.AttrName, Vty.Attr)]
menuAttrMap =
  [ ( Brick.attrName "menu-item-active",
      Vty.defAttr
        `Vty.withForeColor` theme.cellGivenFG
        `Vty.withBackColor` Theme.brighten theme.cellSelectBG
    ),
    ( Brick.attrName "menu-item-inactive",
      Vty.defAttr
        `Vty.withForeColor` theme.cellGivenFG
        `Vty.withBackColor` theme.cellBG
    )
  ]
