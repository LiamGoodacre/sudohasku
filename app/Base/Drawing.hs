module Base.Drawing
  ( darken,
    brighten,
    Theme(..),
    theme,
    logoDraw,
    logoAttrMap,
  )
where

import Brick qualified as Brick
import Data.Word (Word8)
import Graphics.Vty qualified as Vty

rgb :: Word8 -> Word8 -> Word8 -> Vty.Color
rgb = Vty.rgbColor

mapRGB :: (forall i . Integral i => i -> Word8) -> Vty.Color -> Vty.Color
mapRGB onComponent col@(Vty.Color240 c240) =
  case (Vty.color240CodeToRGB c240) of
    Nothing -> col
    Just (r, g, b) ->
      rgb
        (onComponent r)
        (onComponent g)
        (onComponent b)
mapRGB onComponent (Vty.RGBColor r g b) =
  rgb
    (onComponent r)
    (onComponent g)
    (onComponent b)
mapRGB _onComponent (Vty.ISOColor n) =
  Vty.ISOColor (mod n 8 + 8)

darkenRGBComponent :: Integral i => i -> Word8
darkenRGBComponent w =
  fromInteger $ (fromIntegral w * 10) `div` 12

brightenRGBComponent :: Integral i => i -> Word8
brightenRGBComponent w =
  fromInteger $ min 255 $ (fromIntegral w * 12) `div` 10

darken :: Vty.Color -> Vty.Color
darken = mapRGB darkenRGBComponent

brighten :: Vty.Color -> Vty.Color
brighten = mapRGB brightenRGBComponent

paletteBlack :: Vty.Color
paletteBlack = rgb 18 20 32

paletteWhite :: Vty.Color
paletteWhite = rgb 203 205 202

paletteBlue :: Vty.Color
paletteBlue = rgb 35 87 137

paletteRed :: Vty.Color
paletteRed = rgb 255 60 56

paletteGreen :: Vty.Color
paletteGreen = rgb 137 252 0

paletteTeal :: Vty.Color
paletteTeal = rgb 0 175 181

paletteOrange :: Vty.Color
paletteOrange = rgb 255 119 0

palettePink :: Vty.Color
palettePink = rgb 220 0 115

paletteTODO :: Vty.Color
paletteTODO = rgb 255 100 200

data Theme a = Theme
  { screenBG :: !a,
    barBG :: !a,
    barFG :: !a,
    modeNormal :: !a,
    modeHighMark :: !a,
    modeLowMark :: !a,
    modeHighlight :: !a,
    modeJump :: !a,
    cellBG :: !a,
    cellMatchBG :: !a,
    cellSelectBG :: !a,
    cellGivenFG :: !a,
    cellInputFG :: !a
  }

theme :: Theme Vty.Color
theme =
  Theme
    { screenBG = paletteBlack,
      barBG = paletteBlue,
      barFG = paletteWhite,
      modeNormal = paletteRed,
      modeHighMark = paletteOrange,
      modeLowMark = palettePink,
      modeHighlight = darken paletteGreen,
      modeJump = paletteTODO,
      cellBG = paletteWhite,
      cellMatchBG = paletteGreen,
      cellSelectBG = paletteTeal,
      cellGivenFG = paletteBlack,
      cellInputFG = paletteBlue
    }

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

logoAttrMap :: [(Brick.AttrName, Vty.Attr)]
logoAttrMap =
  [ ( Brick.attrName "logo",
      Vty.defAttr
        `Vty.withForeColor` theme.barFG
        `Vty.withBackColor` theme.barBG
    )
  ]
