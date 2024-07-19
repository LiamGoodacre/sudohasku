module VtyOptic where

import Control.Lens
import Graphics.Vty qualified as Vty

_SetTo ::
  Applicative f =>
  (a -> f b) ->
  Vty.MaybeDefault a ->
  f (Vty.MaybeDefault b)
_SetTo ab = \case
  Vty.Default -> pure Vty.Default
  Vty.KeepCurrent -> pure Vty.KeepCurrent
  Vty.SetTo a -> Vty.SetTo <$> ab a

_attrBackColor ::
  Applicative f =>
  (Vty.MaybeDefault Vty.Color -> f (Vty.MaybeDefault Vty.Color)) ->
  Vty.Attr ->
  f Vty.Attr
_attrBackColor onVal attr =
  onVal (Vty.attrBackColor attr)
    <&> \bc -> attr {Vty.attrBackColor = bc}

_attrForeColor ::
  Applicative f =>
  (Vty.MaybeDefault Vty.Color -> f (Vty.MaybeDefault Vty.Color)) ->
  Vty.Attr ->
  f Vty.Attr
_attrForeColor onVal attr =
  onVal (Vty.attrForeColor attr)
    <&> \bc -> attr {Vty.attrForeColor = bc}
