module App
  ( app,
  )
where

import Brick qualified
import Data.Void (Void)
import Domain
import Drawing qualified
import Game.Inputs
import Menu.Inputs

appHandleEvent :: Brick.BrickEvent names e -> Brick.EventM names AppState ()
appHandleEvent brickEvent =
  Brick.get >>= \case
    AppStateMenu m -> do
      m' <- Brick.nestEventM' m (menuInputs brickEvent)
      Brick.put $ AppStateMenu m'
    AppStateGame g -> do
      g' <- Brick.nestEventM' g (gameInputs brickEvent)
      Brick.put $ AppStateGame g'

app :: Brick.App AppState e Void
app =
  Brick.App
    { Brick.appDraw = Drawing.appDraw,
      Brick.appChooseCursor = \_ _ -> Nothing,
      Brick.appHandleEvent = appHandleEvent,
      Brick.appStartEvent = pure (),
      Brick.appAttrMap = const Drawing.appAttrMap
    }
