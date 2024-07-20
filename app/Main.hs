module Main
  ( main,
  )
where

import App qualified
import Brick qualified as Brick
import Domain

main :: IO ()
main = do
  _ <-
    Brick.defaultMain
      App.app
      initAppState
  pure ()
