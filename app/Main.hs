module Main
  ( main,
  )
where

import App qualified
import Brick qualified
import Domain

main :: IO ()
main = do
  _ <-
    Brick.defaultMain
      App.app
      initAppState
  pure ()
