module Domain
  ( module Game.Domain,
    module Menu.Domain,
    AppState (..),
    initAppState,
  )
where

import Game.Domain
import Menu.Domain

data AppState
  = AppStateMenu Menu
  | AppStateGame Game

initAppState :: AppState
initAppState = AppStateMenu Menu {menuTodo = ()}
