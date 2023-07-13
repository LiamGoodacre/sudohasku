module Menu.Domain
  ( MenuItem (..),
    menuItems,
    Menu (..),
  )
where

data MenuItem
  = Play
  | End
  deriving stock (Show)
  deriving stock (Eq, Ord)

menuItems :: [MenuItem]
menuItems =
  [ Play,
    End
  ]

data Menu = Menu
  { menuTodo :: ()
  }
