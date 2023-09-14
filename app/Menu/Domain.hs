module Menu.Domain
  ( MenuItem (..),
    menuItems,
    Menu (..),
    onMenuItemActive,
    nextMenuItem,
    prevMenuItem,
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

nextMenuItem :: MenuItem -> MenuItem
nextMenuItem = \case
  Play -> End
  End -> Play

prevMenuItem :: MenuItem -> MenuItem
prevMenuItem = \case
  Play -> End
  End -> Play

data Menu = Menu
  { menuItemActive :: MenuItem
  }

onMenuItemActive :: Functor f => (MenuItem -> f MenuItem) -> Menu -> f Menu
onMenuItemActive t m = (\v -> m {menuItemActive = v}) <$> t (menuItemActive m)
