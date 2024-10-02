module Functora.Miso.Widgets.Icon
  ( Icon (..),
    Fa,
  )
where

import Functora.Miso.Prelude
import Text.Casing (kebab)

class Icon a where
  icon :: a -> View action
  menu :: a
  fav :: a
  print :: a
  download :: a
  share :: a

data Fa
  = FaBars
  | FaHeart
  | FaPrint
  | FaDownload
  | FaShareNodes
  deriving stock (Eq, Ord, Show, Read, Data, Generic, Enum, Bounded)

instance Icon Fa where
  icon x =
    i_
      [ class_ "fa",
        class_ "fa-solid",
        class_ . from @String @Unicode . kebab $ inspect @String x
      ]
      mempty
  menu = FaBars
  fav = FaHeart
  print = FaPrint
  download = FaDownload
  share = FaShareNodes
