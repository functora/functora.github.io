module Functora.Miso.Widgets.Icon
  ( Icon (..),
    IsIcon (..),
    Fa,
  )
where

import Functora.Miso.Prelude
import Text.Casing (kebab)

data Icon
  = IconMenu
  | IconFav
  | IconPrint
  | IconDownload
  | IconShare
  | IconClose
  | IconGooglePlay
  | IconQr
  | IconDelivery
  | IconBack
  deriving stock (Eq, Ord, Show, Read, Data, Generic, Enum, Bounded)

class (From Icon a) => IsIcon a where
  icon :: Icon -> View action

data Fa
  = FaBars
  | FaHeart
  | FaPrint
  | FaDownload
  | FaShareNodes
  | FaXmark
  | FaGooglePlay
  | FaQrcode
  | FaTruck
  | FaArrowLeft
  deriving stock (Eq, Ord, Show, Read, Data, Generic, Enum, Bounded)

instance From Icon Fa where
  from = \case
    IconMenu -> FaBars
    IconFav -> FaHeart
    IconPrint -> FaPrint
    IconDownload -> FaDownload
    IconShare -> FaShareNodes
    IconClose -> FaXmark
    IconGooglePlay -> FaGooglePlay
    IconQr -> FaQrcode
    IconDelivery -> FaTruck
    IconBack -> FaArrowLeft

instance IsIcon Fa where
  icon x =
    i_
      [ class_
          $ case fa of
            FaGooglePlay -> "fa-brands"
            _ -> "fa-solid",
        class_
          . from @String @Unicode
          . kebab
          $ inspect @String fa
      ]
      mempty
    where
      fa = from @Icon @Fa x
