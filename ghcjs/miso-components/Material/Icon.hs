{-# LANGUAGE OverloadedStrings #-}

module Material.Icon (icon) where

import Material.Prelude as Prelude
import Miso.Html
import Miso.String

-- | Icon view function
icon :: [Attribute msg] -> String -> View msg
icon additionalAttributes iconName =
  i_
    (class_ "material-icons" : additionalAttributes)
    [text (toMisoString iconName)]
