module Functora.Miso.Capa.Flex
  ( flex,
  )
where

import Functora.Miso.Prelude

flex :: [Attribute action] -> [View action] -> View action
flex attrs =
  div_ $ style_ [("display", "flex")] : attrs
