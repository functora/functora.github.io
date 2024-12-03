{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Bfx.Orphan () where

import Bfx.Data.Kind
import Bfx.Import.External

mkFgpt @Method

deriving newtype instance HasCodec CurrencyCode

deriving newtype instance HasItemCodec CurrencyCode
