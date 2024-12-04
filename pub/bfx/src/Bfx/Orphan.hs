{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Bfx.Orphan () where

import Bfx.Data.Kind
import Bfx.Import.External

mkFgpt @Method
