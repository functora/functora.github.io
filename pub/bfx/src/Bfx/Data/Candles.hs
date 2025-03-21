{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.Candles
  ( Section (..),
    Request (..),
    Options (..),
    optsDef,
  )
where

import Bfx.Class.ToRequestParam
import Bfx.Data.Type
import qualified Data.Text as T
import Functora.Prelude
import qualified Prelude

data Section
  = Last
  | Hist
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

instance ToRequestParam Section where
  toTextParam =
    T.toLower
      . T.pack
      . Prelude.show

data Request = Request
  { timeFrame :: CandleTimeFrame,
    symbol :: CurrencyPair,
    section :: Section,
    options :: Options
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

data Options = Options
  { ascOrDesc :: Maybe AscOrDesc,
    limit :: Maybe Natural,
    start :: Maybe UTCTime,
    end :: Maybe UTCTime
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

optsDef :: Options
optsDef =
  Options
    { ascOrDesc = Nothing,
      limit = Just 10000,
      start = Nothing,
      end = Nothing
    }
