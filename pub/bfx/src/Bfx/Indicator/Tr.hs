{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Indicator.Tr
  ( Tr (..),
    tr,
  )
where

import Bfx.Data.Type
import qualified Data.Vector as V
import Functora.Money
import Functora.Prelude

newtype Tr = Tr
  { unTr :: QuotePerBase
  }
  deriving stock
    ( Eq,
      Ord,
      Data,
      Generic
    )

tr :: NonEmpty Candle -> Vector (UTCTime, Tr)
tr cs =
  V.fromList
    $ ( \(c0, c1) ->
          let ch = candleHigh c1
              cl = candleLow c1
              pc = candleClose c0
           in ( candleAt c1,
                Tr
                  $ maximum
                    [ absRange ch cl,
                      absRange ch pc,
                      absRange cl pc
                    ]
              )
      )
    <$> zip (toList cs) (tail cs)

absRange :: QuotePerBase -> QuotePerBase -> QuotePerBase
absRange x y =
  QuotePerBase
    . unsafeFrom @Rational @(Ratio Natural)
    . abs
    $ abs (from @(Ratio Natural) @Rational $ unQuotePerBase x)
    - abs (from @(Ratio Natural) @Rational $ unQuotePerBase y)
