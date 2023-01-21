{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Indicator.Tr
  ( Tr (..),
    tr,
  )
where

import BitfinexClient.Data.Metro
import BitfinexClient.Import
import qualified Data.Vector as V

newtype Tr = Tr
  { unTr :: QuotePerBase'
  }
  deriving newtype
    ( Eq,
      Ord
    )
  deriving stock
    ( Generic
    )

instance NFData Tr

tr :: NonEmpty Candle -> Vector (UTCTime, Tr)
tr cs =
  V.fromList $
    ( \(c0, c1) ->
        let ch = candleHigh c1
            cl = candleLow c1
            pc = candleClose c0
         in ( candleAt c1,
              Tr $
                maximum
                  [ absRange ch cl,
                    absRange ch pc,
                    absRange cl pc
                  ]
            )
    )
      <$> zip (toList cs) (tail cs)

absRange ::
  QuotePerBase 'Buy ->
  QuotePerBase 'Buy ->
  QuotePerBase'
absRange x y =
  mkQuotePerBase' . abs $
    abs (from x)
      - abs (from y)
