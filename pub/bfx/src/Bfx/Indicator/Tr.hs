{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Indicator.Tr
  ( Tr (..),
    mkTrConduit,
    mkTrs,
    mkTr,
  )
where

import Bfx.Data.Type
import qualified Conduit as C
import qualified Data.Vector as V
import Functora.Money
import Functora.Prelude

newtype Tr = Tr
  { unTr :: QuotePerBase
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

mkTrConduit ::
  ( Monad m
  ) =>
  (a -> Candle) ->
  C.ConduitT a (a, Tr) m ()
mkTrConduit mkCandle = do
  mc0 <- C.await
  mc1 <- C.await
  case (,) <$> mc0 <*> mc1 of
    Nothing -> pure ()
    Just (c0, c1) -> do
      C.yield . first (const c1) $ mkTr (mkCandle c0) (mkCandle c1)
      C.leftover c1
      mkTrConduit mkCandle

mkTrs :: NonEmpty Candle -> Vector (UTCTime, Tr)
mkTrs cs =
  V.fromList $ (uncurry mkTr) <$> zip (toList cs) (tail cs)

mkTr :: Candle -> Candle -> (UTCTime, Tr)
mkTr c0 c1 =
  ( candleAt c1,
    Tr
      $ maximum
        [ absRange ch cl,
          absRange ch pc,
          absRange cl pc
        ]
  )
  where
    ch = candleHigh c1
    cl = candleLow c1
    pc = candleClose c0

absRange :: QuotePerBase -> QuotePerBase -> QuotePerBase
absRange x y =
  QuotePerBase
    . unsafeFrom @Fix @FixNonNeg
    . abs
    $ abs (unFixNonNeg $ unQuotePerBase x)
    - abs (unFixNonNeg $ unQuotePerBase y)
