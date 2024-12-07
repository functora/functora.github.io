{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.TestEnv
  ( eraseFirst,
    testAdaAmt,
    itRight,
    itLeft,
  )
where

import Bfx.Import
import qualified Test.Hspec as HS

eraseFirst :: (Bifunctor f) => f a b -> f () b
eraseFirst =
  first $ const ()

testAdaAmt :: MoneyAmount
testAdaAmt = MoneyAmount 4.004004

itRight ::
  ( Show a
  ) =>
  String ->
  (Env -> ExceptT Error IO a) ->
  HS.SpecWith (HS.Arg (Env -> IO ()))
itRight label test =
  HS.it label $ \env -> do
    x <- runExceptT $ test env
    x `HS.shouldSatisfy` isRight

itLeft ::
  ( Show a
  ) =>
  String ->
  (Env -> ExceptT Error IO a) ->
  HS.SpecWith (HS.Arg (Env -> IO ()))
itLeft label test =
  HS.it label $ \env -> do
    x <- runExceptT $ test env
    x `HS.shouldSatisfy` isLeft
