{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.Miso.Orphan () where

import Data.JSString (JSString)
import qualified Data.JSString as JS
import qualified Data.JSString.Text as JST
import Data.Monoid.Factorial
import Data.Monoid.GCD
import Data.Monoid.Null
import Data.Monoid.Textual
import Data.Semigroup.Cancellative
import Functora.Prelude
import qualified Miso
import Prelude hiding (String)

deriving stock instance Generic (Miso.View action)

deriving stock instance Generic (Miso.Attribute action)

deriving stock instance Generic Miso.NS

deriving stock instance Generic Miso.Key

instance NFData (Miso.View action)

instance NFData (Miso.Attribute action)

instance NFData Miso.NS

instance NFData Miso.Key

instance MonoidNull JSString where
  null = JS.null

instance Factorial JSString where
  factors = JS.chunksOf 1
  primePrefix = JS.take 1
  primeSuffix x = if JS.null x then JS.empty else JS.singleton (JS.last x)
  foldl f = JS.foldl f'
    where
      f' a char = f a (JS.singleton char)
  foldl' f = JS.foldl' f'
    where
      f' a char = f a (JS.singleton char)
  foldr f = JS.foldr f'
    where
      f' char a = f (JS.singleton char) a
  length = JS.length
  reverse = JS.reverse

instance LeftGCDMonoid JSString where
  stripCommonPrefix x y = maybe (JS.empty, x, y) id (JS.commonPrefixes x y)

instance LeftReductive JSString where
  stripPrefix = JS.stripPrefix
  isPrefixOf = JS.isPrefixOf

instance TextualMonoid JSString where
  fromText = JST.textToJSString
  singleton = JS.singleton
  splitCharacterPrefix = JS.uncons
  characterPrefix t = if JS.null t then Nothing else Just (JS.head t)
  map = JS.map
  concatMap = JS.concatMap
  toString = const JS.unpack
  toText = const JST.textFromJSString
  any = JS.any
  all = JS.all

  foldl = const JS.foldl
  foldl' = const JS.foldl'
  foldr = const JS.foldr

  scanl = JS.scanl
  scanl1 = JS.scanl1
  scanr = JS.scanr
  scanr1 = JS.scanr1
  mapAccumL = JS.mapAccumL
  mapAccumR = JS.mapAccumR

  takeWhile _ = JS.takeWhile
  dropWhile _ = JS.dropWhile
  break _ = JS.break
  span _ = JS.span
  spanMaybe s0 _ft fc t = case JS.foldr g id t (0, s0) of
    (i, s') | (prefix, suffix) <- JS.splitAt i t -> (prefix, suffix, s')
    where
      g c cont (i, s)
        | Just s' <- fc s c = let i' = succ i :: Int in seq i' $ cont (i', s')
        | otherwise = (i, s)
  spanMaybe' s0 _ft fc t = case JS.foldr g id t (0, s0) of
    (i, s') | (prefix, suffix) <- JS.splitAt i t -> (prefix, suffix, s')
    where
      g c cont (i, s)
        | Just s' <- fc s c =
            let i' = succ i :: Int in seq i' $ seq s' $ cont (i', s')
        | otherwise = (i, s)
  split = JS.split
  find = JS.find

instance FactorialMonoid JSString where
  splitPrimePrefix = fmap (first JS.singleton) . JS.uncons
  splitPrimeSuffix x =
    if JS.null x
      then Nothing
      else Just (JS.init x, JS.singleton (JS.last x))
  inits = JS.inits
  tails = JS.tails
  span f = JS.span (f . JS.singleton)
  break f = JS.break (f . JS.singleton)
  dropWhile f = JS.dropWhile (f . JS.singleton)
  takeWhile f = JS.takeWhile (f . JS.singleton)
  spanMaybe s0 f t = case JS.foldr g id t (0, s0) of
    (i, s') | (prefix, suffix) <- JS.splitAt i t -> (prefix, suffix, s')
    where
      g c cont (i, s)
        | Just s' <- f s (JS.singleton c) =
            let i' = succ i :: Int in seq i' $ cont (i', s')
        | otherwise = (i, s)
  spanMaybe' s0 f t = case JS.foldr g id t (0, s0) of
    (i, s') | (prefix, suffix) <- JS.splitAt i t -> (prefix, suffix, s')
    where
      g c cont (i, s)
        | Just s' <- f s (JS.singleton c) =
            let i' = succ i :: Int in seq i' $ seq s' $ cont (i', s')
        | otherwise = (i, s)
  split f = JS.split f'
    where
      f' = f . JS.singleton
  splitAt = JS.splitAt
  drop = JS.drop
  take = JS.take
