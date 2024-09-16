{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.PreludeOrphan () where

import Data.Binary (Binary)
import qualified Data.Coerce as Coerce
import qualified Data.Data as Data
import Data.Generics (Data)
import Data.Ratio
import Data.Scientific (Scientific)
import Data.Tagged
import qualified Data.Typeable as Typeable
import Text.URI
import qualified Type.Reflection as Reflection
import Universum
import Witch.Mini
import qualified Prelude
#if defined(__GHCJS__) || defined(ghcjs_HOST_OS) || defined(wasi_HOST_OS)
import Data.JSString (JSString)
import Universum.Container.Class (checkingNotNull)
import qualified Data.Sequences as Seq
import qualified Data.JSString as JS
import qualified Data.JSString.Text as JS
import qualified Data.ByteString.Lazy as BL
import Miso.String ()
import qualified Data.Binary as Binary (get, put)
import qualified Data.Aeson as A
import Data.Functor.Contravariant (contramap)
import qualified Data.Text.Lazy as TL
import qualified Data.MonoTraversable as Mono
#endif

instance Binary URI

instance Binary (RText a)

instance Binary RTextLabel

instance Binary QueryParam

instance Binary Authority

instance Binary UserInfo

deriving stock instance Generic (Ratio a)

instance (Typeable a) => Show (MVar a) where
  show =
    const
      . mappend "<REDACTED> :: MVar "
      . Universum.show
      . Typeable.typeRep
      $ Proxy @a

instance Data SomeException where
  gunfold _ z = z . Data.fromConstr
  toConstr _ = error "TODO : toConstr SomeException"
  dataTypeOf _ = error "TODO : dataTypeOf SomeException"

deriving stock instance (Data a, Data b) => Data (TryFromException a b)

instance Data Reflection.SomeTypeRep where
  gunfold _ z = z . Data.fromConstr
  toConstr _ = error "TODO : toConstr SomeTypeRep"
  dataTypeOf _ = error "TODO : dataTypeOf SomeTypeRep"

deriving stock instance
  ( Typeable kind,
    Typeable (tags :: kind),
    Data rep
  ) =>
  Data (Tagged tags rep)

instance
  ( From source target,
    Integral target
  ) =>
  From (Ratio source) (Ratio target)
  where
  from x =
    from @source @target (numerator x)
      % from @source @target (denominator x)

instance
  forall a.
  ( TryFrom Integer a,
    Integral a
  ) =>
  TryFrom Scientific (Ratio a)
  where
  tryFrom x =
    first (withTarget . withSource x)
      . tryFrom @Rational @(Ratio a)
      $ toRational x

instance
  forall a b.
  ( TryFrom a b,
    Integral b
  ) =>
  TryFrom (Ratio a) (Ratio b)
  where
  tryFrom s =
    (%)
      <$> tryFrom' (numerator s)
      <*> tryFrom' (denominator s)
    where
      tryFrom' :: a -> Either (TryFromException (Ratio a) (Ratio b)) b
      tryFrom' = first (withTarget . withSource s) . tryFrom @a @b

withSource ::
  source2 ->
  TryFromException source1 t ->
  TryFromException source2 t
withSource s (TryFromException _ e) =
  TryFromException s e

withTarget ::
  forall target2 source target1.
  TryFromException source target1 ->
  TryFromException source target2
withTarget =
  Coerce.coerce

#if defined(__GHCJS__) || defined(ghcjs_HOST_OS) || defined(wasi_HOST_OS)

instance From String JSString where
  from = JS.pack

instance From JSString String where
  from = JS.unpack

instance From Text JSString where
  from = JS.textToJSString

instance From JSString Text where
  from = JS.textFromJSString

instance From TL.Text JSString where
  from = JS.lazyTextToJSString

instance From JSString TL.Text where
  from = JS.lazyTextFromJSString

instance ConvertUtf8 JSString ByteString where
  encodeUtf8 = encodeUtf8 . JS.unpack
  decodeUtf8 = JS.pack . decodeUtf8
  decodeUtf8Strict = fmap JS.pack . decodeUtf8Strict

instance ConvertUtf8 JSString BL.ByteString where
  encodeUtf8 = encodeUtf8 . JS.unpack
  decodeUtf8 = JS.pack . decodeUtf8
  decodeUtf8Strict = fmap JS.pack . decodeUtf8Strict

instance Binary JSString where
  put = Binary.put . encodeUtf8 @JSString @ByteString
  get = do
    bs <- Binary.get
    case decodeUtf8Strict @JSString @ByteString bs of
      Right x -> pure x
      Left e -> fail $ displayException e

instance A.ToJSONKey JSString where
  toJSONKey = contramap (from @JSString @Text) $ A.toJSONKey @Text

instance A.FromJSONKey JSString where
  fromJSONKey = fmap (from @Text @JSString) $ A.fromJSONKey @Text

type instance Mono.Element JSString = Char

instance Mono.GrowingAppend JSString

instance Seq.Textual JSString where
  words = JS.words
  unwords = JS.unwords . Mono.otoList
  lines = JS.lines
  unlines = JS.unlines . Mono.otoList
  toLower = JS.toLower
  toUpper = JS.toUpper
  toCaseFold = JS.toCaseFold
  {-# INLINE words #-}
  {-# INLINE unwords #-}
  {-# INLINE lines #-}
  {-# INLINE unlines #-}
  {-# INLINE toLower #-}
  {-# INLINE toUpper #-}
  {-# INLINE toCaseFold #-}

instance Seq.IsSequence JSString where
  fromList = JS.pack
  lengthIndex = JS.length
  replicate i c = JS.replicate i (JS.singleton c)
  filter = JS.filter
  break = JS.break
  span = JS.span
  dropWhile = JS.dropWhile
  takeWhile = JS.takeWhile
  splitAt = JS.splitAt
  take = JS.take
  drop = JS.drop
  dropEnd = JS.dropEnd
  partition = JS.partition
  uncons = JS.uncons
  unsnoc t
    | JS.null t = Nothing
    | otherwise = Just (JS.init t, JS.last t)
  groupBy = JS.groupBy
  tailEx = JS.tail
  initEx = JS.init
  splitWhen = JS.split
  {-# INLINE fromList #-}
  {-# INLINE break #-}
  {-# INLINE span #-}
  {-# INLINE dropWhile #-}
  {-# INLINE takeWhile #-}
  {-# INLINE splitAt #-}
  {-# INLINE take #-}
  {-# INLINE drop #-}
  {-# INLINE partition #-}
  {-# INLINE uncons #-}
  {-# INLINE unsnoc #-}
  {-# INLINE filter #-}
  {-# INLINE replicate #-}
  {-# INLINE groupBy #-}
  {-# INLINE tailEx #-}
  {-# INLINE initEx #-}
  {-# INLINE splitWhen #-}
  index t i
    | i < 0 || i >= JS.length t = Nothing
    | otherwise = Just (JS.index t i)
  indexEx = JS.index
  unsafeIndex = JS.index
  {-# INLINE index #-}
  {-# INLINE indexEx #-}
  {-# INLINE unsafeIndex #-}

instance Mono.MonoPointed JSString where
  opoint = JS.singleton
  {-# INLINE opoint #-}

instance Mono.MonoTraversable JSString where
  otraverse f = fmap JS.pack . traverse f . JS.unpack
  {-# INLINE otraverse #-}

instance Mono.MonoFunctor JSString where
  omap = JS.map
  {-# INLINE omap #-}

instance Mono.MonoFoldable JSString where
  ofoldMap f = Mono.ofoldr (mappend . f) mempty
  ofoldr = JS.foldr
  ofoldl' = JS.foldl'
  otoList = JS.unpack
  oall = JS.all
  oany = JS.any
  onull = JS.null
  olength = JS.length
  ofoldr1Ex = JS.foldr1
  ofoldl1Ex' = JS.foldl1'
  headEx = JS.head
  lastEx = JS.last
  {-# INLINE ofoldMap #-}
  {-# INLINE ofoldr #-}
  {-# INLINE ofoldl' #-}
  {-# INLINE otoList #-}
  {-# INLINE oall #-}
  {-# INLINE oany #-}
  {-# INLINE onull #-}
  {-# INLINE olength #-}
  {-# INLINE ofoldr1Ex #-}
  {-# INLINE ofoldl1Ex' #-}
  {-# INLINE headEx #-}
  {-# INLINE lastEx #-}

instance Seq.SemiSequence JSString where
  type Index JSString = Int
  intersperse = JS.intersperse
  reverse = JS.reverse
  find = JS.find
  cons = JS.cons
  snoc = JS.snoc
  sortBy = Seq.defaultSortBy
  {-# INLINE intersperse #-}
  {-# INLINE reverse #-}
  {-# INLINE find #-}
  {-# INLINE sortBy #-}
  {-# INLINE cons #-}
  {-# INLINE snoc #-}

instance Container JSString where
  type Element JSString = Char
  toList = JS.unpack
  {-# INLINE toList #-}
  null = JS.null
  {-# INLINE null #-}
  foldr = JS.foldr
  {-# INLINE foldr #-}
  foldl = JS.foldl
  {-# INLINE foldl #-}
  foldl' = JS.foldl'
  {-# INLINE foldl' #-}
  safeFoldr1 f = checkingNotNull (JS.foldr1 f)
  {-# INLINE safeFoldr1 #-}
  safeFoldl1 f = checkingNotNull (JS.foldl1 f)
  {-# INLINE safeFoldl1 #-}
  length = JS.length
  {-# INLINE length #-}
  elem c = JS.isInfixOf (JS.singleton c) -- there are rewrite rules for this
  {-# INLINE elem #-}
  safeMaximum = checkingNotNull JS.maximum
  {-# INLINE safeMaximum #-}
  safeMinimum = checkingNotNull JS.minimum
  {-# INLINE safeMinimum #-}
  all = JS.all
  {-# INLINE all #-}
  any = JS.any
  {-# INLINE any #-}
  find = JS.find
  {-# INLINE find #-}
  safeHead = fmap fst . JS.uncons
  {-# INLINE safeHead #-}

#endif
