{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Functora.TagsFamily
  ( -- * Constructors
    -- $constructors
    type Tags,
    type (|+|),
    type NoTags,

    -- * Accessors
    -- $accessors
    HasKey,
    HasTag,
    HasTags,
    GetTag,

    -- * Introspection
    -- $introspection
    Fgpt,
    mkEnum,
    mkFgpt,
    inspectTags,

    -- * Reexport
    module X,
  )
where

import Data.Data as X (Data)
import Data.Kind (Type)
import Data.String (IsString, fromString)
import Data.Type.Bool as X (type Not, type (&&))
import Data.Type.Equality as X (type (==))
import Data.Type.Map hiding ((:->))
import qualified Data.Type.Map as TM (Mapping ((:->)))
import Data.Typeable
import qualified Data.Typeable as Typeable
import GHC.Generics as X (Generic)
import GHC.TypeLits as X
  ( ErrorMessage (..),
    SomeSymbol (..),
    Symbol,
    TypeError,
    someSymbolVal,
  )
import qualified Language.Haskell.TH.Lib as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified LiftType
import Singlethongs as X
import Prelude

type family Tags v where
  Tags (v :: k) =
    '[k :-> v]

type family tags |+| v where
  tags |+| (v :: k) =
    AsMap (AddTagFamily (Not (Member k tags)) v tags)

type NoTags = ('[] :: [Mapping Type Type])

type HasKey k tags =
  ( IsMap tags,
    Typeable (GetVals tags),
    Member k tags ~ 'True
  )

type HasTag v tags =
  ( HasTags (Tags v) tags
  )

type HasTags sub sup =
  ( IsMap sub,
    IsMap sup,
    Typeable (GetVals sub),
    Typeable (GetVals sup),
    HasTagsFamily 'Nothing sub sup ~ 'True
  )

type GetTag (v :: k) tags =
  ( Sing v ~ GetTagFamily k tags tags,
    SingI v,
    HasTag v tags
  )

type family Fgpt (a :: k) :: Symbol

mkEnum :: TH.Name -> TH.DecsQ
mkEnum name = do
  let typ = TH.conT name
  [d|
    deriving stock instance Eq $typ

    deriving stock instance Ord $typ

    deriving stock instance Show $typ

    deriving stock instance Enum $typ

    deriving stock instance Bounded $typ

    deriving stock instance Read $typ

    deriving stock instance Data $typ

    deriving stock instance Generic $typ
    |]

mkFgpt :: forall a. (Typeable a) => TH.Q [TH.Dec]
mkFgpt =
  [d|
    type instance
      Fgpt $(LiftType.liftTypeQ @a) =
        $( do
            let rep = Typeable.typeRep $ Proxy @a
            let con = Typeable.typeRepTyCon rep
            pure
              . LiftType.typeRepToType
              . (\(SomeSymbol x) -> Typeable.typeRep x)
              . someSymbolVal
              . ( \x ->
                    Typeable.tyConPackage con
                      <> ":"
                      <> Typeable.tyConModule con
                      <> "."
                      <> Typeable.tyConName con
                      <> "-"
                      <> x
                )
              . show
              $ Typeable.typeRepFingerprint rep
         )
    |]

inspectTags ::
  forall tags text vals.
  ( HasVals vals tags,
    IsString text
  ) =>
  text
inspectTags =
  fromString
    . show
    . Typeable.typeRep
    $ Proxy @vals

--
-- Private
--

type family k :-> v where
  k :-> (v :: k) =
    k 'TM.:-> Sing v

type family AddTagFamily member v tags where
  AddTagFamily 'True (v :: k) tags =
    (k :-> v) ': tags
  AddTagFamily 'False v tags =
    TypeError
      ( 'ShowType v
          ':<>: 'Text " tag is conflicting with "
          ':<>: 'ShowType tags
      )

type family GetTagFamily k tail tags where
  GetTagFamily k ((k 'TM.:-> v) ': _) _ = v
  GetTagFamily k (_ ': tail) tags = GetTagFamily k tail tags
  GetTagFamily k '[] tags =
    TypeError
      ( 'ShowType k
          ':<>: 'Text " key is missing in "
          ':<>: 'ShowType tags
      )

type family HasTagsFamily hastag submap supmap where
  HasTagsFamily 'Nothing '[] _ = 'True
  HasTagsFamily has '[] sup =
    TypeError
      ( 'Text "Impossible HasTagsFamily "
          ':<>: 'ShowType has
          ':<>: 'Text " clause with "
          ':<>: 'ShowType sup
      )
  HasTagsFamily 'Nothing ((k 'TM.:-> v) ': sub) sup =
    HasTagsFamily
      ('Just (Lookup sup k == 'Just v))
      ((k 'TM.:-> v) ': sub)
      sup
  HasTagsFamily ('Just 'True) (_ ': sub) sup =
    HasTagsFamily 'Nothing sub sup
  HasTagsFamily ('Just 'False) ((_ 'TM.:-> v) ': _) sup =
    TypeError
      ( 'ShowType v
          ':<>: 'Text " tag is missing in "
          ':<>: 'ShowType sup
      )

type family ToValsFamily map lst where
  ToValsFamily '[] acc = acc
  ToValsFamily ((_ 'TM.:-> v) ': tail) acc =
    ToValsFamily tail (v ': acc)

type GetVals tags = ToValsFamily tags '[]

type HasVals vals tags =
  ( IsMap tags,
    Typeable vals,
    vals ~ GetVals tags
  )

--
-- TODO : NEED A PROPER INSTANCE!
-- At the moment I don't know how to get
-- LT/GT for a pair of two different
-- arbitrary types. Without proper
-- instance the ordering of map items
-- does matter (it should not be).
-- Using Fgpt workaround for now.
--

type instance
  Cmp ((k0 :: Type) 'TM.:-> _) (k1 'TM.:-> _) =
    Cmp (Fgpt k0) (Fgpt k1)
