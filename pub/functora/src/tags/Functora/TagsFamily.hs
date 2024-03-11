{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Functora.TagsFamily
  ( -- * Constructors
    -- $constructors
    TagsKind,
    type Tags,
    type (|+|),
    type (|-|),
    type (|&|),
    type NoTags,

    -- * Accessors
    -- $accessors
    HasKey,
    HasNotKey,
    HasTag,
    HasTags,
    GetTag,

    -- * Introspection
    -- $introspection
    Fgpt,
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
import Data.Type.Map (AsMap, Cmp, IsMap, Lookup, Mapping ((:->)), Member)
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
import qualified Language.Haskell.TH.Syntax as TH
import qualified LiftType
import Singlethongs as X
import Prelude

type TagsKind = [Mapping Type Type]

type NoTags = ('[] :: [Mapping Type Type])

type family Tags v where
  Tags (v :: k) =
    '[k ':-> Sing v]

type family tags |+| v where
  tags |+| v =
    AsMap (AddTagFamily v tags tags '[])

type family tags |-| v where
  tags |-| v =
    AsMap (UnTagFamily 'False v tags tags '[])

type family lhs |&| rhs where
  '[] |&| rhs = AsMap rhs
  ((_ ':-> v) ': lhs) |&| rhs = (lhs |&| (rhs |+| v))

type HasKey k tags =
  ( IsMap tags,
    Typeable (GetVals tags),
    Member k tags ~ 'True
  )

type HasNotKey k tags =
  ( IsMap tags,
    Typeable (GetVals tags),
    Member k tags ~ 'False
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

type family AddTagFamily v tags prev next where
  AddTagFamily (v :: k) _ '[] next =
    (k ':-> Sing v) ': next
  AddTagFamily (v :: k) tags ((k ':-> _) ': _) _ =
    TypeError
      ( 'ShowType v
          ':<>: 'Text " :: "
          ':<>: 'ShowType k
          ':<>: 'Text " tag conflicts with "
          ':<>: 'ShowType tags
      )
  AddTagFamily v tags (kv ': prev) next =
    AddTagFamily v tags prev (kv ': next)

type family UnTagFamily member v tags prev next where
  UnTagFamily 'True _ _ '[] next =
    next
  UnTagFamily 'False (v :: k) tags '[] _ =
    TypeError
      ( 'ShowType v
          ':<>: 'Text " :: "
          ':<>: 'ShowType k
          ':<>: 'Text " tag is missing in "
          ':<>: 'ShowType tags
      )
  UnTagFamily 'False (v :: k) tags ((k ':-> Sing v) ': prev) next =
    UnTagFamily 'True v tags prev next
  UnTagFamily _ (v :: k) tags ((k ':-> _) ': _) _ =
    TypeError
      ( 'ShowType v
          ':<>: 'Text " :: "
          ':<>: 'ShowType k
          ':<>: 'Text " tag conflicts with "
          ':<>: 'ShowType tags
      )
  UnTagFamily member v tags (kv ': prev) next =
    UnTagFamily member v tags prev (kv ': next)

type family GetTagFamily k tags prev where
  GetTagFamily k _ ((k ':-> v) ': _) = v
  GetTagFamily k tags (_ ': next) = GetTagFamily k tags next
  GetTagFamily k tags '[] =
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
  HasTagsFamily 'Nothing ((k ':-> v) ': sub) sup =
    HasTagsFamily
      ('Just (Lookup sup k == 'Just v))
      ((k ':-> v) ': sub)
      sup
  HasTagsFamily ('Just 'True) (_ ': sub) sup =
    HasTagsFamily 'Nothing sub sup
  HasTagsFamily ('Just 'False) ((k ':-> v) ': _) sup =
    TypeError
      ( 'ShowType v
          ':<>: 'Text " :: "
          ':<>: 'ShowType k
          ':<>: 'Text " tag is missing in "
          ':<>: 'ShowType sup
      )

type family ToValsFamily map lst where
  ToValsFamily '[] acc = acc
  ToValsFamily ((_ ':-> v) ': tail) acc =
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
  Cmp ((k0 :: Type) ':-> _) (k1 ':-> _) =
    Cmp (Fgpt k0) (Fgpt k1)
