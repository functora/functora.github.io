{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Functora.TagsFamily
  ( -- * Constructors
    -- $constructors
    type Tags,
    type NoTags,
    type (|+|),
    type (|-|),
    type (|&|),

    -- * Accessors
    -- $accessors
    MaybeGetTag,
    GetTag,
    HasTag,

    -- * Fingerprints
    -- $fingerprints
    Fgpt,
    mkFgpt,

    -- * Reexport
    module X,
  )
where

import Data.Data as X (Data)
import Data.Kind (Type)
import Data.Type.Bool as X (type Not, type (&&))
import Data.Type.Equality as X (type (==))
import Data.Type.Map (AsMap, Cmp, Mapping ((:->)))
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

-- $constructors
-- Constructors

type family Tags v where
  Tags (v :: k) =
    '[k ':-> Sing v]

type NoTags = ('[] :: [Mapping Type Type])

type family tags |+| v where
  tags |+| v =
    AsMap (AddTagFamily v tags tags '[])

type family tags |-| v where
  tags |-| v =
    AsMap (UnTagFamily 'False v tags tags '[])

type family lhs |&| rhs where
  '[] |&| rhs = AsMap rhs
  ((_ ':-> v) ': lhs) |&| rhs = lhs |&| (rhs |+| v)

-- $accessors
-- Accessors

type MaybeGetTag k tags = MaybeGetTagFamily ('Nothing :: Maybe k) k tags tags

type GetTag k tags = GetTagFamily k tags (MaybeGetTag k tags)

type HasTag (v :: k) tags =
  ( SingI v,
    Typeable v,
    Typeable k,
    Typeable tags,
    v ~ GetTag k tags
  )

-- $fingerprints
-- Fingerprints

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

type family MaybeGetTagFamily mv k tags prev where
  MaybeGetTagFamily ('Just (v :: k)) k _ '[] = 'Just v
  MaybeGetTagFamily 'Nothing _ _ '[] = 'Nothing
  MaybeGetTagFamily ('Just v) k tags ((k ':-> Sing v) ': _) =
    TypeError
      ( 'ShowType v
          ':<>: 'Text " :: "
          ':<>: 'ShowType k
          ':<>: 'Text " tag conflicts with "
          ':<>: 'ShowType tags
      )
  MaybeGetTagFamily 'Nothing k tags ((k ':-> Sing v) ': next) =
    MaybeGetTagFamily ('Just v) k tags next
  MaybeGetTagFamily mv k tags (_ ': next) =
    MaybeGetTagFamily mv k tags next

type family GetTagFamily k tags mv where
  GetTagFamily k _ ('Just (v :: k)) = v
  GetTagFamily k tags ('Just v) =
    TypeError
      ( 'ShowType v
          ':<>: 'Text " :: "
          ':<>: 'ShowType k
          ':<>: 'Text " tag conflicts with "
          ':<>: 'ShowType tags
      )
  GetTagFamily k tags 'Nothing =
    TypeError
      ( 'ShowType k
          ':<>: 'Text " key is missing in "
          ':<>: 'ShowType tags
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
