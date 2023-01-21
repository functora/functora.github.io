module Text.PrettyPrint.GenericPretty.Type
  ( PrettyLog (..),
    SecretVision (..),
  )
where

import Text.PrettyPrint.GenericPretty (Out (..))
import Universum

data PrettyLog a
  = PrettyLog a
  | SecretLog SecretVision a
  deriving stock (Eq, Ord)

data SecretVision
  = SecretVisible
  | SecretHidden
  deriving stock (Eq, Ord, Show, Read, Generic)

instance Out SecretVision

instance (Out a) => Out (PrettyLog a) where
  docPrec n = \case
    PrettyLog x -> docPrec n x
    SecretLog SecretVisible x -> docPrec n x
    SecretLog SecretHidden _ -> "<REDACTED>"
  doc = \case
    PrettyLog x -> doc x
    SecretLog SecretVisible x -> doc x
    SecretLog SecretHidden _ -> "<REDACTED>"
