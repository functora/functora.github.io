{-# LANGUAGE RankNTypes #-}

module Text.PrettyPrint.GenericPretty.Class
  ( GenericPrettyEnv (..),
    Inspect (..),
  )
where

import qualified Text.Pretty.Simple as PrettySimple
import Text.PrettyPrint.GenericPretty (Out)
import qualified Text.PrettyPrint.GenericPretty.Type as Type
import qualified Text.PrettyPrint.GenericPretty.Util as Util
import Universum hiding (show)

newtype Inspect = Inspect {unInspect :: forall txt src. (Out src, IsString txt) => src -> txt}

class (MonadIO m) => GenericPrettyEnv m where
  getStyle :: m PrettySimple.OutputOptions
  getStyle =
    pure PrettySimple.defaultOutputOptionsDarkBg
  getSecretVision :: m Type.SecretVision
  getSecretVision =
    pure Type.SecretHidden
  getInspect :: m Inspect
  getInspect = do
    style <- getStyle
    pure Inspect {unInspect = Util.inspectStyle style}
  getInspectSecret :: m Inspect
  getInspectSecret = do
    secretVision <- getSecretVision
    case secretVision of
      Type.SecretVisible -> getInspect
      Type.SecretHidden -> pure Inspect {unInspect = const "<REDACTED>"}
