{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.WebOrphan () where

#ifndef __GHCJS__
import Functora.Prelude
import Yesod.Core (PathPiece (..))
import qualified Data.Data as Data
import qualified Data.Streaming.Zlib as Zlib
import qualified Network.HTTP.Client as Web
#endif

#ifndef __GHCJS__
instance Data Web.Request where
  gunfold _ z = z . Data.fromConstr
  toConstr _ = error "TODO : toConstr Web.Request"
  dataTypeOf _ = error "TODO : dataTypeOf Web.Request"

instance (Typeable a) => Data (Web.Response a) where
  gunfold _ z = z . Data.fromConstr
  toConstr _ = error "TODO : toConstr Web.Response"
  dataTypeOf _ = error "TODO : dataTypeOf Web.Response"

deriving stock instance Data Web.HttpException

deriving stock instance Data Web.HttpExceptionContent

deriving stock instance Data Zlib.ZlibException

instance PathPiece Natural where
  fromPathPiece raw = do
    mid <- fromPathPiece raw
    rightToMaybe $ tryFrom @Integer @Natural mid
  toPathPiece =
    toPathPiece . from @Natural @Integer
#endif
