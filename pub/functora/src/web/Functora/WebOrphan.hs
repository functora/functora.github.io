{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.WebOrphan () where

#if !defined(__GHCJS__) && !defined(ghcjs_HOST_OS) && !defined(wasi_HOST_OS)
import Functora.Prelude
import qualified Data.Data as Data
import qualified Data.Streaming.Zlib as Zlib
import qualified Network.HTTP.Client as Web
#if __GLASGOW_HASKELL__ < 900
import qualified Network.HTTP.Types as Web
#endif
#endif

#ifdef wasi_HOST_OS
import Functora.Prelude
import qualified GHCJS.DOM.Types as JSDOM
#endif

#if !defined(__GHCJS__) && !defined(ghcjs_HOST_OS) && !defined(wasi_HOST_OS)
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

#if __GLASGOW_HASKELL__ < 900
deriving stock instance Data Web.Status
#endif

#endif

--
-- NOTE : this is evil instance, ok for now..
--
#ifdef wasi_HOST_OS
instance Eq JSDOM.JSContextRef where
  _ == _ = True
#endif
