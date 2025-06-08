{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module Network.GRPC.Server.Helpers where

import qualified Data.ByteString.Char8 as ByteString
import           Data.IORef
import           Network.GRPC.HTTP2.Types (HeaderValue, HeaderKey, GRPCStatus(..), trailerForStatusCode, grpcStatusH, grpcMessageH)
import           Network.Wai (Request)

#if MIN_VERSION_warp(3,3,0)

#else
import           Data.Maybe (fromMaybe)
import           Network.Wai.Handler.Warp (http2dataTrailers, defaultHTTP2Data, modifyHTTP2Data)
#endif

#if MIN_VERSION_base(4,11,0)
#else
import Data.Monoid ((<>))
#endif

-- | Helper to set the GRPCStatus on the trailers reply.
modifyGRPCStatus :: IORef [(HeaderKey, HeaderValue)] -> Request -> GRPCStatus -> IO ()
#if MIN_VERSION_warp(3,3,0)
modifyGRPCStatus ref _ (GRPCStatus s msg) =
  writeIORef ref trailers
#else
modifyGRPCStatus _ req (GRPCStatus s msg) = modifyHTTP2Data req $ \h2data ->
  Just $! (fromMaybe defaultHTTP2Data h2data) { http2dataTrailers = trailers }
#endif
  where
    !trailers = if ByteString.null msg then [status] else [status, message]
    status = (grpcStatusH, trailerForStatusCode s)
    message = (grpcMessageH, msg)
    
