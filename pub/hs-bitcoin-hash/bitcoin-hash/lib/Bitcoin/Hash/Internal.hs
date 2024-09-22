{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}

module Bitcoin.Hash.Internal
  ( G.hash160
  , G.hash256
  , G.ripemd160
  , G.sha256
  , G.hmacSHA512
  ) where

#ifdef ghcjs_HOST_OS
import Bitcoin.Hash.GHCJS as G
#else
import Bitcoin.Hash.GHC as G
#endif
