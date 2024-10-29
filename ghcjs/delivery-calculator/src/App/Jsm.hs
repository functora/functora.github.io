{-# OPTIONS_GHC -Wno-deprecations #-}

module App.Jsm (fetchBlobUris) where

import App.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.Generics as Syb
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude

fetchBlobUris :: (Data a) => a -> JSM (Map Unicode Rfc2397)
fetchBlobUris st = do
  vars <-
    forM blobUris $ \uri -> do
      var <- newEmptyMVar
      Jsm.fetchUrlAsRfc2397 (Just 50000) uri
        $ liftIO
        . putMVar var
        . fmap (uri,)
        . (>>= decodeRfc2397 . BL.fromStrict)
      pure var
  fmap (fromList . catMaybes)
    . forM vars
    $ liftIO
    . takeMVar
  where
    blobUris =
      nubOrd
        . filter (isPrefixOf "blob:")
        . fmap (^. #fieldInput . #uniqueValue)
        $ Syb.listify
          (\(x :: Field DynamicField Unique) -> fieldType x == FieldTypeImage)
          st
