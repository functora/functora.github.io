module App.Jsm (fetchBlobUris) where

import App.Types
import qualified Data.Generics as Syb
import qualified Data.Map as Map
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude

fetchBlobUris :: (Data a) => a -> JSM a
fetchBlobUris st = do
  vars <-
    forM blobUris $ \uri -> do
      var <- newEmptyMVar
      Jsm.fetchUrlAsRfc2397 uri $ liftIO . putMVar var . fmap (uri,)
      pure var
  vals <-
    fmap (fromList . catMaybes)
      . forM vars
      $ liftIO
      . takeMVar
  pure
    $ Syb.everywhere
      ( Syb.mkT $ \(x :: Field DynamicField Unique) ->
          case Map.lookup (x ^. #fieldInput . #uniqueValue) vals of
            Just val
              | fieldType x == FieldTypeImage ->
                  x
                    & #fieldInput
                    . #uniqueValue
                    .~ val
                    & #fieldOutput
                    .~ DynamicFieldText val
            _ -> x
      )
      st
  where
    blobUris =
      nubOrd
        . filter (isPrefixOf "blob:")
        . fmap (^. #fieldInput . #uniqueValue)
        $ Syb.listify
          (\(x :: Field DynamicField Unique) -> fieldType x == FieldTypeImage)
          st
