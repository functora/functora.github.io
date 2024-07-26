module Functora.Miso.Storage
  ( insertStorage,
    selectStorage,
  )
where

import qualified Data.Text.Lazy.Encoding as TL
import Functora.Miso.Prelude
import qualified Language.Javascript.JSaddle as JS

insertStorage :: (ToJSON a) => MisoString -> a -> JSM ()
insertStorage key raw = do
  val <-
    either throw (pure . toMisoString)
      . TL.decodeUtf8'
      . unTagged
      $ encodeJson raw
  void
    $ JS.global
    ^. JS.js2 @MisoString "insertStorage" key val

selectStorage :: (FromJSON a) => MisoString -> (Maybe a -> JSM ()) -> JSM ()
selectStorage key after = do
  success <- JS.function $ \_ _ ->
    handleAny (\e -> consoleLog e >> after Nothing) . \case
      [val] -> do
        valExist <- ghcjsPure $ JS.isTruthy val
        if not valExist
          then after Nothing
          else do
            raw <- JS.fromJSVal @MisoString val
            str <- maybe (throwString @MisoString "Storage bad type!") pure raw
            res <- either throwString pure $ decodeJson str
            after $ Just res
      _ ->
        throwString @MisoString "Storage bad argv!"
  failure <-
    JS.function $ \_ _ _ -> consoleLog @MisoString "Storage reader failure!"
  prom <-
    JS.global ^. JS.js1 @MisoString "selectStorage" key
  void
    $ prom
    ^. JS.js2 @MisoString "then" success failure
