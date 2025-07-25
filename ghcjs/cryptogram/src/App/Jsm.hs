module App.Jsm
  ( getChatId,
    switchInlineQuery,
    insertSecureStorage,
    selectSecureStorage,
  )
where

import Functora.Cfg
import Functora.Miso.Prelude
import qualified GHCJS.Types as JS
import Language.Javascript.JSaddle ((!))
import qualified Language.Javascript.JSaddle as JS

getPkg :: JSM JS.JSVal
getPkg = do
  window <- JS.jsg ("window" :: Unicode)
  window ! ("Telegram" :: Unicode) ! ("WebApp" :: Unicode)

getSub :: Unicode -> JSM JS.JSVal
getSub arg = do
  pkg <- getPkg
  pkg ! arg

getChatId :: JSM Int
getChatId = do
  pkg <- getPkg
  ini <- pkg ! ("initDataUnsafe" :: Unicode)
  rcvr <- ini ! ("receiver" :: Unicode)
  chat <- ini ! ("chat" :: Unicode)
  user <- ini ! ("user" :: Unicode)
  isRcvr <- ghcjsPure $ JS.isTruthy rcvr
  isChat <- ghcjsPure $ JS.isTruthy chat
  let subj =
        if
          | isRcvr -> rcvr
          | isChat -> chat
          | otherwise -> user
  uid <- subj ! ("id" :: Unicode)
  JS.fromJSValUnchecked uid

switchInlineQuery :: Unicode -> JSM ()
switchInlineQuery val = do
  obj <- getPkg
  void $ obj ^. JS.js1 @Unicode "switchInlineQuery" val

insertSecureStorage :: (Binary a) => Unicode -> a -> JSM ()
insertSecureStorage key val = do
  sub <- getSub "SecureStorage"
  void
    $ sub
    ^. JS.js2 @Unicode "setItem" key (encodeBinaryB64Url @Unicode val)

selectSecureStorage ::
  (Show a, Data a, Binary a) => Unicode -> (Maybe a -> JSM ()) -> JSM ()
selectSecureStorage key after = do
  sub <- getSub "SecureStorage"
  fun <- JS.function $ \_ _ ->
    handleAny
      ( \e -> do
          alert . from @String @Unicode $ displayException e
          after Nothing
      )
      . \case
        [failure, success] -> do
          bad <- ghcjsPure $ JS.isTruthy failure
          if bad
            then throwString @Unicode "selectSecureStorage failure!"
            else do
              raw <-
                JS.fromJSValUnchecked @Unicode success
              val <-
                either (throwString @Unicode . inspect) pure
                  $ decodeBinaryB64Url raw
              after
                $ Just val
        _ ->
          throwString @Unicode "selectSecureStorage bad argv!"
  void
    $ sub
    ^. JS.js2 @Unicode "getItem" key fun
