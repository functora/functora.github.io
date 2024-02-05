{-# LANGUAGE CPP #-}

module Main (main) where

#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai as Wai
import Network.Wai.Application.Static
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as Ws
#endif
import Functora.Prelude
import Functora.Rates as Rates
import Miso hiding (view)
import qualified Miso
import Miso.String

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp app =
  Warp.runSettings
    ( Warp.setPort
        8080
        (Warp.setTimeout 3600 Warp.defaultSettings)
    )
    =<< JSaddle.jsaddleOr
      Ws.defaultConnectionOptions
      (app >> syncPoint)
      router
  where
    router req sendResp =
      case Wai.pathInfo req of
        ("static" : _) -> staticApp (defaultWebAppSettings ".") req sendResp
        _ -> JSaddle.jsaddleApp req sendResp
#else
runApp :: IO () -> IO ()
runApp = id
#endif

type Model = Int

data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

main :: IO ()
main = do
  --
  -- TODO
  --
  res <- Rates.fetchCurrencies
  putStrLn $ inspect @Text res
  --
  -- TODO
  --
  runApp
    $ startApp
      App
        { model = 0,
          update = updateModel,
          Miso.view = viewModel,
          subs = mempty,
          events = defaultEvents,
          initialAction = SayHelloWorld,
          mountPoint = Nothing, -- defaults to 'body'
          logLevel = Off
        }

updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp m = noEff m
updateModel SayHelloWorld m =
  m <# do
    liftIO (putStrLn @Text "Hello World") >> pure NoOp

viewModel :: Model -> View Action
viewModel x =
  div_
    []
    [ link_ [rel_ "stylesheet", href_ "static/picnic.min.css"],
      link_ [rel_ "stylesheet", href_ "static/app.css"],
      mainWidget x,
      script_ [src_ "static/clipboard.min.js"] mempty,
      script_ [src_ "static/app.js", defer_ "defer"] mempty
    ]

mainWidget :: Model -> View Action
mainWidget x =
  row
    [ button_ [onClick AddOne] [text "+"],
      text (ms x),
      button_ [onClick SubtractOne] [text "-"]
    ]

row :: [View action] -> View action
row = div_ [class_ $ "flex one center"]
