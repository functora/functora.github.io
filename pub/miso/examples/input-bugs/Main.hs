{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- \| Haskell module declaration

-- | Haskell language pragma
module Main where

-- \| Miso framework import

import Control.Monad.IO.Class
import Miso
import Miso.String (MisoString, ms)

#ifdef IOS
import Language.Javascript.JSaddle.WKWebView as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run
#else
import Language.Javascript.JSaddle.Warp as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run 8080
#endif

-- | Type synonym for an application model
type Model = [MisoString]

-- | Sum type for application events
data Action
  = Noop
  | Update (Model -> Model)

-- | Entry point for a miso application
main :: IO ()
main = runApp $ miso $ \_ -> App {..}
  where
    initialAction = Noop -- initial action to be executed on application load
    model = mempty -- initial model
    update = updateModel -- update function
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs = [] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
    logLevel = Off -- Used to copy DOM into VDOM, applies only to `miso` function

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel Noop st = noEff st
updateModel (Update f) st = noEff $ f st

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel inputs =
  div_
    mempty
    $ [ button_
          [ onClick $ Update (<> [mempty])
          ]
          [ text "+ Add Input"
          ],
        button_
          [ onClick . Update $ \st -> if null st then st else init st
          ]
          [ text "- Remove Input"
          ]
      ]
      <> fmap
        ( uncurry $ \idx prev ->
            input_
              [ value_ prev,
                onInput $ Update . updateAt idx . const
              ]
        )
        ( zip [0 ..] inputs
        )
      <> fmap
        ( img_
            . (: mempty)
            . src_
        )
        inputs

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt idx f xs
  | idx < 0 || idx > length xs = xs
  | otherwise = do
      (i, x) <- zip [0 ..] xs
      pure $ if i == idx then f x else x
