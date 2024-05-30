module Functora.Elm2MisoSpec (spec) where

import Data.String.QQ
import Functora.Elm2Miso (elm2miso)
import Test.Hspec
import Prelude

approxElm :: String
approxElm =
  [s|
module Material.TopAppBar
exposing
    ( Config, config
    , setFixed
    , setDense
    , setAttributes
    , regular
    , row, section, alignEnd, alignStart
    , navigationIcon, title
    , actionItem
    , fixedAdjust
    , denseFixedAdjust
    , denseProminentFixedAdjust
    , prominentFixedAdjust
    , shortFixedAdjust
    , short
    , shortCollapsed
    , prominent
    )

import Html exposing (Html)
import Html.Attributes exposing (class)

type Config msg
    = Config
        { dense : Bool
        , fixed : Bool
        , additionalAttributes : List (Html.Attribute msg)
        }

type Variant
    = Regular
    | Short
    | ShortCollapsed
    | Prominent

config : Config msg
config =
    Config
        { dense = False
        , fixed = False
        , additionalAttributes = []
        }

setDense : Bool -> Config msg -> Config msg
setDense dense (Config config_) =
    Config { config_ | dense = dense }

setFixed : Bool -> Config msg -> Config msg
setFixed fixed (Config config_) =
    Config { config_ | fixed = fixed }

setAttributes : List (Html.Attribute msg) -> Config msg -> Config msg
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }

genericTopAppBar : Variant -> Config msg -> List (Html msg) -> Html msg
genericTopAppBar variant ((Config { additionalAttributes=additionalAttributes }) as config_) nodes =
    Html.node "mdc-top-app-bar"
        (List.filterMap identity
            [ rootCs
            , variantCs variant
            , denseCs config_
            , fixedCs config_
            ]
            ++ additionalAttributes
        )
        nodes
|]

approxMiso :: String
approxMiso =
  [s|
module Material.TopAppBar

    ( Config, config
    , setFixed
    , setDense
    , setAttributes
    , regular
    , row, section, alignEnd, alignStart
    , navigationIcon, title
    , actionItem
    , fixedAdjust
    , denseFixedAdjust
    , denseProminentFixedAdjust
    , prominentFixedAdjust
    , shortFixedAdjust
    , short
    , shortCollapsed
    , prominent
    )

import Html  (Html)
import Miso.Attributes  (class)

data Config msg
    = Config
        { dense :: Bool
        , fixed :: Bool
        , additionalAttributes :: [Miso.Attribute msg]
        }

data Variant
    = Regular
    | Short
    | ShortCollapsed
    | Prominent

config :: Config msg
config =
    Config
        { dense = False
        , fixed = False
        , additionalAttributes = []
        }

setDense :: Bool -> Config msg -> Config msg
setDense dense config_ =
    config_ { dense = dense }

setFixed :: Bool -> Config msg -> Config msg
setFixed fixed config_ =
    config_ { fixed = fixed }

setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
    config_ { additionalAttributes = additionalAttributes }

genericTopAppBar :: Variant -> Config msg -> [Miso.View msg] -> Miso.View msg
genericTopAppBar variant (config_@Config { additionalAttributes=additionalAttributes }) nodes =
    Miso.nodeHtml "mdc-top-app-bar"
        (Maybe.mapMaybe id
            [ rootCs
            , variantCs variant
            , denseCs config_
            , fixedCs config_
            ]
            ++ additionalAttributes
        )
        nodes
|]

spec :: Spec
spec =
  it "elm2miso" $
    elm2miso approxElm `shouldBe` approxMiso
