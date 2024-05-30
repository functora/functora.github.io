{-# LANGUAGE OverloadedStrings #-}

module Material.TextArea
  ( Config,
    config,
    setOnInput,
    setOnChange,
    setLabel,
    setFullwidth,
    setValue,
    setPlaceholder,
    setRows,
    setCols,
    setDisabled,
    setRequired,
    setValid,
    setMinLength,
    setMaxLength,
    setAttributes,
    filled,
    outlined,
  )
where

import qualified Data.Aeson
import qualified Data.Maybe as Maybe
import qualified Material.Icon as Icon
import qualified Miso
import qualified Miso.Html.Event
import Miso.String (ms)
import qualified Miso.String

-- | Configuration of a text area
data Config msg = Config
  { label :: Maybe String,
    fullwidth :: Bool,
    value :: Maybe String,
    placeholder :: Maybe String,
    rows :: Maybe Int,
    cols :: Maybe Int,
    disabled :: Bool,
    required :: Bool,
    valid :: Bool,
    minLength :: Maybe Int,
    maxLength :: Maybe Int,
    additionalAttributes :: [Miso.Attribute msg],
    onInput :: Maybe (String -> msg),
    onChange :: Maybe (String -> msg)
  }

-- | Default configuration of a text area
config :: Config msg
config =
  Config
    { label = Nothing,
      fullwidth = False,
      value = Nothing,
      placeholder = Nothing,
      rows = Nothing,
      cols = Nothing,
      disabled = False,
      required = False,
      valid = True,
      minLength = Nothing,
      maxLength = Nothing,
      additionalAttributes = [],
      onInput = Nothing,
      onChange = Nothing
    }

-- | Specify a text area's label
setLabel :: Maybe String -> Config msg -> Config msg
setLabel label config_ =
  config_ {label = label}

-- | Specify a text field to be fullwidth
setFullwidth :: Bool -> Config msg -> Config msg
setFullwidth fullwidth config_ =
  config_ {fullwidth = fullwidth}

-- | Specify a text area's value
setValue :: Maybe String -> Config msg -> Config msg
setValue value config_ =
  config_ {value = value}

-- | Specify a text area's placeholder
setPlaceholder :: Maybe String -> Config msg -> Config msg
setPlaceholder placeholder config_ =
  config_ {placeholder = placeholder}

-- | Specify a text area's number of rows
setRows :: Maybe Int -> Config msg -> Config msg
setRows rows config_ =
  config_ {rows = rows}

-- | Specify a text area's number of columns
setCols :: Maybe Int -> Config msg -> Config msg
setCols cols config_ =
  config_ {cols = cols}

-- | Specify a text area to be disabled
--
-- Disabled text areas cannot be interacted with and have no visual interaction
-- effect.
setDisabled :: Bool -> Config msg -> Config msg
setDisabled disabled config_ =
  config_ {disabled = disabled}

-- | Specify a text area to be required
setRequired :: Bool -> Config msg -> Config msg
setRequired required config_ =
  config_ {required = required}

-- | Specify a text area to be valid
setValid :: Bool -> Config msg -> Config msg
setValid valid config_ =
  config_ {valid = valid}

-- | Specify a text area's minimum length
setMinLength :: Maybe Int -> Config msg -> Config msg
setMinLength minLength config_ =
  config_ {minLength = minLength}

-- | Specify a text area's maximum length
setMaxLength :: Maybe Int -> Config msg -> Config msg
setMaxLength maxLength config_ =
  config_ {maxLength = maxLength}

-- | Specify additional attributes
setAttributes :: [Miso.Attribute msg] -> Config msg -> Config msg
setAttributes additionalAttributes config_ =
  config_ {additionalAttributes = additionalAttributes}

-- | Specify a message when the user changes the value inside the text area
setOnInput :: (String -> msg) -> Config msg -> Config msg
setOnInput onInput config_ =
  config_ {onInput = Just onInput}

-- | Specify a message when the user confirms a changed value inside the text
-- area
setOnChange :: (String -> msg) -> Config msg -> Config msg
setOnChange onChange config_ =
  config_ {onChange = Just onChange}

-- | Filled text area view function
filled :: Config msg -> Miso.View msg
filled config_ =
  textArea False config_

-- | Outlined text area view function
outlined :: Config msg -> Miso.View msg
outlined config_ =
  textArea True config_

textArea :: Bool -> Config msg -> Miso.View msg
textArea
  outlined_
  ( config_@Config
      { additionalAttributes = additionalAttributes,
        fullwidth = fullwidth
      }
    ) =
    Miso.nodeHtml
      "mdc-text-field"
      ( Maybe.mapMaybe
          id
          [ rootCs,
            noLabelCs config_,
            outlinedCs outlined_,
            fullwidthCs config_,
            disabledCs config_,
            valueProp config_,
            disabledProp config_,
            requiredProp config_,
            validProp config_,
            minLengthProp config_,
            maxLengthProp config_
          ]
          ++ additionalAttributes
      )
      [ inputElt config_,
        notchedOutlineElt config_
      ]

rootCs :: Maybe (Miso.Attribute msg)
rootCs =
  Just (Miso.class_ "mdc-text-field mdc-text-field--textarea")

outlinedCs :: Bool -> Maybe (Miso.Attribute msg)
outlinedCs outlined_ =
  if outlined_
    then Just (Miso.class_ "mdc-text-field--outlined")
    else Nothing

fullwidthCs :: Config msg -> Maybe (Miso.Attribute msg)
fullwidthCs (Config {fullwidth = fullwidth}) =
  if fullwidth
    then Just (Miso.class_ "mdc-text-field--fullwidth")
    else Nothing

disabledCs :: Config msg -> Maybe (Miso.Attribute msg)
disabledCs (Config {disabled = disabled}) =
  if disabled
    then Just (Miso.class_ "mdc-text-field--disabled")
    else Nothing

requiredProp :: Config msg -> Maybe (Miso.Attribute msg)
requiredProp (Config {required = required}) =
  Just (Miso.boolProp "required" required)

validProp :: Config msg -> Maybe (Miso.Attribute msg)
validProp (Config {valid = valid}) =
  Just (Miso.boolProp "valid" valid)

minLengthProp :: Config msg -> Maybe (Miso.Attribute msg)
minLengthProp (Config {minLength = minLength}) =
  Just
    ( Miso.intProp "minLength" (Maybe.fromMaybe (-1) minLength)
    )

maxLengthProp :: Config msg -> Maybe (Miso.Attribute msg)
maxLengthProp (Config {maxLength = maxLength}) =
  Just
    ( Miso.intProp "maxLength" (Maybe.fromMaybe (-1) maxLength)
    )

minLengthAttr :: Config msg -> Maybe (Miso.Attribute msg)
minLengthAttr (Config {minLength = minLength}) =
  fmap (Miso.intProp "minLength") minLength

maxLengthAttr :: Config msg -> Maybe (Miso.Attribute msg)
maxLengthAttr (Config {maxLength = maxLength}) =
  fmap (Miso.intProp "maxLength") maxLength

valueProp :: Config msg -> Maybe (Miso.Attribute msg)
valueProp (Config {value = value}) =
  fmap (Miso.textProp "value" . ms) value

placeholderAttr :: Config msg -> Maybe (Miso.Attribute msg)
placeholderAttr (Config {placeholder = placeholder}) =
  fmap (Miso.placeholder_ . ms) placeholder

inputHandler :: Config msg -> Maybe (Miso.Attribute msg)
inputHandler (Config {onInput = onInput}) =
  Maybe.maybe
    Nothing
    (\f -> Just (Miso.Html.Event.onInput (\s -> f (Miso.String.fromMisoString s))))
    onInput

changeHandler :: Config msg -> Maybe (Miso.Attribute msg)
changeHandler (Config {onChange = onChange}) =
  Maybe.maybe
    Nothing
    (\f -> Just (Miso.Html.Event.onChange (\s -> f (Miso.String.fromMisoString s))))
    onChange

inputElt :: Config msg -> Miso.View msg
inputElt config_ =
  Miso.textarea_
    ( Maybe.mapMaybe
        id
        [ inputCs,
          ariaLabelAttr config_,
          rowsAttr config_,
          colsAttr config_,
          placeholderAttr config_,
          inputHandler config_,
          changeHandler config_,
          minLengthAttr config_,
          maxLengthAttr config_
        ]
    )
    []

inputCs :: Maybe (Miso.Attribute msg)
inputCs =
  Just (Miso.class_ "mdc-text-field__input")

rowsAttr :: Config msg -> Maybe (Miso.Attribute msg)
rowsAttr (Config {rows = rows}) =
  fmap (Miso.rows_ . ms . show) rows

colsAttr :: Config msg -> Maybe (Miso.Attribute msg)
colsAttr (Config {cols = cols}) =
  fmap (Miso.cols_ . ms . show) cols

ariaLabelAttr :: Config msg -> Maybe (Miso.Attribute msg)
ariaLabelAttr
  Config
    { fullwidth = fullwidth,
      placeholder = placeholder,
      label = label
    } =
    if fullwidth
      then fmap (Miso.textProp "aria-label" . ms) label
      else Nothing

disabledProp :: Config msg -> Maybe (Miso.Attribute msg)
disabledProp (Config {disabled = disabled}) =
  Just (Miso.boolProp "disabled" disabled)

labelElt :: Config msg -> Miso.View msg
labelElt (Config {label = label, value = value}) =
  let floatingLabelCs =
        "mdc-floating-label"

      floatingLabelFloatAboveCs =
        "mdc-floating-label--float-above"
   in case label of
        Just str ->
          Miso.span_
            [ if Maybe.fromMaybe "" value /= ""
                then
                  Miso.class_
                    . ms
                    $ floatingLabelCs ++ " " ++ floatingLabelFloatAboveCs
                else
                  Miso.class_ $
                    ms floatingLabelCs,
              Miso.textProp
                "foucClassNames"
                . ms
                $ Data.Aeson.encode [floatingLabelFloatAboveCs]
            ]
            [Miso.text $ ms str]
        Nothing ->
          Miso.text ""

noLabelCs :: Config msg -> Maybe (Miso.Attribute msg)
noLabelCs (Config {label = label}) =
  if label == Nothing
    then Just (Miso.class_ "mdc-text-field--no-label")
    else Nothing

notchedOutlineElt :: Config msg -> Miso.View msg
notchedOutlineElt config_ =
  Miso.div_
    [Miso.class_ "mdc-notched-outline"]
    [ notchedOutlineLeadingElt,
      notchedOutlineNotchElt config_,
      notchedOutlineTrailingElt
    ]

notchedOutlineLeadingElt :: Miso.View msg
notchedOutlineLeadingElt =
  Miso.div_ [Miso.class_ "mdc-notched-outline__leading"] []

notchedOutlineTrailingElt :: Miso.View msg
notchedOutlineTrailingElt =
  Miso.div_ [Miso.class_ "mdc-notched-outline__trailing"] []

notchedOutlineNotchElt :: Config msg -> Miso.View msg
notchedOutlineNotchElt config_ =
  Miso.div_ [Miso.class_ "mdc-notched-outline__notch"] [labelElt config_]
