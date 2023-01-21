module Text.PrettyPrint.GenericPretty.Util
  ( inspect,
    inspectPlain,
    inspectStyle,
  )
where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.Pretty.Simple as PrettySimple
import qualified Text.PrettyPrint as Pretty
import Text.PrettyPrint.GenericPretty (Out)
import qualified Text.PrettyPrint.GenericPretty as GenericPretty
import Universum hiding (show)

inspect :: forall txt src. (Out src, IsString txt) => src -> txt
inspect =
  inspectStyle
    PrettySimple.defaultOutputOptionsDarkBg

inspectPlain :: forall txt src. (Out src, IsString txt) => src -> txt
inspectPlain =
  fromString
    . T.unpack
    . T.replace "\n" ""
    . inspectStyle
      PrettySimple.defaultOutputOptionsNoColor
        { PrettySimple.outputOptionsPageWidth = 100000,
          PrettySimple.outputOptionsCompact = True,
          PrettySimple.outputOptionsCompactParens = True
        }

inspectStyle ::
  forall txt src.
  ( Out src,
    IsString txt
  ) =>
  PrettySimple.OutputOptions ->
  src ->
  txt
inspectStyle style =
  fromString
    . TL.unpack
    . PrettySimple.pStringOpt style
    . GenericPretty.prettyStyle
      Pretty.style
        { Pretty.mode = Pretty.OneLineMode
        }
