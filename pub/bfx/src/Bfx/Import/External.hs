{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Import.External
  ( module X,
  )
where

import Bfx.Import.Witch as X
import Control.Concurrent as X (threadDelay)
import Control.Monad.Extra as X
  ( maybeM,
  )
import Control.Monad.Trans.Except as X
  ( catchE,
    except,
    throwE,
  )
import Data.Aeson as X
  ( FromJSON (..),
    FromJSONKey (..),
    ToJSON (..),
  )
import Data.ByteString.Lazy as X (ByteString)
import Data.Coerce as X (coerce)
import Data.Fixed as X (Fixed, HasResolution (..), showFixed)
import Data.List.Extra as X (notNull)
import Data.Maybe as X (listToMaybe)
import Data.Metrology.Poly as X
  ( quOf,
    (*|),
    (/|),
    (:/),
    (|*),
    (|*|),
    (|+|),
    (|-|),
    (|/),
    (|/|),
  )
import Data.Ratio as X ((%))
import Data.Ratio.Rounding as X (dpRound, sdRound)
import Data.Singletons as X
import Data.Time.Clock as X
  ( DiffTime,
    UTCTime (..),
    addUTCTime,
    diffTimeToPicoseconds,
    diffUTCTime,
    getCurrentTime,
    nominalDay,
    secondsToDiffTime,
  )
import Data.Time.Clock.POSIX as X (posixSecondsToUTCTime)
import Data.Type.Equality as X
  ( TestEquality (..),
    (:~:) (..),
    type (==),
  )
import Database.Persist as X
  ( PersistField (..),
    PersistValue (..),
    SqlType (..),
  )
import Database.Persist.Sql as X
  ( PersistFieldSql (..),
  )
import Katip.Format.Time as X
  ( formatAsLogTime,
  )
import Network.HTTP.Client as X (HttpException (..))
import System.Exit as X
  ( ExitCode (..),
  )
import System.IO.Temp as X (withSystemTempFile)
import Text.PrettyPrint.GenericPretty as X (Out (..))
import Text.PrettyPrint.GenericPretty.Import as X
  ( inspect,
    inspectPlain,
  )
import Universum as X hiding
  ( ByteString,
    Last (..),
    bracket,
    catch,
    finally,
    (^?),
  )
import UnliftIO as X
  ( Handler (..),
    MonadUnliftIO (..),
    UnliftIO (..),
    bracket,
    catch,
    catches,
    finally,
  )
import UnliftIO.Directory as X
  ( copyFile,
    removeFile,
  )
