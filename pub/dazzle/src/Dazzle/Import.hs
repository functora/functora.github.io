{-# OPTIONS_HADDOCK show-extensions #-}

module Dazzle.Import (module X) where

import Control.Exception as X (IOException)
import Control.Monad.Extra as X (maybeM)
import Control.Monad.Trans.Except as X
  ( catchE,
    except,
    throwE,
    withExceptT,
  )
import Data.Coerce as X (coerce)
import Data.Either.Extra as X (fromEither)
import Data.Fixed as X (Pico)
import Data.List as X (partition, singleton)
import Data.List.Extra as X (nubOrd)
import Data.Ratio as X ((%))
import Data.Text as X
  ( intercalate,
    isPrefixOf,
  )
import Data.Text.IO.Utf8 as X (readFile)
import Data.Type.Equality as X
  ( TestEquality (..),
    (:~:) (..),
    type (==),
  )
import Lens.Micro as X
  ( (^.),
    (^..),
    (^?),
  )
import Main.Utf8 as X (withUtf8)
import System.Exit as X
  ( ExitCode (..),
  )
import System.Process as X
  ( CreateProcess (..),
    proc,
    readCreateProcessWithExitCode,
  )
import Text.PrettyPrint.GenericPretty.Import as X
  ( Out (..),
    inspect,
    inspectPlain,
  )
import Text.Show.Unicode as X
  ( ushow,
  )
import Universum as X hiding
  ( atomically,
    bracket,
    intercalate,
    isPrefixOf,
    on,
    over,
    readFile,
    set,
    (^.),
    (^..),
    (^?),
  )
import UnliftIO as X
  ( MonadUnliftIO,
    UnliftIO (..),
    bracket,
    withRunInIO,
    withUnliftIO,
  )
import Witch as X
