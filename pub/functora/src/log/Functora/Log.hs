{-# LANGUAGE TemplateHaskell #-}

module Functora.Log
  ( LogItem,
    logItemLoc,
    logItemSev,
    logItemMsg,
    Log,
    unLog,
    mkLog,
    mkLeft,
    mkLeftT,
    mkEitherRequired,
    mkEitherRequiredT,
    mkEitherRequiredLift,
    mkThis,
    mkThisT,
    mkTheseRequired,
    mkTheseRequiredT,
    catchThis,
    catchThisT,
    logKatip,
    module X,
  )
where

import qualified Data.Text as T
import Functora.LogOrphan ()
import Functora.Prelude
import Functora.PreludeOrphan ()
import Katip as X
  ( ColorStrategy (..),
    Environment (..),
    Katip (..),
    KatipContext (..),
    KatipContextT,
    LogContexts,
    LogEnv,
    LogStr (..),
    Namespace,
    Severity (..),
    Verbosity (..),
    bracketFormat,
    closeScribes,
    defaultScribeSettings,
    initLogEnv,
    jsonFormat,
    katipAddContext,
    logStr,
    logTM,
    mkHandleScribeWithFormatter,
    permitItem,
    registerScribe,
    runKatipContextT,
    sl,
  )
import qualified Katip
import qualified Katip.Core as Katip
import qualified Language.Haskell.TH.Syntax as TH
import qualified Prelude

data LogItem = LogItem
  { _logItemLoc :: TH.Loc,
    _logItemSev :: Severity,
    _logItemMsg :: Text
  }
  deriving stock (Eq, Ord, Data, Generic)

makeLenses ''LogItem

instance Show LogItem where
  show (LogItem loc sev msg) =
    from @Text @String
      $ Katip.renderSeverity sev
      <> " "
      <> from @String @Text (Katip.locationToString loc)
      <> " "
      <> msg

newtype Log = Log
  { _unLog :: [LogItem]
  }
  deriving newtype (Eq, Ord, Semigroup, Monoid)
  deriving stock (Data, Generic)

makeLenses ''Log

instance Show Log where
  show =
    from @Text @String
      . T.intercalate " | "
      . fmap (inspect @Text)
      . view unLog

instance Exception Log

newLog :: (Show a, Data a) => TH.Loc -> Severity -> a -> Log
newLog loc sev msg =
  Log
    [ LogItem
        { _logItemLoc = loc,
          _logItemSev = sev,
          _logItemMsg = inspect msg
        }
    ]

newLeft :: (Show a, Data a) => TH.Loc -> a -> Either Log b
newLeft loc = Left . newLog loc ErrorS

newLeftT :: (Show a, Data a, Monad m) => TH.Loc -> a -> ExceptT Log m b
newLeftT loc = except . newLeft loc

newEitherRequired :: (Show a, Data a) => TH.Loc -> a -> Maybe b -> Either Log b
newEitherRequired loc msg = maybe (newLeft loc msg) Right

newEitherRequiredT ::
  (Show a, Data a, Monad m) => TH.Loc -> a -> Maybe b -> ExceptT Log m b
newEitherRequiredT loc msg = except . newEitherRequired loc msg

newEitherRequiredLift ::
  (Show a, Data a, Monad m) => TH.Loc -> a -> m (Maybe b) -> ExceptT Log m b
newEitherRequiredLift loc msg = newEitherRequiredT loc msg <=< lift

newThis :: (Show a, Data a) => TH.Loc -> a -> These Log b
newThis loc = This . newLog loc ErrorS

newThisT :: (Show a, Data a, Monad m) => TH.Loc -> a -> ChronicleT Log m b
newThisT loc = chronicle . newThis loc

newTheseRequired :: (Show a, Data a) => TH.Loc -> a -> Maybe b -> These Log b
newTheseRequired loc msg = maybe (newThis loc msg) That

newTheseRequiredT ::
  (Show a, Data a, Monad m) => TH.Loc -> a -> Maybe b -> ChronicleT Log m b
newTheseRequiredT loc msg = chronicle . newTheseRequired loc msg

mkLog :: TH.Q TH.Exp
mkLog = do
  loc <- TH.location
  [|newLog $(TH.lift loc)|]

mkLeft :: TH.Q TH.Exp
mkLeft = do
  loc <- TH.location
  [|newLeft $(TH.lift loc)|]

mkLeftT :: TH.Q TH.Exp
mkLeftT = do
  loc <- TH.location
  [|newLeftT $(TH.lift loc)|]

mkEitherRequired :: TH.Q TH.Exp
mkEitherRequired = do
  loc <- TH.location
  [|newEitherRequired $(TH.lift loc)|]

mkEitherRequiredT :: TH.Q TH.Exp
mkEitherRequiredT = do
  loc <- TH.location
  [|newEitherRequiredT $(TH.lift loc)|]

mkEitherRequiredLift :: TH.Q TH.Exp
mkEitherRequiredLift = do
  loc <- TH.location
  [|newEitherRequiredLift $(TH.lift loc)|]

mkThis :: TH.Q TH.Exp
mkThis = do
  loc <- TH.location
  [|newThis $(TH.lift loc)|]

mkThisT :: TH.Q TH.Exp
mkThisT = do
  loc <- TH.location
  [|newThisT $(TH.lift loc)|]

mkTheseRequired :: TH.Q TH.Exp
mkTheseRequired = do
  loc <- TH.location
  [|newTheseRequired $(TH.lift loc)|]

mkTheseRequiredT :: TH.Q TH.Exp
mkTheseRequiredT = do
  loc <- TH.location
  [|newTheseRequiredT $(TH.lift loc)|]

catchThis :: a -> These e a -> These e a
catchThis def = \case
  This e -> These e def
  x -> x

catchThisT ::
  (Semigroup e, Monad m) => a -> ChronicleT e m a -> ChronicleT e m a
catchThisT def expr = do
  res <- lift $ runChronicleT expr
  chronicle $ case res of
    This e -> These e def
    x -> x

logKatip :: (KatipContext m) => Log -> m ()
logKatip log =
  forM_ (view unLog log) $ \x ->
    Katip.logItemM
      (Just $ view logItemLoc x)
      (view logItemSev x)
      (logStr $ view logItemMsg x)
