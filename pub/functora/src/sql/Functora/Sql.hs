module Functora.Sql
  ( module X,
    upsertBy,
    selectOneRequired,
    runMigrationPool,
    PersistNat (..),
    (^:),
  )
where

import Control.Monad.Logger as X
  ( LoggingT (..),
    MonadLoggerIO,
    NoLoggingT (..),
    defaultOutput,
    runLoggingT,
    runNoLoggingT,
    runStdoutLoggingT,
  )
import Data.Pool as X (Pool, destroyAllResources)
import Database.Esqueleto.Experimental as X
  ( putMany,
    selectSource,
  )
import qualified Database.Esqueleto.Internal.Internal as Internal
import Database.Esqueleto.Legacy as X
  ( BaseBackend,
    Entity (..),
    FullOuterJoin (..),
    InnerJoin (..),
    Key,
    LeftOuterJoin (..),
    LockingKind (..),
    PersistEntity (..),
    PersistField (..),
    PersistFieldSql (..),
    PersistValue (..),
    RawSql (..),
    RightOuterJoin (..),
    Single (..),
    SqlBackend,
    SqlExpr,
    SqlPersistT,
    SqlType (..),
    ToBackendKey,
    Unique,
    Value (..),
    addMigration,
    asc,
    count,
    countRows,
    delete,
    deleteKey,
    desc,
    distinct,
    distinctOn,
    don,
    from,
    getBy,
    groupBy,
    in_,
    insertBy,
    insertEntity,
    insertUniqueEntity,
    isNothing,
    just,
    limit,
    locking,
    max_,
    min_,
    notIn,
    not_,
    nothing,
    offset,
    on,
    orderBy,
    rawExecute,
    rawSql,
    runMigration,
    runSqlPool,
    select,
    selectFirst,
    selectOne,
    set,
    transactionUndo,
    update,
    updateCount,
    val,
    valList,
    where_,
    (!=.),
    (&&.),
    (+=.),
    (/=.),
    (<.),
    (<=.),
    (=.),
    (==.),
    (>.),
    (>=.),
    (?.),
    (||.),
  )
import qualified Database.Esqueleto.Legacy as Legacy
import Database.Persist as X
  ( LiteralType (..),
  )
import qualified Database.Persist as Persist
import Database.Persist.Class as X
  ( BackendKey,
  )
import Database.Persist.Sql as X
  ( fromSqlKey,
    runSqlConn,
    toSqlKey,
  )
import Database.Persist.Sql.Migration as X
  ( runMigrationQuiet,
  )
import qualified Database.Persist.Sql.Types.Internal as Sql
import Database.Persist.TH as X
  ( derivePersistField,
    mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import Functora.Prelude
  ( Data,
    Eq,
    Generic,
    HasCallStack,
    Int64,
    Maybe (..),
    MonadIO,
    MonadThrow,
    MonadUnliftIO,
    Natural,
    NonEmpty,
    Ord,
    Read,
    ReaderT,
    Show,
    Text,
    Type,
    Typeable,
    bimap,
    either,
    flip,
    impureThrow,
    inspect,
    inspectType,
    liftIO,
    maybeM,
    pure,
    runReaderT,
    throwString,
    toList,
    tryFrom,
    ($),
    (.),
    (<>),
    (==),
  )
import Functora.SqlOrphan as X ()
import GHC.IO.Handle.FD as X (stderr, stdout)

-- | NOTE : This is workaround. Empty updates do cause exception.
upsertBy ::
  forall record (m :: Type -> Type) backend.
  ( MonadIO m,
    Legacy.PersistUniqueWrite backend,
    Legacy.PersistRecordBackend record backend,
    Legacy.SafeToInsert record
  ) =>
  Legacy.Unique record ->
  record ->
  NonEmpty (Persist.Update record) ->
  ReaderT backend m (Entity record)
upsertBy key rec =
  Legacy.upsertBy key rec . toList

selectOneRequired ::
  forall r a m backend.
  ( MonadIO m,
    MonadThrow m,
    Internal.SqlSelect a r,
    Legacy.SqlBackendCanRead backend,
    HasCallStack,
    Typeable r
  ) =>
  Legacy.SqlQuery a ->
  ReaderT backend m r
selectOneRequired =
  maybeM
    ( throwString $
        "Required at least one row of "
          <> inspectType @r @Text
    )
    pure
    . Legacy.selectOne

-- | Runs a migration action on a pool. Exactly like 'runSqlPool', but it will
--   disable foreign keys if running on a sqlite database per the recommendation
--   in the <https://sqlite.org/lang_altertable.html#making_other_kinds_of_table_schema_changes
--   relevant sqlite documentation>.
--
--   Workaround for <https://github.com/yesodweb/persistent/issues/1125>
runMigrationPool ::
  forall m a.
  ( MonadUnliftIO m
  ) =>
  ReaderT SqlBackend m a ->
  Legacy.ConnectionPool ->
  m a
runMigrationPool r pconn =
  Legacy.runSqlPoolWithHooks r pconn Nothing before after onException
  where
    before conn = do
      let sqlBackend = Legacy.projectBackend conn
      let getter = Legacy.getStmtConn sqlBackend
      whenSqlite conn $ rawExecute "PRAGMA foreign_keys=OFF" []
      liftIO $ Sql.connBegin sqlBackend getter Nothing
    after conn = do
      let sqlBackend = Legacy.projectBackend conn
      let getter = Legacy.getStmtConn sqlBackend
      whenSqlite conn $ rawExecute "PRAGMA foreign_keys=ON" []
      liftIO $ Sql.connCommit sqlBackend getter
    onException conn _ = do
      let sqlBackend = Legacy.projectBackend conn
      let getter = Legacy.getStmtConn sqlBackend
      liftIO $ Sql.connRollback sqlBackend getter
    whenSqlite conn act
      | Sql.connRDBMS conn == "sqlite" =
          runReaderT `flip` conn $ act
    whenSqlite _ _ = pure ()

newtype PersistNat = PersistNat
  { unPersistNat :: Natural
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

instance PersistField PersistNat where
  toPersistValue =
    either impureThrow toPersistValue
      . tryFrom @Natural @Int64
      . unPersistNat
  fromPersistValue raw = do
    int <- fromPersistValue raw
    bimap inspect PersistNat $ tryFrom @Int64 @Natural int

deriving via Int64 instance PersistFieldSql PersistNat

-- | Project a field of an entity.
-- Alias exists to remove interference with Lens.
(^:) ::
  forall typ val.
  (PersistEntity val, PersistField typ) =>
  SqlExpr (Entity val) ->
  EntityField val typ ->
  SqlExpr (Value typ)
(^:) = (Legacy.^.)

infixl 9 ^:
