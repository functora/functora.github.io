module Functora.Sql
  ( module X,
    upsertBy,
    selectOneRequired,
    (^:),
  )
where

import Control.Monad.Logger as X
  ( defaultOutput,
    runLoggingT,
    runNoLoggingT,
  )
import Data.Pool as X (Pool, destroyAllResources)
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
    Value,
    asc,
    deleteKey,
    desc,
    from,
    getBy,
    in_,
    insertBy,
    insertUniqueEntity,
    isNothing,
    just,
    limit,
    locking,
    max_,
    min_,
    notIn,
    nothing,
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
    unValue,
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
import Database.Persist.TH as X
  ( derivePersistField,
    mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import Functora.Prelude
  ( HasCallStack,
    MonadIO,
    MonadThrow,
    NonEmpty,
    ReaderT,
    Text,
    Type,
    Typeable,
    inspectType,
    maybeM,
    pure,
    throwString,
    toList,
    ($),
    (.),
    (<>),
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
