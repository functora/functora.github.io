module Functora.Sql
  ( module X,
    (^:),
  )
where

import Control.Monad.Logger as X
  ( defaultOutput,
    runLoggingT,
    runNoLoggingT,
  )
import Data.Pool as X
  ( Pool,
    destroyAllResources,
  )
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
import Functora.SqlOrphan as X ()
import GHC.IO.Handle.FD as X (stderr, stdout)

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
