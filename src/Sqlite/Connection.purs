module Sqlite.Connection where

import Prelude

import Connection (class MonadConnection, class MonadConverter, class MonadQuerier, Operation(..), runOperation, convertOperation)
import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT, ask)
import Convert as Convert
import Data.Array (mapWithIndex, head, length, zip, filter, unzip)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (for)
import Data.Tuple (fst, snd, Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Foreign (Foreign)
import ParameterizedSql (ParameterizedSql, Value(..), commaJoin, questionMarkToParam, realize, sqlTupleToString, toSql, valueToForeign, (:<>:))
import SQLite3 (DBConnection, closeDB, newDB, queryDB)
import TableDefinition (ColumnDefinition(..))
import Utils (stringify)

-- Env Section
type Config =
  { filePath :: String
  , logSql :: Boolean
  , logResults :: Boolean
  }

defaultConfig :: Config
defaultConfig =
  { filePath: "db"
  , logSql: false
  , logResults: false
  }

newtype Env = Env { db :: DBConnection, logSql :: Boolean, logResults :: Boolean }

derive instance newtypeEnv :: Newtype Env _

makeConnection :: Config -> Aff Env
makeConnection c = do
  db <- newDB c.filePath
  pure $ Env { db: db, logSql: c.logSql, logResults: c.logResults }

-- Connection
newtype Sqlite a = Sqlite (ReaderT Env Aff a)

derive newtype instance functorSqlite :: Functor Sqlite
derive newtype instance applySqlite :: Apply Sqlite
derive newtype instance applicativeSqlite :: Applicative Sqlite
derive newtype instance bindSqlite :: Bind Sqlite
derive newtype instance monadSqlite :: Monad Sqlite
derive newtype instance monadAskSqlite :: MonadAsk Env Sqlite
derive newtype instance monadEffectSqlite :: MonadEffect Sqlite
derive newtype instance monadThrowSqlite :: MonadThrow Error Sqlite
derive newtype instance monadErrorSqlite :: MonadError Error Sqlite

foreign import arrayifyResult :: Foreign -> Array (Array Foreign)

runQuery :: ParameterizedSql -> Sqlite (Array (Array Foreign))
runQuery sql = do
  (Env env) <- ask
  case env.logSql of
    true -> log $ "\n" <> sqlTupleToString realized
    false -> pure unit

  let res = Sqlite $ lift $ queryDB env.db query params
  r <- res
  let arrayified = arrayifyResult r

  case env.logResults of
    true -> log $ "RESULTS: " <> stringify arrayified <> "\n"
    false -> pure unit

  pure $ arrayified
    where
      realized = realize questionMarkToParam sql
      query = fst realized
      params = valueToForeign <$> snd realized

lastInsertedRow :: String -> Array String -> ParameterizedSql
lastInsertedRow name intos = toSql "SELECT " :<>: commaJoin (mapWithIndex mapAliases intos) :<>: " FROM " :<>: name :<>: " " :<>: tableAlias <> " WHERE rowid = last_insert_rowid()"
  where
    mapAliases i a = tableAlias <> "." <> a <> " as '" <> show i <> "'"
    tableAlias = name <> "1"


instance monadConvertSqlite :: MonadConverter Sqlite where
  convertColumnDefinition Id = pure $ "INTEGER PRIMARY KEY AUTOINCREMENT"
  convertColumnDefinition cd = Convert.convertColumnDefinition cd

  convertOperation (Truncate name) = pure $ toSql "DELETE FROM " :<>: name
  convertOperation ins@(Insert name intos values ret)
    | length values > 1 = Convert.convertOperation ins
    | otherwise = case head values of
      Just h -> Convert.convertOperation $ Insert name (filteredIntos h) [(filteredValues h)] ret
      Nothing -> Convert.convertOperation ins
        where
          zipped vs = zip intos vs
          keep (Tuple into NullValue) = false
          keep a = true
          filtered h = filter keep (zipped h)
          unzipped h = unzip $ filtered h
          filteredValues h = snd $ unzipped h
          filteredIntos h = fst $ unzipped h
  convertOperation (Select selects state) = Convert.convertOperation (Select newSelects state)
    where
      newSelects = mapWithIndex makeAliased selects
      makeAliased i sel = sel :<>: " as '" :<>: show i :<>: "'"

  convertOperation op = Convert.convertOperation op



instance monadQuerierSqlite :: MonadQuerier Sqlite where
  runOperation (Insert name intos values returnResults) = do
    let inserts = (\value -> Insert name intos [value] returnResults) <$> values
    for inserts \ins -> do
      _ <- convertOperation ins >>= runQuery
      case returnResults of
        false -> pure []
        true -> do
          res <- runQuery $ lastInsertedRow name intos
          case head res of
            Just a -> pure a
            Nothing -> pure []
  runOperation op = convertOperation op >>= runQuery

  runCommand op = do
    log "Warning: Cannot run commands with this SQLite driver.  All Commands return 0."
    _ <- runOperation op
    pure 0

  withTransaction m = do
    log "Warning: Transactions are not implemented with this SQLite driver. Running SQL directly."
    m

instance monadConnectionSqlite :: MonadConnection Env Sqlite where
  withConnection env (Sqlite m) = do
    res <- runReaderT m env
    closeDB (unwrap env).db
    pure res
