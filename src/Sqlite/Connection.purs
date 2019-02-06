module Sqlite.Connection where

import Prelude

import Connection (Operation(..), class MonadConnection, class MonadQuerier, runOperation)
import Control.Lazy (defer)
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
import ParameterizedSql (ParameterizedSql, commaJoin, questionMarkToParam, realize, sqlTupleToString, toSql, valueToForeign, (:<>), Value(..))
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

convertColumnDefinition :: ColumnDefinition -> String
convertColumnDefinition Id = "INTEGER PRIMARY KEY AUTOINCREMENT"
convertColumnDefinition cd = Convert.makeConvertColumnDefinition (defer \_ -> convertColumnDefinition) cd

baseConvert :: Operation -> ParameterizedSql
baseConvert = Convert.makeConvert convertColumnDefinition

-- Convert Section
convert :: Operation -> ParameterizedSql
convert (Truncate name) = toSql "DELETE FROM " :<> name
convert ins@(Insert name intos values)
  | length values > 1 = baseConvert ins
  | otherwise = case head values of
    Just h -> baseConvert $ Insert name (filteredIntos h) [(filteredValues h)]
    Nothing -> baseConvert ins
      where
        zipped vs = zip intos vs
        keep (Tuple into NullValue) = false
        keep a = true
        filtered h = filter keep (zipped h)
        unzipped h = unzip $ filtered h
        filteredValues h = snd $ unzipped h
        filteredIntos h = fst $ unzipped h

convert op = baseConvert op

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
lastInsertedRow name intos = toSql "SELECT " :<> commaJoin (mapWithIndex mapAliases intos):<> " FROM " :<> name :<> " WHERE rowid = last_insert_rowid()"
  where
    mapAliases i a = a <> " as '" <> show i <> "'"

instance monadQuerierSqlite :: MonadQuerier Sqlite where
  runOperation (Insert name intos values) = do
    let inserts = (\value -> Insert name intos [value]) <$> values
    for inserts \ins -> do
      _ <- runQuery $ convert ins
      res <- runQuery $ lastInsertedRow name intos
      case head res of
        Just a -> pure a
        Nothing -> pure []
  runOperation op = runQuery $ convert op

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
