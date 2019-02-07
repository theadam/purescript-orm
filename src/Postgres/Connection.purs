module Postgres.Connection where

import Prelude

import Connection (class MonadConnection, class MonadConverter, class MonadQuerier, Operation(..), convertOperation)
import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Convert as Convert
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (fst, snd)
import Database.PostgreSQL (Connection, Pool, Query(..), command, newPool, unsafeQuery)
import Database.PostgreSQL as P
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Foreign (Foreign)
import ParameterizedSql (ParameterizedSql(..), SqlPart(..), Value(..), commaJoin, mapParts, positionalToParam, realize, sqlTupleToString, (:<>:), valueToForeign, toSql)
import Utils (stringify)

-- Env Section
type Config =
  { database :: String
  , user :: Maybe String
  , password :: Maybe String
  , host :: Maybe String
  , port :: Maybe Int
  , maxClientCount :: Maybe Int
  , idleTimeoutMillis :: Maybe Int
  , logSql :: Boolean
  , logResults :: Boolean
  }

defaultConfig :: Config
defaultConfig =
  { database: "postgres"
  , user: Just "postgres"
  , password: Nothing
  , host: Nothing
  , port: Nothing
  , maxClientCount: Nothing
  , idleTimeoutMillis: Just 100
  , logSql: false
  , logResults: false
  }

newtype Env = Env { pool :: Pool, logSql :: Boolean, logResults :: Boolean }

derive instance newtypeEnv :: Newtype Env _

makeConnection :: Config -> Aff Env
makeConnection c = do
  pool <- newPool
    { database: c.database
    , user: c.user
    , password: c.password
    , host: c.host
    , port: c.port
    , max: c.maxClientCount
    , idleTimeoutMillis: c.idleTimeoutMillis
    }
  pure $ Env { pool: pool, logSql: c.logSql, logResults: c.logResults }


-- Convert Section
nullToDefault :: SqlPart -> SqlPart
nullToDefault (Param NullValue) = StringPart "DEFAULT"
nullToDefault a = a

-- Connection
newtype Postgres a = Postgres (ReaderT Env Aff a)

derive newtype instance functorPostgres :: Functor Postgres
derive newtype instance applyPostgres :: Apply Postgres
derive newtype instance applicativePostgres :: Applicative Postgres
derive newtype instance bindPostgres :: Bind Postgres
derive newtype instance monadPostgres :: Monad Postgres
derive newtype instance monadAskPostgres :: MonadAsk Env Postgres
derive newtype instance monadEffectPostgres :: MonadEffect Postgres
derive newtype instance monadThrowPostgres :: MonadThrow Error Postgres
derive newtype instance monadErrorPostgres :: MonadError Error Postgres

runWith :: forall a. (Connection -> String -> Array Foreign -> Aff a) -> ParameterizedSql -> Postgres a
runWith runner sql = do
  (Env env) <- ask

  case env.logSql of
    true -> log $ "\n" <> sqlTupleToString realized
    false -> pure unit

  r <- Postgres $ lift $ P.withConnection env.pool (\conn -> runner conn query params)

  case env.logResults of
    true -> log $ "RESULTS: " <> stringify r <> "\n"
    false -> pure unit

  pure r
    where
      realized = realize positionalToParam sql
      query = fst realized
      params = valueToForeign <$> snd realized

instance monadConverterPostgres :: MonadConverter Postgres where
  convertColumnDefinition cd = Convert.convertColumnDefinition cd
  convertOperation ins@(Insert name intos values returnResults) = do
    orig <- Convert.convertOperation ins
    pure $ mapParts nullToDefault orig :<>: ret returnResults
      where
        ret true = toSql " RETURNING " :<>: commaJoin intos
        ret false = ParameterizedSql []
  convertOperation op = Convert.convertOperation op

instance monadQuerierPostgres :: MonadQuerier Postgres where
  runOperation op = convertOperation op >>= runWith unsafeQuery
  runCommand op = convertOperation op >>= runWith (\conn s -> command conn (Query s))

  withTransaction (Postgres m) = do
    wrapped <- ask
    let (Env env) = wrapped
    let aff = runReaderT m wrapped
    Postgres $ lift $ P.withConnection env.pool (\c -> P.withTransaction c aff)

instance monadConnectionPostgres :: MonadConnection Env Postgres where
  withConnection env (Postgres m) = runReaderT m env
