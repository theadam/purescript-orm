module Connection where

import Prelude

import Control.Monad.Aff (Aff, delay, forkAff)
import Control.Monad.Aff.AVar (AVar, makeVar, peekVar, putVar, takeVar)
import Control.Monad.Aff.Console (log)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (kind Effect)
import Data.Foreign (Foreign)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Database.PostgreSQL (Pool, newPool)
import Database.PostgreSQL as P
import Record as R

type Defaults =
  ( user :: String
  , password :: String
  , host :: String
  , port :: Int
  , max :: Int
  , idleTimeoutMillis :: Int
  , logSql :: Boolean
  )

type Config = { database :: String | Defaults }


defaults :: Record Defaults
defaults =
  { user: "postgres"
  , password: "postgres"
  , host: "127.0.0.1"
  , port: 5432
  , max: 10
  , idleTimeoutMillis: 1000
  , logSql: false
  }

foreign import data ORM :: Effect

class Configable (r :: # Type) where
  createConfig :: Record r -> Config
instance recordConfig
  :: ( Union r f ( database :: String | Defaults )
     , Union f a Defaults
     ) => Configable r where
        createConfig r = R.merge r (R.subrecord defaults)

poolVar :: forall fx. Aff (orm :: ORM | fx) (AVar Pool)
poolVar = unsafeCoerceAff makeVar

logVar :: forall fx. Aff (orm :: ORM | fx) (AVar Boolean)
logVar = unsafeCoerceAff makeVar

connect :: forall r fx. Configable r => Record r -> Aff ( orm :: ORM | fx ) Unit
connect r = unsafeCoerceAff $ do
  let config = createConfig r
  pool <- newPool (R.delete (SProxy :: SProxy "logSql") config)
  var <- poolVar
  putVar var pool

  shouldLog <- logVar
  putVar shouldLog config.logSql

withConfig :: forall fx r a. Configable r => Record r -> Aff ( orm :: ORM | fx ) a -> Aff (orm :: ORM | fx) Unit
withConfig config action = do
  _ <- connect config
  _ <- action
  pure unit

query :: forall fx. String -> Array Foreign -> Aff (orm :: ORM | fx) (Array (Array Foreign))
query sql params = unsafeCoerceAff $ do
  var <- poolVar
  pool <- peekVar var

  log' <- logVar
  shouldLog <- peekVar log'

  case shouldLog of
       true -> logSql
       false -> pure unit

  P.withConnection pool run
    where
      run conn = P.unsafeQuery conn sql params
      logSql = log $ "Executing SQL: " <> sqlString sql params
      sqlString s [] = s
      sqlString s a = s <> " With Params " <> stringifyParams a

foreign import stringifyParams :: Array Foreign -> String


