module Exec where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (kind Effect)
import Data.Foreign (Foreign)
import Data.Symbol (SProxy(..))
import Database.PostgreSQL (Pool, newPool)
import Database.PostgreSQL as P
import Record as R

foreign import data ORM :: Effect

type Orm fx = (orm :: ORM | fx)
type AffOrm fx a = Aff (Orm fx) a

type Config =
  ( database :: String
  , user :: String
  , password :: String
  , host :: String
  , port :: Int
  , max :: Int
  , idleTimeoutMillis :: Int
  , logSql :: Boolean
  )

exec
 :: forall fx
  . Record Config
 -> Pool
 -> Runner fx
exec conf pool sql params = unsafeCoerceAff $ do
  let shouldLog = conf.logSql

  case shouldLog of
       true -> logSql
       false -> pure unit

  P.withConnection pool run
    where
      run conn = P.unsafeQuery conn sql params
      logSql = log $ "Executing SQL: " <> sqlString sql params
      sqlString s [] = s
      sqlString s a = s <> " With Params " <> stringifyParams a

type Runner fx = BaseRunner (Orm fx)
type BaseRunner fx = String -> Array Foreign -> Aff fx (Array (Array Foreign))

makeRunner :: forall fx. Record Config -> Runner fx
makeRunner conf sql args = unsafeCoerceAff $ do
  pool <- newPool (R.delete (SProxy :: SProxy "logSql") conf)
  exec conf pool sql args

foreign import stringifyParams :: Array Foreign -> String

