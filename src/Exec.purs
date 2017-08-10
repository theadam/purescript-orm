module Exec where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (kind Effect)
import Data.Foreign (Foreign)
import Data.String (joinWith)
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
  , logSQL :: Boolean
  , logResults :: Boolean
  )

exec
 :: forall fx
  . Record Config
 -> Pool
 -> Runner fx
exec conf pool sql params = unsafeCoerceAff $ do
  case conf.logSQL of
       true -> logSQL
       false -> pure unit

  res <- P.withConnection pool run

  case conf.logResults of
       true -> logResults res
       false -> pure unit

  pure res
    where
      run conn = P.unsafeQuery conn sql params
      logSQL = log $ "Executing SQL: " <> sqlString sql params
      sqlString s [] = s
      sqlString s a = s <> " With Params " <> stringifyParams a
      logResults [] = log $ "No Return value."
      logResults res = log $ "Return Values SQL: " <> joinWith ", " (toRes <$> res)
      toRes s = "(" <> stringifyParams s <> ")"

type Runner fx = BaseRunner (Orm fx)
type BaseRunner fx = String -> Array Foreign -> Aff fx (Array (Array Foreign))

makeRunner :: forall fx. Record Config -> Runner fx
makeRunner conf sql args = unsafeCoerceAff $ do
  let withoutLogSQL = R.delete (SProxy :: SProxy "logSQL") conf
  let poolConf = R.delete (SProxy :: SProxy "logResults") withoutLogSQL
  pool <- newPool poolConf
  exec conf pool sql args

foreign import stringifyParams :: Array Foreign -> String

