module Connection where

import Prelude

import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (kind Effect)
import Create (class CreateRecord, create, createIfNotExists)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)
import Exec (AffOrm, Config, makeRunner)
import Insert (class Defaults, class InsertRecord, insert, withDefaults)
import Query (Query, query, queryOne)
import Record as R
import Select (class SelectMappable)
import Table (Table, drop, truncate)
import Type.Prelude (Proxy)

type Defaults =
  ( user :: String
  , password :: String
  , host :: String
  , port :: Int
  , max :: Int
  , idleTimeoutMillis :: Int
  , logSql :: Boolean
  )

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

class Configable (r :: # Type) where
  createConfig :: Record r -> Record Config
instance recordConfig
  :: ( Union r f Config
     , Union f a Defaults
     ) => Configable r where
        createConfig r = R.merge r (R.subrecord defaults)

data Connection fx = Connection
 { createDb
   :: AffOrm fx Unit

 , create
   :: forall n cd. IsSymbol n => CreateRecord cd => Proxy (Table n cd) -> AffOrm fx Unit
 , createIfNotExists
   :: forall n cd. IsSymbol n => CreateRecord cd => Proxy (Table n cd) -> AffOrm fx Unit

 , drop
   :: forall n cd. IsSymbol n => Proxy (Table n cd) -> AffOrm fx Unit
 , truncate
   :: forall n cd. IsSymbol n => Proxy (Table n cd) -> AffOrm fx Unit

 , insert
   :: forall n cd r i.
      IsSymbol n => InsertRecord cd r => Defaults i r =>
      Proxy (Table n cd) -> Record i -> AffOrm fx Unit

 , queryOne
   :: forall s r. Query s -> (SelectMappable s r => AffOrm fx (Maybe r))
 , query
   :: forall s r. Query s -> (SelectMappable s r => AffOrm fx (Array r))
 }

connect :: forall r fx. Configable r => Record r -> AffOrm fx (Connection fx)
connect r = unsafeCoerceAff $ do
  let config = createConfig r
  let runner = makeRunner config
  let adminRunner = makeRunner (config { database = "postgres" })
  pure $ Connection
   { createDb: adminRunner ("CREATE DATABASE " <> config.database) [] $> unit

   , create: create runner
   , createIfNotExists: createIfNotExists runner

   , drop: drop runner
   , truncate: truncate runner

   , insert: \t rec -> insert runner t (withDefaults rec)

   , queryOne: \q -> queryOne runner q
   , query: \q -> query runner q
   }

