module Main where

import Prelude

import Column (type (&), Column, Default, Id, Nullable, StringColumn)
import Connection (ORM, withConfig)
import Control.Alt ((<|>))
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Create (create)
import Table (Table)
import Type.Proxy (Proxy(..))


type Person = Table "people" (
  Id &
  Column "first_name" StringColumn &
  Column "last_name" StringColumn &
  Column "middle_name" (Nullable StringColumn) &
  Column "gender" (Default "'MALE'" StringColumn)
)

person :: Proxy Person
person = Proxy

type Address = Table "addresses" (
  Id &
  Column "city" StringColumn &
  Column "state" StringColumn &
  Column "zip" StringColumn &
  Column "line1" StringColumn &
  Column "line2" (Nullable StringColumn)
)

address :: Proxy Address
address = Proxy

main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION, orm :: ORM | e) Unit
main = do
  const unit <$> (launchAff $ do
    withConfig { database: "test-orm" , logSql: true } $ do
      create person <|> log "false"
  )





