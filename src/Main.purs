module Main where

import Prelude

import Connection (withTransaction, withConnection, class MonadQuerier)
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Operations (createIfNotExists, insertBatch, truncate)
import Postgres.Connection as Postgres
import TableDefinition (Table, makeTable, Default, Id, Nullable, StringColumn)


people :: Table
  ( id :: Id
  , first_name :: StringColumn
  , last_name :: StringColumn
  , middle_name :: Nullable StringColumn
  , gender :: Default "'Male'" StringColumn
  )
people = makeTable "people"

operations :: forall m. MonadEffect m => MonadQuerier m => m Unit
operations = do
  createIfNotExists people
  truncate people

  withTransaction do
    newPeople <- insertBatch people \value -> do
      value { first_name: "George", last_name: "Washington" }
      value { first_name: "John", last_name: "Adams" }
      value { first_name: "Thomas", last_name: "Jefferson" }

    for_ newPeople \person -> do
      log $ show person

main :: Effect Unit
main = launchAff_ $ do
  let log = true

  connection <- Postgres.makeConnection (
    Postgres.defaultConfig { database = "purescript_test", logSql = log, logResults = log }
  )

  withConnection connection operations

