module Main where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Operations (createIfNotExists, insertBatch, truncate)
import TableDefinition (Table, makeTable, Default, Id, Nullable, StringColumn)
import Connection (withTransaction, withConnection)
import Postgres.Connection (makeConnection, defaultConfig)


people :: Table
  ( id :: Id
  , first_name :: StringColumn
  , last_name :: StringColumn
  , middle_name :: Nullable StringColumn
  , gender :: Default "'Male'" StringColumn
  )
people = makeTable "people"

main :: Effect Unit
main = launchAff_ $ do
  connection <- makeConnection (
    defaultConfig { database = "purescript_test", logSql = true, logResults = true }
  )

  withConnection connection do
    createIfNotExists people
    truncate people

    withTransaction do
      newPeople <- insertBatch people $ \value -> do
        value { first_name: "George", last_name: "Washington" }
        value { first_name: "John", last_name: "Adams" }
        value { first_name: "Thomas", last_name: "Jefferson" }

      for_ newPeople $ \person -> do
        log $ show person

