module Main where

import Prelude hiding (when,join)

import Connection (withTransaction, withConnection, class MonadQuerier)
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Expression (cond, default, isNotNull, toNullable, when, (:<>), (:<|>), (:==))
import Operations (createIfNotExists, insertBatch, truncate, drop, select, insertBatch_, insert)
import Operations.Select (from, join)
import Postgres.Connection as Postgres
import Sqlite.Connection as Sqlite
import TableDefinition (Default, Id, IntegerColumn, Nullable, StringColumn, Table, makeTable)


people :: Table
  ( id :: Id
  , first_name :: StringColumn
  , last_name :: StringColumn
  , middle_name :: Nullable StringColumn
  , gender :: Default "'Male'" StringColumn
  , location_id :: Nullable IntegerColumn
  )
people = makeTable "people"

locations :: Table
  ( id :: Id
  , name :: StringColumn
  )
locations = makeTable "locations"

operations :: forall m. MonadEffect m => MonadQuerier m => m Unit
operations = do
  createIfNotExists people
  createIfNotExists locations
  truncate people
  truncate locations

  withTransaction do
    dc <- insert locations { name: "Washington DC" }
    philly <- insert locations { name: "Philadelphia" }
    somewhere <- insert locations { name: "Somewhere Else" }


    -- Insert in a batch, returning the results
    newPeople <- insertBatch people \value -> do
      value { first_name: "George", last_name: "Washington", location_id: dc.id }
      value { first_name: "John", last_name: "Adams", location_id: dc.id }
      value { first_name: "Thomas", last_name: "Jefferson", location_id: dc.id }

    -- Insert in a batch, ignoring the results (this will change the
    -- underlying queries if it needs to in order to avoid returning results)
    insertBatch_ people \value -> do
      value { first_name: "John", middle_name: "Quincy", last_name: "Adams", location_id: dc.id }
      value { first_name: "Other", last_name: "Adams", location_id: somewhere.id }
      value { first_name: "James", last_name: "Madison", location_id: dc.id }
      value { first_name: "Benjamin", last_name: "Franklin", location_id: philly.id }

    selectedPeople <- select do
      person <- from people
      location <- join locations (\l -> person.location_id :== toNullable(l.id))


      let name p = (
         cond (do
           when
             (isNotNull p.middle_name)
             (p.first_name :<> " " :<> (p.middle_name :<|> "") :<> " " :<> p.last_name)
           )
           (default (p.first_name :<> " " :<> p.last_name))
      )


      pure { name: name person, location: location }

    for_ selectedPeople \person -> do
      log $ show person

  drop people
  drop locations

main :: Effect Unit
main = launchAff_ $ do
  let logOps = true

  log "Running with Postgres"
  connection <- Postgres.makeConnection (
    Postgres.defaultConfig { database = "purescript_test", logSql = logOps, logResults = logOps }
  )

  withConnection connection operations

  log "Running with Sqlite"
  sqlite <- Sqlite.makeConnection { filePath: ":memory:", logSql: logOps, logResults: logOps }

  withConnection sqlite operations
