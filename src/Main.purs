module Main where

import Prelude hiding (when, join)

import Connection (withTransaction, withConnection, class MonadQuerier)
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Expression (isNotNull, cond, when, default, (:<), (:&&), (:==), (:<>), (:<|>))
import Operations (createIfNotExists, insertBatch, truncate, drop, select, insertBatch_)
import Operations.Select (from, join)
import Postgres.Connection as Postgres
import Sqlite.Connection as Sqlite
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
    -- Insert in a batch, returning the results
    newPeople <- insertBatch people \value -> do
      value { first_name: "George", last_name: "Washington" }
      value { first_name: "John", last_name: "Adams" }
      value { first_name: "Thomas", last_name: "Jefferson" }

    -- Insert in a batch, ignoring the results (this will change the
    -- underlying queries if it needs to in order to avoid returning results)
    insertBatch_ people \value -> do
      value { first_name: "John", middle_name: "Quincy", last_name: "Adams" }
      value { first_name: "Other", last_name: "Adams" }
      value { first_name: "James", last_name: "Madison" }
      value { first_name: "Benjamin", last_name: "Franklin" }

    selectedPeople <- select do
      p1 <- from people
      p2 <- join people (\p -> p.last_name :== p1.last_name :&& p.id :< p1.id)


      let name person = (
         cond (do
           when
             (isNotNull person.middle_name)
             (person.first_name :<> " " :<> (person.middle_name :<|> "") :<> " " :<> person.last_name)
           )
           (default (person.first_name :<> " " :<> person.last_name))
      )


      pure $
        { name1: name p1
        , name2: name p2
        , first_names_match: p1.first_name :== p2.first_name
        }

    for_ selectedPeople \person -> do
      log $ show person

  drop people

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
