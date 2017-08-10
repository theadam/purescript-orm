module Main where

import Prelude

import Column (type (&), Column, Default, Id, Nullable, StringColumn)
import Connection (Connection(..), connect)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Error.Class (catchError)
import Data.Array (filter)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.String (joinWith)
import Exec (ORM)
import Query (from, select)
import RecordList ((&))
import SQLExpression (orDefault, (.<>))
import Table (Table)
import Type.Proxy (Proxy(..))

type People = Table "people" (
  Id &
  Column "first_name" StringColumn &
  Column "last_name" StringColumn &
  Column "middle_name" (Nullable StringColumn) &
  Column "gender" (Default "'MALE'" StringColumn)
)

people :: Proxy People
people = Proxy

main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION, orm :: ORM | e) Unit
main = unit <$ do
 launchAff $ create `catchError` handleError
   where
     create = do
       Connection db <- connect
         { database: "purescript_test"
         , logSQL: true
         , logResults: true
         }

       db.createIfNotExists people
       db.truncate people

       db.insertInto_ people { first_name: "George", last_name: "Washington" }

       db.insertInto_ people $
           { first_name: "Abraham", last_name: "Lincoln" } &
           { first_name: "Teddy", last_name: "Roosevelt" } &
           { first_name: "Dwight", last_name: "Eisenhower", middle_name: "D." }

       vals <- db.query $ do
         person <- from people
         select (
           person.first_name .<> " " .<>
           person.middle_name `orDefault` "" .<> " " .<>
           person.last_name
         )

       for_ vals $ log

     fullName r = joinWith " " $ filter (_ /= "") [r.first_name, fromMaybe "" r.middle_name, r.last_name]

     handleError e = log (show e)





