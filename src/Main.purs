module Main where

import Prelude

import Column (type (&), Column, Default, Id, Nullable, StringColumn)
import Connection (Connection(..), connect)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Error.Class (catchError)
import Data.Foldable (for_)
import Exec (ORM)
import RecordList ((&))
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
       Connection db <- connect { database: "purescript_test", logSql: true }

       db.createIfNotExists people
       db.truncate people

       first <- db.insertInto people { first_name: "Adam", last_name: "Nalisnick" }


       saved <- db.insertInto people $
           { first_name: "Adam", last_name: "Nalisnick" } &
           { first_name: "Adam", last_name: "Nalisnick" }


       for_ saved (log <<< show <<< (_.gender))

     handleError e = log (show e)





