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
import Data.Maybe (Maybe(..))
import Exec (ORM)
import Query (filter, select, (.=))
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

main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION, orm :: ORM | e) Unit
main = unit <$ do
 launchAff $ create `catchError` handleError
   where
     create = do
       Connection db <- connect { database: "purescript_test", logSql: true }

       db.createIfNotExists person
       db.truncate person

       db.insert person { first_name: "Adam", last_name: "Nalisnick", middle_name: Just "David" }
       db.insert person { first_name: "Adam", last_name: "Nalisnick" }
       db.insert person { first_name: "Adam", last_name: "Nalisnick" }
       db.insert person { first_name: "Adam", last_name: "Nalisnick" }
       db.insert person { first_name: "Adam", last_name: "Nalisnick" }

       vals <- db.query $ do
         p <- select person
         filter (p.first_name .= "Adam")
         pure p.first_name

       for_ vals log

     handleError e = log (show e)





