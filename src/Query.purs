module Query where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (State, gets, modify, runState)
import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.StrMap (singleton)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Select (class SelectMappable, class SelectRecord, getSelects, mapToResult, toSelectables)
import Table (Table)
import Type.Proxy (Proxy(..))
import Utils (class Sqlable, toSql)

data From = From String String
fromToSelect :: From -> String
fromToSelect (From table alias) = table <> " " <> alias

initialQueryState :: QueryState
initialQueryState =
  { aliasCount: 0
  , froms: []
  , filters: []
  }

type QueryState = { aliasCount :: Int, froms :: Array From, filters :: Array Filter }
type Query r = State QueryState r

getAlias :: String -> Query String
getAlias table = do
   current <- gets (_.aliasCount)
   let next = current + 1
   modify (_ { aliasCount = next })
   pure $ table <> show next

queryStateToSelect :: String -> QueryState -> String
queryStateToSelect selects { froms, filters } =
  "SELECT " <> selects <> " FROM " <> joinWith ", " (fromToSelect <$> froms) <>
  whereClause filters
    where
      whereClause [] = ""
      whereClause a = " WHERE " <> joinWith " AND " (unfilter <$> filters)
      unfilter (Filter s) = s

query :: forall s r fx. SelectMappable s r => Query s -> Aff (console :: CONSOLE | fx) (Array r)
query q = do
  let (Tuple s q) = runState q initialQueryState
  let selects = joinWith ", " (getSelects s)
  log $ queryStateToSelect selects q

  let res = mapToResult s (singleton "people1.first_name" (toForeign "Adam"))

  case res of
       Left s -> throwError (error s)
       Right rec -> pure [rec]

queryOne :: forall s r fx. SelectMappable s r => Query s -> Aff (console :: CONSOLE | fx) r
queryOne q = do
  let (Tuple s q) = runState q initialQueryState
  let selects = joinWith ", " (getSelects s)
  log $ queryStateToSelect selects q

  let res = mapToResult s (singleton "people1.first_name" (toForeign "Adam"))

  case res of
       Left s -> throwError (error s)
       Right rec -> pure rec

select :: forall r n cd. IsSymbol n => SelectRecord cd r => Proxy (Table n cd) -> Query (Record r)
select _ = do
  alias <- getAlias table
  froms <- gets (_.froms)
  modify (_ { froms = snoc froms (From table alias) })
  pure (toSelectables (Proxy :: Proxy cd) alias)
    where
      table = reflectSymbol (SProxy :: SProxy n)

data Filter = Filter String

isEqual :: forall a b. Sqlable a => Sqlable b => a -> b -> Filter
isEqual a b = Filter (toSql a <> " == " <> toSql b)
infixr 6 isEqual as .==

filter :: Filter -> Query Unit
filter f = do
  filters <- gets (_.filters)
  modify (_ { filters = snoc filters f })

