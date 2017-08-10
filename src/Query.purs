module Query where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (State, gets, modify, runState)
import Data.Array (head, snoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd, uncurry)
import Exec (ORM, Runner, AffOrm)
import Select (class SelectMappable, class SelectRecord, getSelects, mapToResult, toSelectables)
import Table (Table)
import Type.Proxy (Proxy(..))
import Utils (ParamSQL(..), SQLPart(..), joinParamSQLWith, realize)

data From = From String String
fromToSelect :: From -> String
fromToSelect (From table alias) = table <> " " <> alias

initialQueryState :: QueryState
initialQueryState =
  { fromAliasCount: 0
  , selectAliasCount: 0
  , froms: []
  , filters: []
  , limit: Nothing
  , offset: Nothing
  }

type QueryState =
 { fromAliasCount :: Int
 , selectAliasCount :: Int
 , froms :: Array From
 , filters :: Array Filter
 , limit :: Maybe Int
 , offset :: Maybe Int
 }

type Query r = State QueryState r

getAlias :: String -> Query String
getAlias table = do
   current <- gets (_.fromAliasCount)
   let next = current + 1
   modify (_ { fromAliasCount = next })
   pure $ table <> show next

queryStateToSelect :: ParamSQL -> QueryState -> ParamSQL
queryStateToSelect (ParamSQL selects) { froms, filters, limit } =
  ParamSQL $ [Raw "SELECT "] <> selects <> [Raw (" FROM " <> joinWith ", " (fromToSelect <$> froms) <> whereClause filters <> limit' limit)]
    where
      whereClause [] = ""
      whereClause a = " WHERE " <> joinWith " AND " (unfilter <$> filters)
      unfilter (Filter s) = s
      limit' Nothing = ""
      limit' (Just n) = " LIMIT " <> show n

query :: forall fx s r any. SelectMappable s r any => Runner fx -> Query s -> AffOrm fx (Array r)
query runner q = do
  let (Tuple s q) = runState q initialQueryState
  let paramSQLList = joinParamSQLWith ", " $ getSelects s
  let sql = queryStateToSelect paramSQLList q

  arr <- uncurry runner $ realize sql
  let res = sequence $ map snd <$> mapToResult s <$> arr

  case res of
       Left s -> throwError (error s)
       Right rec -> pure rec

queryOne :: forall fx any. Runner fx -> (forall s. Query s -> (forall r. SelectMappable s r any => Aff (orm :: ORM | fx) (Maybe r)))
queryOne runner q = do
  res <- query runner (q <* modify (_ { limit = Just 1 }))
  pure $ head res

from :: forall r n cd. IsSymbol n => SelectRecord cd r => Proxy (Table n cd) -> Query (Record r)
from _ = do
  alias <- getAlias table
  froms <- gets (_.froms)
  modify (_ { froms = snoc froms (From table alias) })
  pure (toSelectables (Proxy :: Proxy cd) alias)
    where
      table = reflectSymbol (SProxy :: SProxy n)

data Filter = Filter String

filter :: Filter -> Query Unit
filter f = do
  filters <- gets (_.filters)
  modify (_ { filters = snoc filters f })

select :: forall a. a -> Query a
select = pure
