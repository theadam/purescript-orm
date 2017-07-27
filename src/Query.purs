module Query where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (State, gets, modify, runState)
import Data.Array (head, snoc, zip)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (fromFoldable)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Exec (ORM, Runner, AffOrm)
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
  , limit: Nothing
  , offset: Nothing
  }

type QueryState =
 { aliasCount :: Int
 , froms :: Array From
 , filters :: Array Filter
 , limit :: Maybe Int
 , offset :: Maybe Int
 }

type Query r = State QueryState r

getAlias :: String -> Query String
getAlias table = do
   current <- gets (_.aliasCount)
   let next = current + 1
   modify (_ { aliasCount = next })
   pure $ table <> show next

queryStateToSelect :: String -> QueryState -> String
queryStateToSelect selects { froms, filters, limit } =
  "SELECT " <> selects <> " FROM " <> joinWith ", " (fromToSelect <$> froms) <>
  whereClause filters <> limit' limit
    where
      whereClause [] = ""
      whereClause a = " WHERE " <> joinWith " AND " (unfilter <$> filters)
      unfilter (Filter s) = s
      limit' Nothing = ""
      limit' (Just n) = " LIMIT " <> show n

query :: forall fx s r. SelectMappable s r => Runner fx -> Query s -> AffOrm fx (Array r)
query runner q = do
  let (Tuple s q) = runState q initialQueryState
  let selectList = getSelects s
  let selects = joinWith ", " selectList
  let sql = queryStateToSelect selects q

  arr <- runner sql []
  let toSelectMap item = fromFoldable (zip selectList item)
  let maps = toSelectMap <$> arr
  let res = sequence $ mapToResult s <$> maps

  case res of
       Left s -> throwError (error s)
       Right rec -> pure rec

queryOne :: forall fx. Runner fx -> (forall s. Query s -> (forall r. SelectMappable s r => Aff (orm :: ORM | fx) (Maybe r)))
queryOne runner q = do
  res <- query runner (q <* modify (_ { limit = Just 1 }))
  pure $ head res

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
isEqual a b = Filter (toSql a <> " = " <> toSql b)
infixr 6 isEqual as .=

filter :: Filter -> Query Unit
filter f = do
  filters <- gets (_.filters)
  modify (_ { filters = snoc filters f })

