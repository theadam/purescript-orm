module Classes.Select where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (uncons)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
import Foreign (Foreign)
import ParameterizedSql (ParameterizedSql, toSql)
import Prim.Row as Row
import Prim.RowList (kind RowList, Nil, Cons, class RowToList)
import Record (insert, delete, get)
import TableDefinition (class ColumnType)
import Type.Data.Row (RProxy)
import Type.Equality (class TypeEquals, to)
import Type.Prelude (Proxy(..), RLProxy(..), SProxy(..), reflectSymbol)
import Utils (class FromForeign, fromForeign)


class SelectResult r o | r -> o where
  mapResultRow :: Proxy r -> Array Foreign -> Either Error (Tuple (Array Foreign) o)
  createSelects :: r -> Array ParameterizedSql

class SelectResultRL (rl :: RowList) i o | rl -> o, rl -> i where
  mapResultRowRL :: RLProxy rl -> Array Foreign -> Either Error (Tuple (Array Foreign) o)
  createSelectsRL :: RLProxy rl -> i -> Array ParameterizedSql

instance selectResultRecord ::
  (
    RowToList r rl,
    SelectResultRL rl (Record r) (Record o)
  ) => SelectResult (Record r) (Record o) where
    mapResultRow _ = mapResultRowRL (RLProxy :: RLProxy rl)
    createSelects r = createSelectsRL (RLProxy :: RLProxy rl) r

instance selectResultRowListLast ::
  (
    IsSymbol k,
    SelectResult v o,
    Row.Lacks k (),
    Row.Cons k o () r,
    Row.Cons k v () i
  ) => SelectResultRL (Cons k v Nil) (Record i) (Record r) where
    mapResultRowRL _ fs = do
        Tuple tail mapped <- mapResultRow (Proxy :: Proxy v) fs
        pure $ (Tuple tail $ insert (SProxy :: SProxy k) mapped {})
    createSelectsRL _ r = createSelects $ get (SProxy :: SProxy k) r
else instance selectResultRowListRecur ::
  (
    IsSymbol k,
    SelectResult v o,
    Row.Lacks k r',
    Row.Cons k o r' r,
    Row.Lacks k i',
    Row.Cons k v i' i,
    SelectResultRL rl (Record i') (Record r')
  ) => SelectResultRL (Cons k v rl) (Record i) (Record r) where
    mapResultRowRL _ fs = do
      Tuple tail mapped <- mapResultRow (Proxy :: Proxy v) fs
      Tuple tail' rest <- mapResultRowRL (RLProxy :: RLProxy rl) tail
      pure $ (Tuple tail' $ insert (SProxy :: SProxy k) mapped rest)
    createSelectsRL _ r =
      (createSelects $ get (SProxy :: SProxy k) r) <>
      (createSelectsRL (RLProxy :: RLProxy rl) (delete (SProxy :: SProxy k) r))

data ColumnAlias o = ColumnAlias String String

instance selectResultColumnAlias :: (FromForeign o) => SelectResult (ColumnAlias o) o where
  mapResultRow _ fs = do
    case uncons fs of
      Nothing -> throwError $ error "Missing item in result row"
      Just { head, tail } -> Tuple tail <$> fromForeign head
  createSelects (ColumnAlias table field) = [toSql $ table <> "." <> field]

-- Create a record o Column aliases from a table
class Fromable (t :: # Type) exprs | t -> exprs where
  toFroms :: RProxy t -> String -> exprs

instance fromableTable ::
  (RowToList t rl, FromableColumns rl exprs) => Fromable t exprs where
  toFroms _ alias = generateFroms (RLProxy :: RLProxy rl) alias

class FromableColumns (rl :: RowList) exprs | rl -> exprs where
  generateFroms :: RLProxy rl -> String -> exprs

instance nilFromableColumns :: (TypeEquals (Record ()) r) => FromableColumns Nil r where
    generateFroms _ _ = to {}
else instance consFromableColumns ::
 (
   IsSymbol k,
   Row.Lacks k r',
   Row.Cons k (ColumnAlias o) r' r,
   FromableColumns rl' (Record r'),
   ColumnType v i o
 ) => FromableColumns (Cons k v rl') (Record r) where
   generateFroms _ alias = insert sproxy (ColumnAlias alias (reflectSymbol sproxy)) (generateFroms (RLProxy :: RLProxy rl') alias)
    where
      sproxy :: SProxy k
      sproxy = SProxy
