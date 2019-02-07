module Classes.Insert where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Array (uncons)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Defaultable (withDefaultNothings, class DefaultNothings)
import Effect.Exception (Error, error)
import Foreign (Foreign)
import Prim.Row as Row
import Prim.RowList (Cons, Nil, kind RowList, class RowToList)
import Record (get, insert)
import TableDefinition (class ColumnType)
import ParameterizedSql (class ToValue, Value, toValue)
import Type.Data.Row (RProxy)
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals, to)
import Utils (convertEither, class FromForeign, fromForeign)

class InsertResult (t :: # Type) r | t -> r where
  mapResult :: forall m. MonadError Error m => RProxy t -> Array Foreign -> m r

instance tableInsertResult :: (RowToList cs csl, InsertResultL csl r) => InsertResult cs r where
  mapResult _ = mapResultL (RLProxy :: RLProxy csl)

class InsertResultL (t :: RowList) r | t -> r where
  mapResultL :: forall m. MonadError Error m => RLProxy t -> Array Foreign -> m r

instance nilInsertResultL :: (TypeEquals {} r) => InsertResultL Nil r where
  mapResultL _ _ = pure $ to {}

instance consInsertResultL :: (ColumnType v i o, FromForeign o, IsSymbol k, InsertResultL rl (Record r'), Row.Cons k o r' r, Row.Lacks k r') => InsertResultL (Cons k v rl) (Record r) where
  mapResultL _ a = do
    let unc = uncons a
    case unc of
      Nothing -> throwError $ error "Unexpected end to result list"
      Just { head, tail } -> do
        h <- convertEither $ fromForeign head
        rest <- mapResultL (RLProxy :: RLProxy rl) tail
        pure $ insert (SProxy :: SProxy k) h rest

-- Creates Insert from a single table and permissive insert record
class Insertable (t :: # Type) r where
    values :: RProxy t -> r -> Array Value

instance insertableTableRecord ::
    (
      RowToList csr cs,
      ColsToInput cs i,
      DefaultNothings r i,
      InsertableValues cs i
    ) => Insertable csr r where
      values _ r = insertableValues (RLProxy :: RLProxy cs) (withDefaultNothings r :: i)

-- Create an array of tuples of columns and their values
class InsertableValues (cs :: RowList) i where
    insertableValues :: RLProxy cs -> i -> Array Value

instance nilInsertableValues :: InsertableValues Nil i where
    insertableValues _ _ = []
else instance otherInsertableValues ::
  (
    IsSymbol k,
    InsertableValues rest (Record i),
    ToValue iv,
    Row.Cons k iv i' i,
    ColumnType v iv ov
  ) => InsertableValues (Cons k v rest) (Record i) where
    insertableValues _ i = [toValue value] <> insertableValues (RLProxy :: RLProxy rest) i
      where
        key :: SProxy k
        key = SProxy
        value = get key i

class ColsToInput (cs :: RowList) input | cs -> input

instance emptyColsInput :: ColsToInput Nil (Record ())
else instance colsToInput ::
  (
      ColsToInput rest (Record tail),
      ColumnType v i o,
      Row.Cons k i tail all
  ) =>
  ColsToInput (Cons k v rest) (Record all)

