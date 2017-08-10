module Insert where

import Prelude

import Column (And, Column)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Array (head, zip)
import Data.Either (Either(..))
import Data.Foldable (fold, intercalate)
import Data.Foreign (Foreign, isNull)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (StrMap, empty, fromFoldable, keys, lookup, singleton, union, values)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence)
import Data.Tuple (Tuple, uncurry)
import Database.PostgreSQL (class FromSQLValue, fromSQLValue)
import Defaults (class Defaults, withDefaults)
import Exec (AffOrm, Runner)
import Record as R
import RecordList (NilRecordList, RecordList(RecordList))
import SQLExpression (class ColumnType, class SQLExpression, toSQLExpression)
import Table (Table, columns, tableName)
import Type.Data.Boolean (kind Boolean)
import Type.Proxy (Proxy(..))
import Utils (ParamSQL(..), SQLPart(..), addCommas, realize, surroundWith)

class InsertRecord cd r | cd -> r where
  toPairs :: Proxy cd -> Record r -> StrMap ParamSQL

instance colInsertRecord ::
  (SQLExpression i t, ColumnType cd i o, IsSymbol n, RowCons n i () r) =>
  InsertRecord (Column n cd) r where
    toPairs _ r = singleton (reflectSymbol sproxy) (toSQLExpression (R.get sproxy r))
      where
        proxy = Proxy :: Proxy cd
        sproxy = SProxy :: SProxy n

instance colInsertAnd ::
  (InsertRecord c1 r1, InsertRecord c2 r2, Union r2 r1 r3, Union r1 r2 r3) =>
  InsertRecord (And c1 c2) r3 where
    toPairs _ r =
      union
        (toPairs (Proxy :: Proxy c1) (R.subrecord r))
        (toPairs (Proxy :: Proxy c2) (R.subrecord r))

newtype Insert t = Insert (Array (StrMap ParamSQL))
derive instance insertNewtype :: Newtype (Insert t) _

instance smigroupInsert :: Semigroup (Insert t) where
  append (Insert p1) (Insert p2) = Insert (p1 <> p2)

class Insertable t a res | a t -> res where
  toInsert :: Proxy t -> a -> Insert t
  toResult
    :: Proxy t -> Proxy a -> (Array (StrMap Foreign)) -> Either String res

instance insertableInsertRecord
  :: ( InsertRecord cd r
     , Defaults i r
     , InsertResponse (Table n cd) res
     ) => Insertable (Table n cd) (Record i) res where
  toInsert t a = Insert [pairs]
    where
      pairs = toPairs (columns t) (withDefaults a)
  toResult t _ f = case head f of
    Nothing -> Left "Invalid empty insert result"
    Just h -> toResponse t h

nullToDefault :: ParamSQL -> ParamSQL
nullToDefault (ParamSQL a) = ParamSQL $ map addDefaults a
  where
    addDefaults (Param n) | isNull n = Raw "DEFAULT"
                          | otherwise = Param n
    addDefaults s = s

insertToSQL :: forall n cd. IsSymbol n => Insert (Table n cd) -> Tuple String (Array Foreign)
insertToSQL (Insert maps) = realize sql
  where
    start = ParamSQL $ [Raw (space
      ["INSERT INTO", tableName (Proxy :: Proxy (Table n cd)), cols, "VALUES "]
    )]
    end = ParamSQL $ [Raw (space [" RETURNING", bareCols])]
    sql = start <> params <> end
    params = intercalate (ParamSQL $ [Raw ", "]) $
      (surroundWith "(" ")" <<< addCommas <<< nullToDefault <<< fold <<< values)
      <$> maps
    bareCols = comma (keys $ fromMaybe empty (head maps))
    cols = surround (bareCols)
    comma = joinWith ", "
    space = joinWith " "
    surround str = "(" <> str <> ")"

insertInto :: forall n r cd fx res. IsSymbol n => Runner fx -> Proxy (Table n cd) -> r -> (Insertable (Table n cd) r res => AffOrm fx res)
insertInto runner table rec = do
  let ins = toInsert table rec
  arry <- ((uncurry runner) $ insertToSQL $ ins)

  let cols = keys $ fromMaybe empty (head (unwrap ins))
  let convert a = fromFoldable $ zip cols a
  let maps = convert <$> arry

  let res = toResult table (Proxy :: Proxy r) maps

  case res of
    Right r -> pure r
    Left s -> throwError $ error s

class AllInsertable t h where
  toInsert' :: (Proxy t) -> h -> Insert t
instance nilInsertable :: (Insertable t r res) => AllInsertable t (RecordList r NilRecordList) where
  toInsert' t (RecordList h _) = toInsert t h
instance recAllInsertable
  :: (Insertable t r res, AllInsertable t (RecordList s s'))
  => AllInsertable t (RecordList r (RecordList s s')) where
    toInsert' t (RecordList h tail) = toInsert t h <> toInsert' t tail

instance listInsertable
  :: ( AllInsertable t (RecordList h tail)
     , InsertResponse t res
     ) =>  Insertable t (RecordList h tail) (Array res) where
  toInsert t list = toInsert' t list
  toResult t _ f = sequence $ toResponse t <$> f

class InsertResponse t res where
  toResponse :: Proxy t -> StrMap Foreign -> Either String res
instance tableInsertColumns
  :: InsertResponseColumns cd res => InsertResponse (Table n cd) res where
    toResponse table = toResponse' $ columns table

class InsertResponseColumns cd res | cd -> res where
  toResponse' :: Proxy cd -> StrMap Foreign -> Either String res
instance colInsertResponse
  :: ( ColumnType ty i o
     , IsSymbol n
     , FromSQLValue o
     , RowCons n o () res
     ) => InsertResponseColumns (Column n ty) (Record res)
       where
         toResponse' _ map = case lookup name map of
           Nothing -> Left $ "Invalid insert response for " <> name
           Just h -> R.insert (SProxy :: SProxy n) <$> fromSQLValue h <@> {}
             where name = reflectSymbol (SProxy :: SProxy n)

instance andInsertResponse
  :: ( InsertResponseColumns c1 (Record r1)
     , InsertResponseColumns c2 (Record r2)
     , Union r1 r2 r3
     ) => InsertResponseColumns (And c1 c2) (Record r3)
       where
         toResponse' _ map = R.merge <$>
           (toResponse' (Proxy :: Proxy c1) map :: Either String (Record r1)) <*>
           (toResponse' (Proxy :: Proxy c2) map)

