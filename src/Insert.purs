module Insert where

import Prelude

import Column (class ColumnType, And, Column)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Array (filter, head, snoc, zip)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Foreign (Foreign, isNull)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (StrMap, fromFoldable, keys, lookup, singleton, union, values)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Database.PostgreSQL (class FromSQLValue, fromSQLValue, toSQLValue)
import Defaults (class Defaults, withDefaults)
import Exec (AffOrm, Runner)
import Record as R
import RecordList (NilRecordList, RecordList(RecordList))
import Table (Table, columns, tableName)
import Type.Data.Boolean (kind Boolean)
import Type.Proxy (Proxy(..))

class InsertRecord cd r | cd -> r where
  toPairs :: Proxy cd -> Record r -> StrMap Foreign

instance colInsertRecord ::
  (ColumnType cd i o, IsSymbol n, RowCons n i () r) =>
  InsertRecord (Column n cd) r where
    toPairs _ r = singleton (reflectSymbol sproxy) (toSQLValue (R.get sproxy r))
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

data Placeholder = Default | Param
data Vals = Single (StrMap Foreign) | Multiple Vals (StrMap Foreign)

instance semiGroupVals :: Semigroup Vals where
  append v (Single w) = Multiple v w
  append v (Multiple ws w) = Multiple (v <> ws) w

newtype Insert t = Insert
 { vals :: Vals
 , placeholders :: Array (Array Placeholder)
 }
derive instance insertNewtype :: Newtype (Insert t) _

instance semiGroupInsert :: Semigroup (Insert t) where
  append (Insert a) (Insert b) = Insert
   { vals: a.vals <> b.vals
   , placeholders: a.placeholders <> b.placeholders
   }

replacePlaceholders :: Array (Array Placeholder) -> Array (Array String)
replacePlaceholders ps = snd $ foldl outer (Tuple 1 []) ps
  where
    outer (Tuple i rows) ary = Tuple (fst res) (snoc rows (snd res))
      where
        res = foldl runner (Tuple i []) ary
    runner (Tuple i row) Default = Tuple i (snoc row "DEFAULT")
    runner (Tuple i row) Param = Tuple (i + 1) (snoc row ("$" <> show i))

valsHead :: Vals -> StrMap Foreign
valsHead (Single s) = s
valsHead (Multiple _ s) = s

valParams :: Vals -> Array Foreign
valParams (Single map) = values map
valParams (Multiple vals map) = valParams vals <> values map

mayIns :: forall t. Maybe (Insert t) -> Insert t -> Insert t
mayIns Nothing i = i
mayIns (Just i) j = i <> j

class Insertable t a res | a t -> res where
  toInsert :: Proxy t -> a -> Insert t
  toResult
    :: Proxy t -> Proxy a -> (Array (StrMap Foreign)) -> Either String res

instance insertableInsertRecord
  :: ( InsertRecord cd r
     , Defaults i r
     , InsertResponse (Table n cd) res
     ) => Insertable (Table n cd) (Record i) res where
  toInsert t a = Insert
    { vals: Single pairs
    , placeholders: [placeholders]
    }
      where
        placeholders = map toPlaceholder (values pairs)
        toPlaceholder v | isNull v = Default
                        | otherwise = Param
        pairs = toPairs (columns t) (withDefaults a)
  toResult t _ f = case head f of
    Nothing -> Left "Invalid empty insert result"
    Just h -> toResponse t h

insertCols :: forall t. Insert t -> Array String
insertCols = keys <<< valsHead <<< (_.vals) <<< unwrap

insertToSQL :: forall n cd. IsSymbol n => Insert (Table n cd) -> Tuple String (Array Foreign)
insertToSQL (Insert { vals, placeholders }) = Tuple sql params
  where
    sql = space ["INSERT INTO", tableName (Proxy :: Proxy (Table n cd)), cols, "VALUES", ps, "RETURNING", bareCols]
    params = filter (not <<< isNull) $ valParams vals
    ps = comma $ map (surround <<< comma) (replacePlaceholders placeholders)
    vh = valsHead vals
    bareCols = comma (keys vh)
    cols = surround (bareCols)
    comma = joinWith ", "
    space = joinWith " "
    surround str = "(" <> str <> ")"

insertInto :: forall n r cd fx res. IsSymbol n => Runner fx -> Proxy (Table n cd) -> r -> (Insertable (Table n cd) r res => AffOrm fx res)
insertInto runner table rec = do
  let ins = toInsert table rec
  arry <- ((uncurry runner) $ insertToSQL $ ins)

  let cols = insertCols ins
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

