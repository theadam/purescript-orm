module Convert where

import Prelude

import Connection (From(..), Operation(..))
import Control.Lazy (defer)
import Data.Tuple (Tuple(..))
import ParameterizedSql (ParameterizedSql, commaJoin, listify, toSql, (:<>))
import TableDefinition (ColumnDefinition(..))

makeConvertColumnDefinition :: (ColumnDefinition -> String) -> ColumnDefinition -> String
makeConvertColumnDefinition _ Id = "SERIAL PRIMARY KEY"
makeConvertColumnDefinition _ (Varchar i) = "VARCHAR(" <> show i <> ")"
makeConvertColumnDefinition c (NotNull cd) = c cd <> " NOT NULL"
makeConvertColumnDefinition c (Nullable cd) = c cd <> " NULL"
makeConvertColumnDefinition c (Default default cd) = c cd <> " DEFAULT " <> default

convertColumnDefinition :: ColumnDefinition -> String
convertColumnDefinition = makeConvertColumnDefinition $ defer (\_ -> convertColumnDefinition)

makeConvert :: (ColumnDefinition -> String) -> Operation -> ParameterizedSql
makeConvert _ (Insert name intos values) =
  toSql "INSERT INTO " :<> name :<> " " :<> (listify intos) :<> " VALUES " :<> rows
    where
      rows = commaJoin $ listify <$> values
makeConvert convertCol (Create ifNotExists name cols) = toSql "CREATE TABLE " :<> modifier ifNotExists :<> name :<> " " :<> listify (mapCol <$> cols)
  where
    modifier true = " IF NOT EXISTS "
    modifier _ = ""
    mapCol (Tuple col def) = col <> " " <> convertCol def
makeConvert _ (Truncate t) = toSql "TRUNCATE TABLE " :<> t
makeConvert _ (Drop t) = toSql "DROP TABLE " :<> t
makeConvert _ (Select selects { froms }) =
  toSql "SELECT " :<> commaJoin selects :<> " FROM " :<> (commaJoin $ fromToStr <$> froms)
    where
      fromToStr (From table alias) = table <> " " <> alias


convert :: Operation -> ParameterizedSql
convert = makeConvert convertColumnDefinition
