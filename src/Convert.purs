module Convert where

import Prelude

import Connection (class MonadConverter, From(..), Join(..), Operation(..))
import Connection as C
import Data.Array (length)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Expression (BoolExp, representation)
import ParameterizedSql (ParameterizedSql, commaJoin, listify, sqlJoin, toSql, (:<>:))
import TableDefinition (ColumnDefinition(..))

convertColumnDefinition :: forall m. MonadConverter m => ColumnDefinition -> m String
convertColumnDefinition Id = pure $ "SERIAL PRIMARY KEY"
convertColumnDefinition Integer = pure $ "INTEGER"
convertColumnDefinition (Varchar i) = pure $ "VARCHAR(" <> show i <> ")"
convertColumnDefinition (NotNull cd) = do
  def <- C.convertColumnDefinition cd
  pure $ def <> " NOT NULL"
convertColumnDefinition (Nullable cd) = do
  def <- C.convertColumnDefinition cd
  pure $ def <> " NULL"
convertColumnDefinition (Default default cd) = do
  def <- C.convertColumnDefinition cd
  pure $ def <> " DEFAULT " <> default

convertOperation :: forall m. MonadConverter m => Operation -> m ParameterizedSql
convertOperation (Insert name intos values _) = pure $
  toSql "INSERT INTO " :<>: name :<>: " " :<>: (listify intos) :<>: " VALUES " :<>: rows
    where
      rows = commaJoin $ listify <$> values

convertOperation (Create ifNotExists name cols) = do
  let start = toSql "CREATE TABLE " :<>: modifier ifNotExists :<>: name :<>: " "
  defs <- sequence $ (mapCol <$> cols)
  pure $ start :<>: listify defs
    where
      modifier true = " IF NOT EXISTS "
      modifier _ = ""
      mapCol (Tuple col def) = do
        d <- C.convertColumnDefinition def
        pure $ col <> " " <> d

convertOperation (Truncate t) = pure $ toSql "TRUNCATE TABLE " :<>: t
convertOperation (Drop t) = pure $ toSql "DROP TABLE " :<>: t
convertOperation (Select selects { froms, wheres, joins }) = pure $
  toSql "SELECT " :<>: commaJoin selects :<>: " FROM " :<>: (commaJoin $ fromToStr <$> froms) :<>: joinClause :<>: ws
    where
      ws | length wheres == 0 = toSql ""
         | otherwise = toSql " WHERE " :<>: conditions
      conditions = sqlJoin " AND " (representation <$> wheres)
      joinClause | length joins == 0 = toSql ""
                 | otherwise = toSql " " :<>: sqlJoin " " (joinToSql <$> joins)



-- Helpers
fromToStr :: From -> String
fromToStr (From table alias) = table <> " " <> alias

joinToSql :: Join -> ParameterizedSql
joinToSql (Inner f e) = toSql "JOIN " :<>: joinContent f e
joinToSql (Left f e) = toSql "LEFT JOIN " :<>: joinContent f e
joinToSql (Outer f e) = toSql "OUTER JOIN " :<>: joinContent f e
joinToSql (Right f e) = toSql "RIGHT JOIN " :<>: joinContent f e

joinContent :: From -> BoolExp -> ParameterizedSql
joinContent f e = (toSql $ fromToStr f) :<>: " ON " :<>: representation e
