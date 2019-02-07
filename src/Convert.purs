module Convert where

import Prelude

import Connection (class MonadConverter, From(..), Operation(..))
import Connection as C
import Data.Array (length)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Expression (representation)
import ParameterizedSql (ParameterizedSql, commaJoin, listify, sqlJoin, toSql, (:<>:))
import TableDefinition (ColumnDefinition(..))

convertColumnDefinition :: forall m. MonadConverter m => ColumnDefinition -> m String
convertColumnDefinition Id = pure $ "SERIAL PRIMARY KEY"
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
convertOperation (Select selects { froms, wheres }) = pure $
  toSql "SELECT " :<>: commaJoin selects :<>: " FROM " :<>: (commaJoin $ fromToStr <$> froms) :<>: ws
    where
      fromToStr (From table alias) = table <> " " <> alias
      ws | length wheres == 0 = toSql ""
         | otherwise = toSql " WHERE " :<>: joined
      joined = sqlJoin " AND " (representation <$> wheres)

