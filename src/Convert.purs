module Convert where

import Prelude

import Connection (Operation(..))
import Data.Tuple (Tuple(..))
import ParameterizedSql (ParameterizedSql, commaJoin, listify, toSql, (:<>))

-- genericSelect :: Select -> ParameterizedSql
-- genericSelect (Select { selects, froms, filters, limit, offset }) =
--   toSql "SELECT " :<> commaJoin selects :<>
--     " FROM " :<> fs :<>
--     " WHERE " :<> ws :<>
--     handleInt "LIMIT" limit :<>
--     handleInt "OFFSET" offset
--       where
--         mapFrom (From t a) = t <> " " <> a
--         fs = commaJoin $ mapFrom <$> froms
--         ws = sqlJoin " AND " filters
--         handleInt text (Just i) = " " <> text <> " " <> show i
--         handleInt _ Nothing = ""

convert :: Operation -> ParameterizedSql
convert (Insert name intos values) =
  toSql "INSERT INTO " :<> name :<> " " :<> (listify intos) :<> " VALUES " :<> rows
    where
      rows = commaJoin $ listify <$> values
convert (Create ifNotExists name cols) = toSql "CREATE TABLE " :<> modifier ifNotExists :<> name :<> " " :<> listify (mapCol <$> cols)
  where
    modifier true = " IF NOT EXISTS "
    modifier _ = ""
    mapCol (Tuple col def) = col <> " " <> def
convert (Truncate t) = toSql "TRUNCATE TABLE " :<> t
