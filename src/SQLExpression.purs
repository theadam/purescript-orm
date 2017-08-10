module SQLExpression where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Database.PostgreSQL (class FromSQLValue, null, toSQLValue)
import Type.Prelude (Proxy(..))
import Utils (ParamSQL(..), SQLPart(..), Selectable, fromString, toSelect)

newtype RawExpr a = RawExpr ParamSQL

class SQLExpressable i where
  convertToSQL :: i -> ParamSQL
  withType :: Proxy i -> ParamSQL -> ParamSQL

toTypedSQLExpression :: forall i o. (SQLExpression i o) => i -> ParamSQL
toTypedSQLExpression i = case (convertToSQL i) of
                              (ParamSQL [Param v]) -> case (typeSQL (Proxy :: Proxy i)) of
                                       Nothing -> ParamSQL [Param v]
                                       Just s -> ParamSQL [TypedParam v s]
                              p -> p


instance sqlExpressableImpl :: (SQLExpression i o, FromSQLValue o) => SQLExpressable i where
  convertToSQL = toSQLExpression
  withType _ (ParamSQL [Param v]) = case (typeSQL (Proxy :: Proxy i)) of
                                       Nothing -> ParamSQL [Param v]
                                       Just s -> ParamSQL [TypedParam v s]
  withType _ p = p

class (SQLExpressable i, FromSQLValue o) <= SQLExpression i o | i -> o where
  toSQLExpression :: i -> ParamSQL
  typeSQL :: Proxy i -> Maybe String

instance stringSQLExpression:: SQLExpression String String where
       toSQLExpression v = ParamSQL [Param $ toSQLValue v]
       typeSQL _ = Just "VARCHAR"

instance booleanSQLExpression:: SQLExpression Boolean Boolean where
       toSQLExpression v = ParamSQL [Param $ toSQLValue v]
       typeSQL _ = Just "BOOLEAN"

instance charSQLExpression:: SQLExpression Char Char where
       toSQLExpression v = ParamSQL [Param $ toSQLValue v]
       typeSQL _ = Just "CHAR"

instance intSQLExpression:: SQLExpression Int Int where
       toSQLExpression v = ParamSQL [Param $ toSQLValue v]
       typeSQL _ = Just "INT"

instance maybeSQLExpression
  :: (SQLExpression a a, FromSQLValue a) => SQLExpression (Maybe a) (Maybe a) where
       toSQLExpression Nothing = ParamSQL [Param null]
       toSQLExpression (Just a) = convertToSQL a
       typeSQL m = typeSQL (Proxy :: Proxy a)

instance rawExprSQLExpressionHelper
  :: (FromSQLValue o) => SQLExpression (RawExpr o) o where
       toSQLExpression (RawExpr p) = p
       typeSQL _ = Nothing

instance selectableSQLExpressionHelper
  :: ( ColumnType t i o
     , FromSQLValue o
     , IsSymbol col
     ) => SQLExpression (Selectable col t) o where
       toSQLExpression = toSelect
       typeSQL _ = Nothing

class (SQLExpressable i, FromSQLValue o) <= ColumnType cd i o | cd -> i, cd -> o where
  createType :: Proxy cd -> String

strConcat :: forall i1 i2
  . SQLExpression i1 String
 => SQLExpression i2 String
 => i1 -> i2 -> RawExpr String
strConcat a b = RawExpr $ toSQLExpression a <> ParamSQL [Raw " || "] <> toSQLExpression b

infixl 6 strConcat as .<>

orDefault :: forall i1 i2 o
  . SQLExpression i1 (Maybe o)
 => SQLExpression i2 o
 => i1 -> i2 -> RawExpr o
orDefault a b = RawExpr $
  fromString "COALESCE(" <>
  toSQLExpression a <>
  fromString ", " <>
  toSQLExpression b <>
  fromString ")"


class RealSQLExpression a b | a -> b where
  toSQLExpression' :: a -> ParamSQL

instance realToSQLExpression :: (SQLExpression a b) => RealSQLExpression a b where
  toSQLExpression' = toSQLExpression



