module SQLExpression where

import Prelude

import Column (class ColumnType)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)
import Database.PostgreSQL (class FromSQLValue, toSQLValue)
import Utils (ParamSQL(..), SQLPart(..), Selectable, fromString, toSelect)

newtype RawExpr a = RawExpr ParamSQL

class FromSQLValue o <= SQLExpression i o | i -> o where
  toSQLExpression :: i -> ParamSQL
  toTypedSQLExpression :: i -> ParamSQL

instance stringSQLExpressionHelper :: SQLExpression String String where
       toSQLExpression v = ParamSQL [Param $ toSQLValue v]
       toTypedSQLExpression v = ParamSQL [TypedParam (toSQLValue v) "VARCHAR"]

instance booleanSQLExpressionHelper :: SQLExpression Boolean Boolean where
       toSQLExpression v = ParamSQL [Param $ toSQLValue v]
       toTypedSQLExpression v = ParamSQL [TypedParam (toSQLValue v) "BOOLEAN"]

instance charSQLExpressionHelper :: SQLExpression Char Char where
       toSQLExpression v = ParamSQL [Param $ toSQLValue v]
       toTypedSQLExpression v = ParamSQL [TypedParam (toSQLValue v) "CHAR"]

instance intSQLExpressionHelper :: SQLExpression Int Int where
       toSQLExpression v = ParamSQL [Param $ toSQLValue v]
       toTypedSQLExpression v = ParamSQL [TypedParam (toSQLValue v) "INT"]

instance rawExprSQLExpressionHelper
  :: (FromSQLValue o) => SQLExpression (RawExpr o) o where
       toSQLExpression (RawExpr p) = p
       toTypedSQLExpression (RawExpr p) = p

instance selectableSQLExpressionHelper
  :: ( ColumnType t i o
     , FromSQLValue o
     , IsSymbol col
     ) => SQLExpression (Selectable col t) o where
       toSQLExpression = toSelect
       toTypedSQLExpression = toSelect

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



