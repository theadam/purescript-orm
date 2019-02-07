module Expression where

import Prelude hiding (eq)

import Control.Monad.Writer (Writer, tell, execWriter)
import Data.Array (length)
import Data.Maybe (Maybe)
import ParameterizedSql (class ToValue, ParameterizedSql(..), SqlPart(..), sqlJoin, sqlType, toSql, toValue, (:<>:))
import Type.Prelude (Proxy(..))

class Expression t o | t -> o where
  representation :: t -> ParameterizedSql

data ColumnAlias o = ColumnAlias String String

data RealizedExpression o = RealizedExpression ParameterizedSql

type BoolExp = RealizedExpression Boolean

data Raw o = Raw String

instance columnAliasExpression :: Expression (ColumnAlias o) o where
  representation (ColumnAlias table field) = toSql $ table <> "." <> field
else instance realizedExpression :: Expression (RealizedExpression o) o where
  representation (RealizedExpression o) = o
else instance rawExpression :: Expression (Raw o) o where
  representation (Raw o) = toSql o
else instance valueExpression :: (ToValue a) => Expression a a where
  representation a = ParameterizedSql [TypedParam (toValue a) (sqlType (Proxy :: Proxy a)) ]

realize :: forall a o. Expression a o => a -> RealizedExpression o
realize a = RealizedExpression $ representation a

and :: BoolExp -> BoolExp -> BoolExp
and (RealizedExpression a) (RealizedExpression b) = RealizedExpression $ a :<>: " AND " :<>: b

infixl 3 and as :&&

or :: BoolExp -> BoolExp -> BoolExp
or (RealizedExpression a) (RealizedExpression b) = RealizedExpression $ a :<>: " OR " :<>: b

infixl 2 or as :||

eq :: forall a b o. Expression a o => Expression b o => a -> b -> BoolExp
eq a b = RealizedExpression $ representation a :<>: " = " :<>: representation b

infixl 4 eq as :==

ne :: forall a b o. Expression a o => Expression b o => a -> b -> BoolExp
ne a b = RealizedExpression $ representation a :<>: " != " :<>: representation b

infixl 4 ne as :!=
infixl 4 ne as :/=

lt :: forall a b o. Expression a o => Expression b o => a -> b -> BoolExp
lt a b = RealizedExpression $ representation a :<>: " < " :<>: representation b

infixl 4 lt as :<

lte :: forall a b o. Expression a o => Expression b o => a -> b -> BoolExp
lte a b = RealizedExpression $ representation a :<>: " <= " :<>: representation b

infixl 4 lt as :<=

gt :: forall a b o. Expression a o => Expression b o => a -> b -> BoolExp
gt a b = RealizedExpression $ representation a :<>: " > " :<>: representation b

infixl 4 lt as :>

gte :: forall a b o. Expression a o => Expression b o => a -> b -> BoolExp
gte a b = RealizedExpression $ representation a :<>: " >= " :<>: representation b

infixl 4 lt as :>=

isNull :: forall a o. Expression a (Maybe o) => a -> RealizedExpression Boolean
isNull a = RealizedExpression $ representation a :<>: " IS NULL"

isNotNull :: forall a o. Expression a (Maybe o) => a -> RealizedExpression Boolean
isNotNull a = RealizedExpression $ representation a :<>: " IS NOT NULL"

-- Can convert any non-nullable expression to a nullable one.  Good for comparisons
-- between nullable and non-nullable expressions.  This would be similar to `Just`
toNullable :: forall a o. Expression a o => a -> RealizedExpression (Maybe o)
toNullable a = RealizedExpression $ representation a

coalesce :: forall a b o. Expression a (Maybe o) => Expression b o => a -> b -> RealizedExpression o
coalesce a b = RealizedExpression $ toSql "COALESCE(" :<>: representation a :<>: ", " :<>: representation b :<>: ")"

infixr 3 coalesce as :<|>

concat :: forall a b. Expression a String => Expression b String => a -> b -> RealizedExpression String
concat a b = RealizedExpression $ toSql "(" :<>: representation a :<>: " || " :<>: representation b :<>: ")"

infixl 6 concat as :<>

data When o = When ParameterizedSql ParameterizedSql
data Default o = Default ParameterizedSql

when :: forall a b o. Expression a Boolean => Expression b o => a -> b -> Writer (Array (When o)) Unit
when a b = tell [When (representation a) (representation b)]

default :: forall b o. Expression b o => b -> Default o
default b = Default $ representation b

cond :: forall o. Writer (Array (When o)) Unit -> Default o -> RealizedExpression o
cond w (Default def) = case length whens of
  0 -> RealizedExpression def
  _ -> RealizedExpression $ toSql "CASE " :<>: realized :<>: " ELSE " :<>: def :<>: " END"
    where
      whens = execWriter w
      realizeWhen (When a b) = toSql "WHEN " :<>: a :<>: " THEN " :<>: b
      realized = sqlJoin " " (realizeWhen <$> whens)

null :: forall a o. Expression a o => RealizedExpression (Maybe o)
null = RealizedExpression $ toSql "NULL"
