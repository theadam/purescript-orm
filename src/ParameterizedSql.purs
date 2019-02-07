module ParameterizedSql where

import Prelude

import Data.Array (cons, intercalate)
import Data.List (List(..), (:), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Foreign (Foreign, unsafeToForeign)
import Type.Prelude (Proxy(..))
import Utils (null)

data Value
 = StringValue String
 | IntValue Int
 | NumberValue Number
 | NullValue

valueToForeign :: Value -> Foreign
valueToForeign (StringValue a) = unsafeToForeign a
valueToForeign (IntValue a) = unsafeToForeign a
valueToForeign (NumberValue a) = unsafeToForeign a
valueToForeign NullValue = null

class ToValue a where
  toValue :: a -> Value
  sqlType :: Proxy a -> String

instance toValueString :: ToValue String where
  toValue = StringValue
  sqlType _ = "VARCHAR"

else instance toValueInt :: ToValue Int where
  toValue = IntValue
  sqlType _ = "INT"

else instance toValueNumber :: ToValue Number where
  toValue = NumberValue
  sqlType _ = "NUMBER"

else instance toValueMaybe :: (ToValue a) => ToValue (Maybe a) where
  toValue (Just a) = toValue a
  toValue _ = NullValue
  sqlType _ = sqlType (Proxy :: Proxy a)

instance showValue :: Show Value where
  show (StringValue s) = show s
  show (IntValue i) = show i
  show (NumberValue n) = show n
  show NullValue = "null"

data SqlPart = StringPart String | Param Value | TypedParam Value String
data ParameterizedSql = ParameterizedSql (Array SqlPart)

mapParts :: (SqlPart -> SqlPart) -> ParameterizedSql -> ParameterizedSql
mapParts fn (ParameterizedSql parts) = ParameterizedSql $ fn <$> parts

instance semigroupParameterizedSql :: Semigroup ParameterizedSql where
  append (ParameterizedSql a) (ParameterizedSql b) = ParameterizedSql (a <> b)

instance monoidParameterizedSql :: Monoid ParameterizedSql where
  mempty = ParameterizedSql []

class ToParameterizedSql s where
  toSql :: s -> ParameterizedSql

instance toParameterizedSqlParameterizedSql :: ToParameterizedSql ParameterizedSql where
  toSql value = value

else instance toParameterizedSqlString :: ToParameterizedSql String where
  toSql value = ParameterizedSql ([StringPart value])

else instance toParameterizedSqlSqlPart :: ToParameterizedSql SqlPart where
  toSql value = ParameterizedSql ([value])

else instance toParameterizedSqlValue :: ToParameterizedSql Value where
  toSql value = ParameterizedSql ([Param $ value])

class AddToParameterizedSql a b where
  addTo :: a -> b -> ParameterizedSql

instance addToParameterizedSqlToParameterizedSql :: (
    ToParameterizedSql a,
    ToParameterizedSql b
  ) => AddToParameterizedSql a b where
  addTo a b = toSql a <> toSql b

infixr 5 addTo as :<>:

sqlJoin :: forall a. (ToParameterizedSql a) => String -> Array a -> ParameterizedSql
sqlJoin str ary = intercalate (ParameterizedSql $ [StringPart str]) $ toSql <$> ary

commaJoin :: forall a. (ToParameterizedSql a) => Array a -> ParameterizedSql
commaJoin = sqlJoin ", "

listify :: forall a. (ToParameterizedSql a) => Array a -> ParameterizedSql
listify ary = "(" :<>: commaJoin ary :<>: ")"

instance showSQLPart :: Show SqlPart where
  show (StringPart p) = p
  show (Param f) = "[" <> show f <> "]"
  show (TypedParam f _) = show $ Param f

sqlTupleToString :: Tuple String (Array Value) -> String
sqlTupleToString t = fst t <> "\n\t" <> "With Params: (" <> params <> ")"
  where
    params = intercalate ", " $ show <$> snd t

sqlToString :: (ParameterizedSql -> Tuple String (Array Value)) -> ParameterizedSql -> String
sqlToString r p = sqlTupleToString realized
    where
      realized = r p

instance showParameterizedSql :: Show ParameterizedSql where
  show p = sqlToString defaultRealize p

positionalToParam :: Int -> String
positionalToParam i = "$" <> show i

questionMarkToParam :: Int -> String
questionMarkToParam _ = "?"

realize :: (Int -> String) -> ParameterizedSql -> Tuple String (Array Value)
realize toParam (ParameterizedSql a) = realize' toParam 1 $ fromFoldable a

realize' :: (Int -> String) -> Int -> List SqlPart -> Tuple String (Array Value)
realize' toParam i (StringPart str:rest) = Tuple (str <> rStr) (rParams)
  where
    Tuple rStr rParams = realize' toParam i rest
realize' toParam i (Param f:rest) = Tuple (toParam i <> rStr) (cons f rParams)
  where
    Tuple rStr rParams = realize' toParam (i + 1) rest
realize' toParam i (TypedParam f s:rest) = Tuple ("CAST(" <> toParam i <> " AS " <> s <> ")" <> rStr) (cons f rParams)
  where
    Tuple rStr rParams = realize' toParam (i + 1) rest
realize' _ _ Nil = Tuple "" []

defaultRealize :: ParameterizedSql -> Tuple String (Array Value)
defaultRealize = realize questionMarkToParam
