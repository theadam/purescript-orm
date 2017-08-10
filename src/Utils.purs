module Utils where

import Prelude

import Data.Array (cons)
import Data.Foldable (intercalate)
import Data.Foreign (Foreign)
import Data.List (List(..), fromFoldable, (:))
import Data.Monoid (class Monoid)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))

data Selectable (n :: Symbol) t = Selectable String

data SQLPart = Raw String | Param Foreign | TypedParam Foreign String
data ParamSQL = ParamSQL (Array SQLPart)

fromString :: String -> ParamSQL
fromString s = ParamSQL $ [Raw s]

instance paramSQLSemigroup :: Semigroup ParamSQL where
  append (ParamSQL a) (ParamSQL b) = ParamSQL (a <> b)
instance paramSQLMonoit :: Monoid ParamSQL where
  mempty = ParamSQL []

toParts :: ParamSQL -> Array SQLPart
toParts (ParamSQL a) = a

joinParamSQLWith :: String -> Array ParamSQL -> ParamSQL
joinParamSQLWith s ary = ParamSQL $ intercalate [Raw s] $ toParts <$> ary

toSelect :: forall n t. IsSymbol n => Selectable n t -> ParamSQL
toSelect (Selectable alias) = ParamSQL
  [Raw $ alias <> "." <> reflectSymbol (SProxy :: SProxy n)]

realize :: ParamSQL -> Tuple String (Array Foreign)
realize (ParamSQL a) = realize' 1 $ fromFoldable a

realizeWithTypes :: ParamSQL -> Tuple String (Array Foreign)
realizeWithTypes (ParamSQL a) = realize' 1 $ fromFoldable a

realize' :: Int -> List SQLPart -> Tuple String (Array Foreign)
realize' i (Raw str:rest) = Tuple (str <> rStr) (rParams)
  where
    Tuple rStr rParams = realize' i rest
realize' i (Param f:rest) = Tuple (toParam i <> rStr) (cons f rParams)
  where
    Tuple rStr rParams = realize' (i + 1) rest
realize' i (TypedParam f s:rest) = Tuple (toParam i <> "::" <> s <> rStr) (cons f rParams)
  where
    Tuple rStr rParams = realize' (i + 1) rest
realize' i Nil = Tuple "" []

toParam :: Int -> String
toParam i = "$" <> show i
