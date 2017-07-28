module Defaults where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Record (LProxy(LProxy))
import Record as R
import Type.Data.Boolean (kind Boolean)
import Type.Data.Symbol (class Equals)
import Type.Prelude (BProxy(..), False, True)
import Type.Row (kind RowList, class RowToList, Cons, Nil)

class DefaultMaybes (ol :: RowList) (o :: # Type) | ol -> o where
  allNothings :: LProxy ol -> Record o
instance nilMaybes :: DefaultMaybes Nil () where
  allNothings _ = {}
instance consMaybes
  :: ( IsSymbol k
     , DefaultMaybes ol' o'
     , RowCons k (Maybe t) o' o
     ) => DefaultMaybes (Cons k (Maybe t) ol') o where
       allNothings _ = R.insert keyProxy Nothing (allNothings (LProxy :: LProxy ol'))
         where
           keyProxy = SProxy :: SProxy k

class RecMaybe (il :: RowList) (ol :: RowList) (i :: # Type) (o :: # Type)
  | ol -> o, il -> i where
    withDefaults' :: LProxy il -> LProxy ol -> Record i -> Record o

instance nilRecMaybe :: DefaultMaybes ol o => RecMaybe Nil ol () o where
  withDefaults' _ _ _ = allNothings (LProxy :: LProxy ol)

instance consMaybeRecMaybe
  :: ( IsSymbol k
     , Equals j k b
     , RecMaybeHelper (Cons j s il') (Cons k t ol') i o b
     ) => RecMaybe (Cons j s il') (Cons k t ol') i o where
  withDefaults' il ol r = withDefaultsHelper (BProxy :: BProxy b) il ol r

instance failRecMaybe
  :: ( IsSymbol k
     , Fail (TypeConcat (TypeConcat "'" k) "' is an invalid key for this record")
     ) => RecMaybe (Cons k t il') Nil i () where
       withDefaults' _ _ _ = {}

class RecMaybeHelper (il :: RowList) (ol :: RowList) (i :: # Type) (o :: # Type) (b :: Boolean) where
  withDefaultsHelper :: BProxy b -> LProxy il -> LProxy ol -> Record i -> Record o

instance hasRecMaybeHelper
  :: ( IsSymbol k
     , RecMaybe il' ol' i' o'
     , RowCons k t i' i
     , RowCons k t o' o
     ) => RecMaybeHelper (Cons k t il') (Cons k t ol') i o True where
  withDefaultsHelper _ _ _ r = R.insert keyProxy (R.get keyProxy r) rest
    where
      keyProxy = SProxy :: SProxy k
      rest = withDefaults' (LProxy :: LProxy il') (LProxy :: LProxy ol') (R.delete keyProxy r)

instance hasMaybeRecMaybeHelper
  :: ( IsSymbol k
     , RecMaybe il' ol' i' o'
     , RowCons k t i' i
     , RowCons k (Maybe t) o' o
     ) => RecMaybeHelper (Cons k t il') (Cons k (Maybe t) ol') i o True where
  withDefaultsHelper _ _ _ r = R.insert keyProxy (Just $ R.get keyProxy r) rest
    where
      keyProxy = SProxy :: SProxy k
      rest = withDefaults' (LProxy :: LProxy il') (LProxy :: LProxy ol') (R.delete keyProxy r)

instance doesntHaveRecMaybeHelper
  :: ( IsSymbol k
     , RecMaybe il ol' i o'
     , RowCons k (Maybe t) o' o
     ) => RecMaybeHelper il (Cons k (Maybe t) ol') i o False where
  withDefaultsHelper _ _ _ r = R.insert keyProxy Nothing rest
    where
      keyProxy = SProxy :: SProxy k
      rest = withDefaults' (LProxy :: LProxy il) (LProxy :: LProxy ol') r

class Defaults (i :: # Type) (o :: # Type) where
  withDefaults :: Record i -> Record o

instance defaultsImpl :: (RowToList i il, RowToList o ol, RecMaybe il ol i o) => Defaults i o where
  withDefaults i = withDefaults' (LProxy :: LProxy il) (LProxy :: LProxy ol) i
