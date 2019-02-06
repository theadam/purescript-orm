module Defaultable where

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.RowList (class RowToList, kind RowList, Nil, Cons)
import Prim.TypeError (class Fail, Text, Beside, kind Doc)
import Record (get, insert, merge)
import Type.Data.Boolean (kind Boolean, True, False)
import Type.Data.Ordering (LT, class Equals)
import Type.Data.RowList (RLProxy(..))
import Type.Data.Symbol (class Compare)
import Type.Equality (class TypeEquals, to)
import Type.Prelude (Proxy(..))
import Type.Row as Row
import Unsafe.Coerce (unsafeCoerce)


withDefaults :: forall d f r. Defaultable d f r => d -> f -> r
withDefaults = withDefaultsImpl (Proxy :: Proxy r)

withDefaultNothings :: forall f r. DefaultNothings f r => f -> r
withDefaultNothings = withDefaultsImpl proxy {}
  where
    proxy :: Proxy r
    proxy = Proxy


-- Supporting classes
-- Give some defaults then some fields, produce a final record
class Defaultable defaults fields record
   where
     withDefaultsImpl :: Proxy record -> defaults -> fields -> record

class Defaultable (Record ()) fields record <= DefaultNothings fields record

instance defaultNothingsDefaultable :: (Defaultable (Record ()) f r) => DefaultNothings f r

instance defaultableFromRL ::
    (
      FillDefaults (Record r) (Record ns),
      Row.Union d ns dns',
      Row.Nub dns' dns,
      Row.Union f dns f1,
      Row.Nub f1 f2,
      RowToList f2 f2RL,
      RowToList r rRL,
      RealizedFields f2RL rRL (Record f2) (Record r)
    ) => Defaultable (Record d) (Record f) (Record r)
      where
        withDefaultsImpl _ d f = realizeDefaults (RLProxy :: RLProxy f2RL) (RLProxy :: RLProxy rRL) ((merge f (merge d (nothings (Proxy :: Proxy (Record r))))) :: (Record f2))

-- Can take some fields like { a: "test", b: Nothing, c: "a" } and have it become record:
-- { a: String, b: Maybe String, c: Maybe String }.  Turning c: "a" into c: Just "a".
class RealizedFields (fieldsL :: RowList) (recordL :: RowList) fields record
    where
      realizeDefaults :: RLProxy fieldsL -> RLProxy recordL -> fields -> record

instance emptyRealizedFields :: (TypeEquals (Record ()) r) => RealizedFields Nil Nil f r where
    realizeDefaults _ _ _ = to {}

else instance mayRealizedFields ::
  (
    IsSymbol k,
    Row.Cons k v f' f,
    Row.Cons k (Maybe v) r' r,
    RealizedFields fl rl (Record f) (Record r'),
    Row.Lacks k r'
  ) =>
  RealizedFields (Cons k v fl) (Cons k (Maybe v) rl) (Record f) (Record r) where
    realizeDefaults _ _ f = insert key (Just (get key f)) (realizeDefaults flProxy rlProxy f)
        where
          key :: SProxy k
          key = SProxy
          flProxy :: RLProxy fl
          flProxy = RLProxy
          rlProxy :: RLProxy rl
          rlProxy = RLProxy

else instance otherRealizedFields ::
  (
    IsSymbol k,
    Row.Cons k v f' f,
    Row.Cons k v r' r,
    RealizedFields fl rl (Record f) (Record r'),
    Row.Lacks k r'
  ) =>
  RealizedFields (Cons k v fl) (Cons k v rl) (Record f) (Record r) where
    realizeDefaults _ _ f = insert key (get key f) (realizeDefaults flProxy rlProxy f)
        where
          key :: SProxy k
          key = SProxy
          flProxy :: RLProxy fl
          flProxy = RLProxy
          rlProxy :: RLProxy rl
          rlProxy = RLProxy
else instance rightNilReailizedFields ::
    (
        Fail
          (Beside
            (Beside (Text "Record contains an invalid field \"") (Text k))
            (Text "\""))
    ) =>
    RealizedFields (Cons k v fl) Nil c d where
      realizeDefaults _ _ f = unsafeCoerce {}
else instance leftNilReailizedFields ::
    (
        Fail
          (Beside
            (Beside (Text "Record is missing field \"") (Text k))
            (Text "\""))
    ) =>
    RealizedFields Nil (Cons k v fl) c d where
      realizeDefaults _ _ f = unsafeCoerce {}
else instance failReailizedFields ::
    (
        Compare k2 k ord,
        Equals ord LT isLt,
        IfDoc isLt
          (Beside
            (Beside (Text "Record contains an invalid field \"") (Text k2))
            (Text "\""))
          (Beside
            (Beside (Text "Record is missing field \"") (Text k))
            (Text "\""))
          doc,
        Fail doc
    ) =>
    RealizedFields (Cons k2 v2 fl2) (Cons k v fl) c d where
      realizeDefaults _ _ f = unsafeCoerce {}

-- Defaults
class FillDefaults record nothings | record -> nothings where
    nothings :: Proxy record -> nothings

instance defNothingsRow ::
    (RowToList record recordRL, FillDefaultsRL recordRL nothings) =>
    FillDefaults (Record record) nothings
    where
      nothings _ = nothingsRL (RLProxy :: RLProxy recordRL)

class FillDefaultsRL (recordRL :: RowList) nothings | recordRL -> nothings where
    nothingsRL :: RLProxy recordRL -> nothings

instance emptyFillDefaultsRL :: FillDefaultsRL Nil (Record ()) where
    nothingsRL _ = {}

else instance defaultNothingsMaybeRL ::
    (FillDefaultsRL rest (Record c), Row.Cons k (Maybe v) c nothings, Row.Lacks k c, IsSymbol k) =>
    FillDefaultsRL (Cons k (Maybe v) rest) (Record nothings)
        where
          nothingsRL _ = insert (SProxy :: SProxy k) Nothing (nothingsRL (RLProxy :: RLProxy rest))

else instance defaultBooleanRL ::
    (FillDefaultsRL rest (Record c), Row.Cons k Boolean c nothings, Row.Lacks k c, IsSymbol k) =>
    FillDefaultsRL (Cons k Boolean rest) (Record nothings)
        where
          nothingsRL _ = insert (SProxy :: SProxy k) false (nothingsRL (RLProxy :: RLProxy rest))

else instance defaultNothingsRL ::
    (FillDefaultsRL rest c) =>
    FillDefaultsRL (Cons k v rest) c
        where
          nothingsRL _ = nothingsRL (RLProxy :: RLProxy rest)

class IfDoc (bool :: Boolean)
         (onTrue :: Doc)
         (onFalse :: Doc)
         (output :: Doc) |
         bool onTrue onFalse -> output
instance ifTrue :: IfDoc True onTrue onFalse onTrue
instance ifFalse :: IfDoc False onTrue onFalse onFalse
