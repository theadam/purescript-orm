module RecordList where

data NilRecordList = NilRecordList
data RecordList h t = RecordList h t

class RecordListable h t r | t -> r where
  appendRecordList :: h -> t -> RecordList h r

instance bothRecRecList
  :: RecordListable (Record i) (Record j) (RecordList (Record j) NilRecordList) where
    appendRecordList i j = RecordList i (RecordList j NilRecordList)

instance leftRecInsertListable
  :: RecordListable (Record i) (RecordList h t) (RecordList h t) where
    appendRecordList i j = RecordList i j

infixr 6 appendRecordList as &

