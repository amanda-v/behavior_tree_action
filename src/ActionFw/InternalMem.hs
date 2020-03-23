module ActionFw.InternalMem where

import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H

data Memory = SMem String | IMem Int

type InternalMem = [String]

{--
type InternalMem s String v = C.HashTable s k v

foo :: ST s (HashTable s k v)
foo = do
    ht <- H.new
    H.insert ht 1 1
    return ht
--}