import Data.Array (array, Array, (//), (!))
import qualified Data.List as List (find, length, delete)
import Data.Hashable
import Data.Maybe

--  Tipos de hashing comunes
--      Separate chaining       X
--      Open adressing
--          Linear probing
--          Quadratic probing
--          Double hashing

data HashTable a b = HashTable (Array Int [(a, b)])
  deriving (Show, Eq)

getTable :: HashTable a b -> Array Int [(a,b)]
getTable (HashTable arr) = arr

empty :: Eq a => Int -> HashTable a b
empty n = HashTable (array(0, n-1) [(i,[]) | i <- [0..n-1]])

isEmpty :: (Eq a, Eq b) => HashTable a b -> Bool
isEmpty t@(HashTable arr) = t == empty (length arr)

put :: (Hashable a, Eq a, Eq b) => (a, b) -> HashTable a b -> HashTable a b
put (k, v) (HashTable arr) = if containsKey k (HashTable arr) 
                                then let removeKeyd = getTable (removeKey k (HashTable arr))
                                     in (HashTable (removeKeyd // [(i, (k,v) : (removeKeyd ! i))])) 
                                else (HashTable (arr // [(i, (k,v) : (arr ! i))]))
  where i = hashSChaining (List.length arr) k 
        --xs = (k,v) : (arr ! i)

entries :: HashTable a b -> [(a,b)]
entries (HashTable arr) = concat [arr!i | i<-[1..n-1]]
    where n = length arr

keys :: HashTable a b -> [a]
keys (HashTable arr) = concat [map fst entries | i<-[1..(length arr)-1], let entries = arr!i]
    where n = length arr

values :: HashTable a b -> [b]
values (HashTable arr) = concat [map snd entries | i<-[1..(length arr)-1], let entries = arr!i]
    where n = length arr

containsKey :: (Hashable a, Eq a) => a -> HashTable a b-> Bool
containsKey k (HashTable arr) = if isNothing (getValue k (HashTable arr)) then False else True

containsValue :: Eq b => b -> HashTable a b -> Bool
containsValue v t@(HashTable arr) = any (==v) (values t)
    
getValue :: (Hashable k, Eq k) => k -> HashTable k v -> Maybe (k, v)
getValue key (HashTable table) =
  List.find (\(k,v) -> k == key) bucket
  where
    position = hashSChaining (List.length table) key
    bucket = table ! position

clear :: Eq a => HashTable a b -> HashTable a b
clear t@(HashTable arr) = empty (length arr)

size :: HashTable a b -> Int
size t@(HashTable arr) = length arr

-- merge?

-- mirar como aumentar los tamaños para una lista nueva
putAll :: HashTable a b -> HashTable a b -> HashTable a b
putAll t1@(HashTable arr1) t2@(HashTable arr2) = undefined

replace :: (Hashable a, Eq a, Eq b) => (a, b) -> HashTable a b -> HashTable a b
replace (k, v) t = put (k, v) t

removeKey :: (Hashable a, Eq a, Eq b) => a -> HashTable a b -> HashTable a b
removeKey key (HashTable arr) = 
  case getValue
 key (HashTable arr) of
    Nothing -> (HashTable arr)
    Just (k,v) -> 
      HashTable (arr // [(i, newxs)])
      where
        i = hashSChaining (List.length arr) key
        xs = arr ! i
        newxs = List.delete (k,v) xs

hashSChaining :: Hashable a => Int -> a -> Int
hashSChaining n = (`mod` n) . hash

t1, t2 :: HashTable String [Int]
t1 = HashTable (array (0,9) [(0,[]),(1,[]),(2,[]),(3,[]),(4,[]),(5,[]),(6,[]),(7,[("Paco",[1,2])]),(8,[]),(9,[])])
t2 = HashTable (array (0,9) [(0,[]),(1,[]),(2,[]),(3,[]),(4,[]),(5,[]),(6,[]),(7,[]),(8,[]),(9,[])])