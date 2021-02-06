{--import Data.Hashable

{--data HashTable a b = HT [(a,b)]
    deriving Show

insert :: Hashable a => HashTable a -> (a,b) -> HashTable a
insert (HT entries) (key, value) = HT (datentriesa ++ )
    where add = [(index, xs)]
          index = getHash (length entries) key
          xs = (key, value) : --}



import Data.Array (array, Array, (//), (!))
import qualified Data.List as List (find, length, delete)
import Data.Hashable
import Debug.Trace

data HashTable a b =
  HashTable (Array Int [(a, b)])
  deriving Show

mkHashTable :: Eq a => Int -> HashTable a b
mkHashTable n =
  HashTable (array(0,n-1) [(i,[]) | i <- [0..n-1]])

insert :: Hashable a => (a, b) -> HashTable a b -> HashTable a b
insert (k, v) t@(HashTable arr) = 
  if contains k t then t else HashTable (arr // [(i, xs)])
  where i = mHash (List.length arr) k 
        xs = (k,v) : (arr ! i)
    -- Hacer que no hayan claves repetidas

contains :: Eq k => k -> HashTable k v -> Bool
contains key t = if isNothing result then False else True
    where result = search key t
    
search :: (Hashable k, Eq k) => k -> HashTable k v -> Maybe (k, v)
search key (HashTable table) =
  List.find (\(k,v) -> k == key) bucket
  where
    position = mHash (List.length table) key
    bucket = table ! position

delete :: (Hashable a, Eq a, Eq b) => a -> HashTable a b -> HashTable a b
delete key (HashTable arr) =
  case search key (HashTable arr) of
    Nothing -> (HashTable arr)
    Just (k,v) -> 
      HashTable (arr // [(i, newxs)])
      where
        i = mHash (List.length arr) key
        xs = arr ! i
        newxs = List.delete (k,v) xs

mHash :: Hashable a => Int -> a -> Int
mHash n =
  (`mod` n) . hash

(|>) = flip ($)

-- El que tenemos CREO que es separate chaining
-- hacer open addressing--}