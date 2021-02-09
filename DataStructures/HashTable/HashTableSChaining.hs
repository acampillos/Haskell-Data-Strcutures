import Data.Array (array, Array, (//), (!))
import qualified Data.List as List (find, length, delete)
import Data.Hashable
import Data.Maybe

--  Tipos de hashing comunes
--      Separate chaining       <
--      Open adressing          
--          Linear probing      
--          Quadratic probing   
--          Double hashing      

--                           |pares|, tabla
data HashTable a b = HashTable Int (Array Int [(a, b)])
  deriving (Show, Eq)

getTable :: HashTable a b -> Array Int [(a,b)]
getTable (HashTable _ arr) = arr

empty :: Eq a => Int -> HashTable a b
empty n = HashTable 0 (array (0, n-1) [(i,[]) | i <- [0..n-1]])

isEmpty :: (Eq a, Eq b) => HashTable a b -> Bool
isEmpty t@(HashTable _ arr) = t == empty (length arr)

-- // double table size if 50% full
-- (n >= m/2)
put :: (Hashable a, Eq a, Eq b) => (a, b) -> HashTable a b -> HashTable a b
put (k, v) t@(HashTable pairs arr) = if pairs >= (div buckets 2) 
                                     then let resized = resize (2*buckets) t 
                                          in put' (k,v) resized 
                                     else put' (k, v) t
    where buckets = length arr

put' (k, v) t@(HashTable pairs arr) = if containsKey k t
                                    then let removedKey = getTable (removeKey k t)
                                         in (HashTable pairs (removedKey // [(i, (k,v) : (removedKey ! i))])) 
                                    else (HashTable (pairs+1) (arr // [(i, (k,v) : (arr ! i))]))
  where i = hashSChaining (List.length arr) k 

entries :: HashTable a b -> [(a,b)]
entries (HashTable _ arr) = concat [arr!i | i<-[0..n-1]]
    where n = length arr

keys :: HashTable a b -> [a]
keys (HashTable _ arr) = concat [map fst entries | i<-[1..(length arr)-1], let entries = arr!i]
    where n = length arr

values :: HashTable a b -> [b]
values (HashTable _ arr) = concat [map snd entries | i<-[1..(length arr)-1], let entries = arr!i]
    where n = length arr

containsKey :: (Hashable a, Eq a) => a -> HashTable a b-> Bool
containsKey k t@(HashTable _ arr) = if isNothing (getValue k t) then False else True

containsValue :: Eq b => b -> HashTable a b -> Bool
containsValue v t = any (==v) (values t)
    
getValue :: (Hashable k, Eq k) => k -> HashTable k v -> Maybe (k, v)
getValue key (HashTable _ table) =
  List.find (\(k,v) -> k == key) bucket
  where
    position = hashSChaining (List.length table) key
    bucket = table ! position

clear :: Eq a => HashTable a b -> HashTable a b
clear t@(HashTable _ arr) = empty (length arr)

size :: HashTable a b -> Int
size t@(HashTable _ arr) = length arr

-- merge?

replace :: (Hashable a, Eq a, Eq b) => (a, b) -> HashTable a b -> HashTable a b
replace (k, v) t = put (k, v) t

-- // halves size of array if it's 12.5% full or less
-- (n > 0 && n <= m/8) 
removeKey :: (Hashable a, Eq a, Eq b) => a -> HashTable a b -> HashTable a b
removeKey key t@(HashTable _ arr) = if pairs > 0 && pairs <= (div buckets 8) then resize (div buckets 2) removed else removed
    where removed = removeKey' key t
          pairs = getNumPairs removed
          buckets = length (getTable removed)

getNumPairs :: HashTable a b -> Int
getNumPairs (HashTable pairs table) = pairs

removeKey' key t@(HashTable pairs arr) = 
  case getValue key t of
    Nothing -> t
    Just (k,v) -> (HashTable (pairs-1) (arr // [(i, newxs)]))
      where
        i = hashSChaining (List.length arr) key
        xs = arr ! i
        newxs = List.delete (k,v) xs

hashSChaining :: Hashable a => Int -> a -> Int
hashSChaining n = (`mod` n) . hash

--  newSize, oldHT, (numKeys, numValues) {no se si lo puedo sacar por dentro y ya}
resize :: (Hashable a, Eq a, Eq b) => Int -> HashTable a b -> HashTable a b
resize capacity t = putAll (empty capacity) t

putAll :: (Hashable a, Eq a, Eq b) => HashTable a b -> HashTable a b -> HashTable a b
putAll t1 t2 = foldr (\(k,v) ac -> put (k,v) ac) t1 entrySet
    where entrySet = entries t2

printHT :: (Show a, Show b) => HashTable a b -> String
printHT t@(HashTable pairs table) = linea ++ header ++ linea ++ contenido ++ linea
    where n = length table
          width = 90
          linea = (concat ["-" | i<-[0..width]]) ++ "\n"
          header = "| Bucket    | Entries (key-value pairs)                                                   |\n"
          leftPad = 11
          rightPad = 77
          contenido = concat ["| " ++ (show i) ++ (concat [" " | j<-[0..difLeft-1]]) ++ "| " ++ (procesaEntradas (show (table!i))) ++ " |\n" 
            | i<-[0..n-1], let difLeft = leftPad - (length (show i))-1]
          procesaEntradas cadena | nc > rightPad = (take (rightPad-2) cadena) ++ " |\n" ++ "|           | " ++ (procesaEntradas ((drop rightPad cadena) ++ (concat [" " | j<-[0..(rightPad - length (drop 36 cadena))]])))
                                 | otherwise = cadena ++ (concat [" " | j<-[0..difRight-3]])
                                 where nc = length cadena
                                       difRight = rightPad - nc

main :: IO ()
main = do
  putStr (printHT t6)

t1 :: HashTable String [Int]
t1 = HashTable 1 (array (0,9) [(0,[]),(1,[]),(2,[]),(3,[]),(4,[]),(5,[]),(6,[]),(7,[("Paco",[1,2])]),(8,[]),(9,[])])
-- test doubling size by inserting pair
t2 :: HashTable Int Int
t2 = HashTable 3 (array (0,5) [(0,[(1,0)]),(1,[(2,0)]),(2,[(3,0)]),(3,[]),(4,[]),(5,[])])
-- test halving sive by removing pair
t3 :: HashTable Int Int
t3 = HashTable 1 (array (0,9) [(0,[(1,0)]),(1,[]),(2,[]),(3,[]),(4,[]),(5,[]),(6,[]),(7,[]),(8,[]),(9,[])])

t4 :: HashTable Int Int
t4 = HashTable 0 (array (0,11) [(0,[]),(1,[]),(2,[]),(3,[]),(4,[]),(5,[]),(6,[]),(7,[]),(8,[]),(9,[]),(10,[]),(11,[])])

t5 :: HashTable Int Int
t5 = HashTable 3 (array (0,5) [(0,[(1,0),(1,0),(1,0),(1,0),(1,0),(1,0),(1,0),(1,0)]),(1,[(2,0)]),(2,[(3,0)]),(3,[]),(4,[]),(5,[])])

t6 :: HashTable Int String
t6 = HashTable 0 (array (0,11) [(0,[(1,"Alvaro Campillos Delgado Perez Espejo Benitez"), (2,"Pepe PAco Pedro Benito MAnuel"),
  (3,"Antonio Antonio Antonio Antonio Antonio Antonio Antonio Antonio ")]),(1,[]),(2,[]),(3,[(1,"Alvaro Campillos Delgado Perez Espejo Benitez"), (2,"Pepe PAco Pedro Benito MAnuel"),
  (3,"Antonio Antonio Antonio Antonio Antonio Antonio Antonio Antonio ")]),(4,[]),(5,[]),(6,[]),(7,[]),(8,[]),(9,[]),(10,[]),(11,[])])