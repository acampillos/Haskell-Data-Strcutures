module DataStructures.HashTable.HashTableLProbing(
    HashTableLP(..),
    getTable,
    empty,
    isEmpty,
    put,
    getValue,
    replace,
    removeKey,
    putAll,
    hashSChaining,
    resize,
    entries,
    keys,
    values,
    containsKey,
    containsValue,
    getNumPairs,
    clear,
    size,
    printHT
) where

import Data.Array (array, Array, (//), (!))
import qualified Data.List as List (find, length, delete)
import Data.Hashable
import Data.Maybe

-- LINEAR PROBING (OPEN ADDRESSING):
-- Trata de evitar las colisiones entre elementos que se insertan a la tabla con
-- posibles valores para la función de hash iguales probando primeramente la posición
-- determinada por la función de hash y si esta está ocupada vamos probando en 
-- los siguientes buckets.
--
--  hash(x) % |tabla|, (hash(x)+1) % |tabla|, (hash(x)+2) % |tabla|, ...
--

-----------------------
-- CONSTRUCTORES
-----------------------
-- Lo implemenntamos mediante un array que contendrá para cada indice del mismo
-- una lista enlazada con los pares que se inserten y tengan como valor de hash
-- el índice del array.
-- Llamaermos a cada una de estas listas enlazadas buckets.
-- Igualmente guardaremos el número de pares contenidos en la tabla con el fin 
-- de hacer más eficientes los cálculos asociados al aumento/disminución del
-- tamaño de la tabla.
--
--        HashTableLP = |pares contenidos| [indice bucket, [pares en el bucket]]
--
data HashTableLP a b = HashTableLP Int (Array Int [(a, b)])
  deriving (Show, Eq)


-----------------------
-- FUNCIONES
-----------------------

getTable :: HashTableLP a b -> Array Int [(a,b)]
getTable (HashTableLP _ arr) = arr

empty :: Int -> HashTableLP a b
empty n = HashTableLP 0 (array (0, n-1) [(i,[]) | i <- [0..n-1]])

isEmpty :: (Eq a, Eq b) => HashTableLP a b -> Bool
isEmpty t@(HashTableLP _ arr) = t == empty (length arr)

put :: (Hashable a, Eq a, Eq b) => (a, b) -> HashTableLP a b -> HashTableLP a b
-- Inserta el par (a, b) en la tabla.
-- Parámetros: Par clave-valor
--             Tabla en la que se inserta
-- Devuelve:   Tabla con el par insertado

-- Se encarga de hacer un aumento/disminución de la tabla y posteriormente realiza la inserción.
put (k, v) t@(HashTableLP pairs arr) = if (fromIntegral pairs) >= ((fromIntegral buckets) / 2)
                                     then let resized = resize (2*buckets) t 
                                          in put' (k,v) resized 
                                     else put' (k, v) t
    where buckets = length arr

-- Inserta el par en la tabla
put' (k, v) t@(HashTableLP pairs arr) = if containsKey k t 
                                then let removedKey = getTable (removeKey k t)
                                     in (HashTableLP pairs (removedKey // [(i, (k,v) : (removedKey ! i))])) 
                                else (HashTableLP (pairs+1) (arr // [(i, (k,v) : (arr ! i))]))
  where i = hashLProbing t k 
        
getValue :: (Hashable k, Eq k) => k -> HashTableLP k v -> Maybe (k, v)
-- Obtiene el valor asociado a la clave que recibe en la tabla correspondiente.
-- Parámetros: Clave de la que queremos obtener el valor.
--             Tabla de la que lo obtenemos.
-- Devuelve:   Par clave-valor

-- Recorremos el conjunto de buckets desde la posición obtenida por la función de hash filtrando
-- aquellos que contengan la clave que buscamos (solo será uno de ellos).
-- Si el resultado de la búsqueda es nulo el elemento no se encuentra en la tabla.
getValue key t@(HashTableLP _ table) = if null result then Nothing else head result
  where position = hashSChaining (List.length table) key
        result = filter (not . isNothing) 
          (map (List.find (\(k,v) -> k == key)) 
            ([bucket | i<-[position..(length table)-1], let bucket = table!i] ++ [bucket | i<-[0..position-1], let bucket = table!i]))

replace :: (Hashable a, Eq a, Eq b) => (a, b) -> HashTableLP a b -> HashTableLP a b
replace (k, v) t = put (k, v) t

removeKey :: (Hashable a, Eq a, Eq b) => a -> HashTableLP a b -> HashTableLP a b
-- Elimina el par asociado a la clave en la tabla.
-- Parámetros: Clave.
--             Tabla.
-- Devuevle:   Tabla con el par eliminado.

-- Se encarga de hacer un aumento/disminución de la tabla tras la eliminación del par
removeKey key t@(HashTableLP _ table) = if pairs > 0 && (fromIntegral pairs) <= (fromIntegral buckets / 8) then resize (div buckets 2) removed else removed
    where removed = removeKey' key t
          pairs = getNumPairs removed
          buckets = length (getTable removed)

-- Elimina el par de la tabla si este existe
removeKey' key t@(HashTableLP pairs table) = 
  case getValue key t of
    Nothing -> t
    Just (k,v) -> HashTableLP (pairs-1) (table // [(position', bucket')])
      where position = hashSChaining (List.length table) key
            buckets = [(i,bucket) | i<-[position..(length table)-1], let bucket = table!i] ++ 
              [(i,bucket) | i<-[0..position-1], let bucket = table!i]
            indexedBucket = filter (\(i,xs) -> any (\(ke, val) -> ke==key) xs) buckets
            position' = fst (head indexedBucket)
            bucket = table ! position'
            bucket' = List.delete (k,v) bucket
-- Calculamos la función de hash y tomamos todos los buckets desde esa posición hasta el final de la tabla y desde el principio.
-- Obtenemos el bucket en el que se encuentra nuestro elemento y lo eliminamos del mismo.
-- Finalmente, reemplazamos el bucket en la tabla por el bucket con el elemento eliminado.

putAll :: (Hashable a, Eq a, Eq b) => HashTableLP a b -> HashTableLP a b -> HashTableLP a b
-- Inserta todos los pares de la segunda tabla recibida en la primera
-- Parámetros: Tabla 1
--             Tabla 2
-- Devuelve:   Tabla 1 con los nuevos pares
putAll t1 t2 = foldr (\(k,v) ac -> put (k,v) ac) t1 entrySet
    where entrySet = entries t2

hashSChaining :: Hashable a => Int -> a -> Int
-- Calcula la función de hash a partir de n (longitud de la tabla)
hashSChaining n = (`mod` n) . hash

hashLProbing :: Hashable a => HashTableLP a b -> a -> Int
-- Calcula el índice del siguiente bucket vacío a partir del hash del elemento recibido y el tamaño de la tabla.
-- Parámetros: Tabla hash
--             Valor del que queremos el hash
-- Devuelve:   Índice con bucket vacío
hashLProbing t@(HashTableLP _ arr) v
    | null (arr ! i) = i
    | otherwise = fst (head (filter (\(_, b) -> null b) ([(j',arr!j') | j<-[i..n-1], let j' = mod j n] ++ [(j',arr!j') | j<-[0..i], let j' = mod j n])))
    where n = length arr
          i = hashSChaining n v

resize :: (Hashable a, Eq a, Eq b) => Int -> HashTableLP a b -> HashTableLP a b
-- Cambia el tamaño de la tabla insertando todos los elementos en una vacía con la capacidad indicada
resize capacity t = putAll (empty capacity) t

entries :: HashTableLP a b -> [(a,b)]
entries (HashTableLP _ arr) = concat [arr!i | i<-[0..n-1]]
    where n = length arr

keys :: HashTableLP a b -> [a]
keys (HashTableLP _ arr) = concat [map fst entries | i<-[1..(length arr)-1], let entries = arr!i]
    where n = length arr

values :: HashTableLP a b -> [b]
values (HashTableLP _ arr) = concat [map snd entries | i<-[1..(length arr)-1], let entries = arr!i]
    where n = length arr

containsKey :: (Hashable a, Eq a) => a -> HashTableLP a b-> Bool
containsKey k t@(HashTableLP _ arr) = if isNothing (getValue k t) then False else True

containsValue :: Eq b => b -> HashTableLP a b -> Bool
containsValue v t@(HashTableLP _ arr) = any (==v) (values t)

getNumPairs :: HashTableLP a b -> Int
getNumPairs (HashTableLP pairs table) = pairs

clear :: Eq a => HashTableLP a b -> HashTableLP a b
clear t@(HashTableLP _ arr) = empty (length arr)

size :: HashTableLP a b -> Int
size t@(HashTableLP _ arr) = length arr

-- merge?


printHT :: (Show a, Show b) => HashTableLP a b -> String
printHT t@(HashTableLP pairs table) = linea ++ header ++ linea ++ contenido ++ linea
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

t1 :: HashTableLP String [Int]
t1 = HashTableLP 1 (array (0,9) [(0,[]),(1,[]),(2,[]),(3,[]),(4,[]),(5,[]),(6,[]),(7,[("Paco",[1,2])]),(8,[]),(9,[])])
-- test doubling size by inserting pair
t2 :: HashTableLP Int Int
t2 = HashTableLP 3 (array (0,5) [(0,[(1,0)]),(1,[(2,0)]),(2,[(3,0)]),(3,[]),(4,[]),(5,[])])
-- test halving sive by removing pair
t3 :: HashTableLP Int Int
t3 = HashTableLP 1 (array (0,9) [(0,[(1,0)]),(1,[]),(2,[]),(3,[]),(4,[]),(5,[]),(6,[]),(7,[]),(8,[]),(9,[])])