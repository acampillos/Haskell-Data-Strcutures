import Data.Array (array, Array, (//), (!))
import qualified Data.List as List (find, length, delete)
import Data.Hashable
import Data.Maybe

-- QUADRATIC PROBING (OPEN ADDRESSING):
-- Trata de evitar las colisiones entre elementos que se insertan a la tabla con
-- posibles valores para la función de hash iguales probando primeramente la posición
-- determinada por la función de hash y si esta está ocupada vamos probando en 
-- los siguientes (i^2)-ésimos buckets.
--
--  hash(x) % |tabla|, (hash(x)+1*1) % |tabla|, (hash(x)+2*2) % |tabla|, ...
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
--        HashTable = |pares contenidos| [indice bucket, [pares en el bucket]]
--
data HashTable a b = HashTable Int (Array Int [(a, b)])
  deriving (Show, Eq)

-----------------------
-- FUNCIONES
-----------------------

getTable :: HashTable a b -> Array Int [(a,b)]
getTable (HashTable _ arr) = arr

empty :: Eq a => Int -> HashTable a b
empty n = HashTable 0 (array (0, n-1) [(i,[]) | i <- [0..n-1]])

isEmpty :: (Eq a, Eq b) => HashTable a b -> Bool
isEmpty t@(HashTable _ arr) = t == empty (length arr)

put :: (Hashable a, Eq a, Eq b) => (a, b) -> HashTable a b -> HashTable a b
-- Inserta el par (a, b) en la tabla.
-- Parámetros: Par clave-valor
--             Tabla en la que se inserta
-- Devuelve:   Tabla con el par insertado

-- Se encarga de hacer un aumento/disminución de la tabla y posteriormente realiza la inserción.
put (k, v) t@(HashTable pairs arr) = if (fromIntegral pairs) >= ((fromIntegral buckets) / 2)
                                     then let resized = resize (2*buckets) t 
                                          in put' (k,v) resized 
                                     else put' (k, v) t
    where buckets = length arr

-- Inserta el par en la tabla
put' (k, v) t@(HashTable pairs arr) = if containsKey k t
                                    then let removedKey = getTable (removeKey k t)
                                         in (HashTable pairs (removedKey // [(i, (k,v) : (removedKey ! i))])) 
                                    else (HashTable (pairs+1) (arr // [(i, (k,v) : (arr ! i))]))
  where i = hashQProbing t k 
        
getValue :: (Hashable k, Eq k) => k -> HashTable k v -> Maybe (k, v)
-- Obtiene el valor asociado a la clave que recibe en la tabla correspondiente.
-- Parámetros: Clave de la que queremos obtener el valor.
--             Tabla de la que lo obtenemos.
-- Devuelve:   Par clave-valor

-- Recorremos el conjunto de buckets desde la posición obtenida por la función de hash filtrando
-- aquellos que contengan la clave que buscamos (solo será uno de ellos).
-- Si el resultado de la búsqueda es nulo el elemento no se encuentra en la tabla.
getValue key t@(HashTable _ table) = if null result then Nothing else head result
  where position = hashQProbing t key
        result = filter (not . isNothing) 
          (map (List.find (\(k,v) -> k == key)) 
            ([bucket | i<-[position..(length table)-1], let bucket = table!i] ++ [bucket | i<-[0..position-1], let bucket = table!i]))

replace :: (Hashable a, Eq a, Eq b) => (a, b) -> HashTable a b -> HashTable a b
replace (k, v) t = put (k, v) t

removeKey :: (Hashable a, Eq a, Eq b) => a -> HashTable a b -> HashTable a b
-- Elimina el par asociado a la clave en la tabla.
-- Parámetros: Clave.
--             Tabla.
-- Devuevle:   Tabla con el par eliminado.

-- Se encarga de hacer un aumento/disminución de la tabla tras la eliminación del par
removeKey key t@(HashTable _ table) = if pairs > 0 && (fromIntegral pairs) <= (fromIntegral buckets / 8) then resize (div buckets 2) removed else removed
    where removed = removeKey' key t
          pairs = getNumPairs removed
          buckets = length (getTable removed)

-- Elimina el par de la tabla si este existe
removeKey' key t@(HashTable pairs table) = 
  case getValue key t of
    Nothing -> t
    Just (k,v) -> HashTable (pairs-1) (table // [(position', bucket')])
      where position = hashSChaining (List.length table) key
            buckets = [(i,bucket) | i<-[position..(length table)-1], let bucket = table!i] ++ [(i,bucket) | i<-[0..position-1], let bucket = table!i]
            indexedBucket = filter (\(i,xs) -> any (\(ke, val) -> ke==key) xs) buckets
            position' = fst (head indexedBucket)
            bucket = table ! position'
            bucket' = List.delete (k,v) bucket

putAll :: (Hashable a, Eq a, Eq b) => HashTable a b -> HashTable a b -> HashTable a b
-- Inserta todos los pares de la segunda tabla recibida en la primera
-- Parámetros: Tabla 1
--             Tabla 2
-- Devuelve:   Tabla 1 con los nuevos pares
putAll t1 t2 = foldr (\(k,v) ac -> put (k,v) ac) t1 entrySet
    where entrySet = entries t2

hashSChaining :: Hashable a => Int -> a -> Int
-- Calcula la función de hash a partir de n (longitud de la tabla)
hashSChaining n = (`mod` n) . hash

hashQProbing :: Hashable a => HashTable a b -> a -> Int
-- Calcula el índice del siguiente bucket vacío a partir del hash del elemento recibido y el tamaño de la tabla.
-- Parámetros: Tabla hash
--             Valor del que queremos el hash
-- Devuelve:   Índice con bucket vacío
hashQProbing t@(HashTable _ arr) v
    | null (arr ! i) = i
    | otherwise = fst (head (filter (\(_, b) -> null b) ([(j',arr!j') | j<-[(i+1*1),(i+2*2)..n-1], let j' = mod j n])))
    where n = length arr
          i = hashSChaining n v

resize :: (Hashable a, Eq a, Eq b) => Int -> HashTable a b -> HashTable a b
-- Cambia el tamaño de la tabla insertando todos los elementos en una vacía con la capacidad indicada
resize capacity t = putAll (empty capacity) t

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
    
getNumPairs :: HashTable a b -> Int
getNumPairs (HashTable pairs table) = pairs
clear :: Eq a => HashTable a b -> HashTable a b
clear t@(HashTable _ arr) = empty (length arr)

size :: HashTable a b -> Int
size t@(HashTable _ arr) = length arr

-- merge?

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


t1 :: HashTable String [Int]
t1 = HashTable 1 (array (0,9) [(0,[]),(1,[]),(2,[]),(3,[]),(4,[]),(5,[]),(6,[]),(7,[("Paco",[1,2])]),(8,[]),(9,[])])
-- test doubling size by inserting pair
t2 :: HashTable Int Int
t2 = HashTable 3 (array (0,5) [(0,[(1,0)]),(1,[(2,0)]),(2,[(3,0)]),(3,[]),(4,[]),(5,[])])
-- test halving sive by removing pair
t3 :: HashTable Int Int
t3 = HashTable 1 (array (0,9) [(0,[(1,0)]),(1,[]),(2,[]),(3,[]),(4,[]),(5,[]),(6,[]),(7,[]),(8,[]),(9,[])])