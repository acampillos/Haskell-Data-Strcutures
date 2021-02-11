-- COLA DOBLEMENTE TERMINADA/DOUBLE-ENDED QUEUE:
-- ADT que generaliza una cola, permitiendo la inserción y eliminación de elementos 
-- del principio y final de la cola.

-----------------------
-- CONSTRUCTORES
-----------------------

-- Lo implementamos mediante dos listas junto al tamaño de las mismas.
--
--      Deque = |Front| Front |Rear| Rear
--
data Deque a = Deque Int [a] Int [a]
    deriving (Show)

-- El balance perfecto se da cuando el conjunto de elementos está dividido en 
-- partes iguales para las dos listas que componen la deque.
-- Para mejorar la eficiencia, restauramos este balance mediante un invariante que
-- nos permite no tener que restaurarlo con cada operación:
--
--          |F| <= c|R|+1 y |R| <= c|F|+1
--
-- Ninguno de los dos conjuntos será c veces más largo que el otro.
-- El valor de c puede ser modificado a continuación:
c :: Int
c = 4

-----------------------
-- FUNCIONES
-----------------------

empty :: Deque a
empty = Deque 0 [] 0 []

isEmpty :: Deque a -> Bool
isEmpty (Deque sf f sr r) = sf + sr == 0

queue :: Deque a -> Deque a
-- Restaura el balance en la deque.
-- Tomamos los i (mitad del total de elementos) primeros elementos de la lista mayor para remplazar a esta.
-- Hacemos que la nueva lista menor se contenga a sí misma y al resto de la anterior.
-- Tomamos el resto de elementos de la mayor en orden inverso para mantener el orden de la cola.

-- Parámetros: Deque.
-- Devuelve: Deque con balance restaurado.
queue q@(Deque sf f sr r)
    | sf > c*sr + 1 = 
        let f' = take i f
            r' = r ++ reverse (drop i f)
        in Deque i f' j r'
    | sr > c*sf + 1 =
        let f' = f ++ reverse (drop i r)
            r' = take i r
        in Deque i f' j r'
    | otherwise = q
    where i = div (sf+sr) 2                 -- Mitad del total de elementos
          j = sf + sr - i                   -- Índice de comienzo de la segunda lista


-- FUNCIONES ASOCIADAS AL FRENTE (FRONT)

cons :: Deque a -> a -> Deque a
-- Inserta en el frente un elemento.
-- Parámetros: Deque.
--             Elemento a insertar en el frente.
-- Devuelve:   Deque con el nuevo elemento en el frente.
cons (Deque sf f sr r) x = queue (Deque (sf+1) (x:f) sr r)

head :: Deque a -> a
-- Inspecciona el elemento del frente.
-- Parámetros: Deque.
-- Devuelve:   Elemento del frente.
head (Deque 0 [] 0 []) = error "Empty deque"
-- no se si r es solo un elemento [x] o (x:r), igual en el resto de casos
head (Deque 0 [] sr r) = Prelude.head r
head (Deque sf (x:f) _ _) = x

tail :: Deque a -> Deque a
-- Elimina el elemento en el frente.
-- Parámetros: Deque.
-- Devuelve:   Deque.
tail (Deque 0 [] 0 []) = error "Empty deque"
tail (Deque 0 [] sr [x]) = empty
tail (Deque sf f sr r) = queue (Deque (sf-1) (Prelude.tail f) sr r)


-- FUNCIONES ASOCIADAS AL FINAL (REAR)

snoc :: Deque a -> a -> Deque a
-- Inserta en el final un elemento.
-- Parámetros: Deque.
--             Elemento a insertar en el final.
-- Devuelve:   Deque con el nuevo elemento en el final.
snoc (Deque sf f sr r) x = queue (Deque sf f (sr+1) (x:r))

last :: Deque a -> a
-- Inspecciona el elemento del final.
-- Parámetros: Deque.
-- Devuelve:   Elemento del final.
last (Deque 0 [] 0 []) = error "Empty deque"
last (Deque sf [x] 0 []) = x
last (Deque _ _ sr (x:r)) = x

init :: Deque a -> Deque a
-- Elimina el elemento en el final.
-- Parámetros: Deque.
-- Devuelve:   Deque.
init (Deque 0 [] 0 []) = error "Empty queue"
init (Deque sf [x] 0 []) = empty
init (Deque sf f sr r) = queue (Deque sf f (sr-1) (Prelude.tail r))

printDeque :: (Show a) => Deque a -> String
printDeque (Deque sf f sr r) = show (f ++ (reverse r))

q1, q2, q3 :: Deque Int
q1 = Deque 3 [1,2,3] 2 [4,5]
q2 = Deque 7 [1,2,3,4,5,6,7] 1 [8]
q3 = undefined