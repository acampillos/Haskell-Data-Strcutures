module DataStructures.MinHeap(
    MinHeap(..),
    empty,
    isEmpty,
    DataStructures.MinHeap.insert,
    merge,
    findMin,
    deleteMin,
    elements,
    equals,
    rank,
    helpMinHeap
) where

import Data.List

-- MONTÍCULO BINARIO MÍNIMO / MIN BINARY HEAP:
-- Son árboles binarios completos que cumplen la propiedad de que para cada padre en el árbol
-- sus hijos tienen un valor mayor que el de este.

-----------------------
-- CONSTRUCTORES
-----------------------

-- Lo implementamos mediante un árbol binario que en cada nodo también contendrá el rango del mismo.
-- Entendemos por rango la menor distancia desde el nodo a una hoja del árbol binario.
-- Lo almacenamos con el fin de ahorrar complejidad temporal a coste de complejidad espacial.

data MinHeap a = Empty | Tree Int a (MinHeap a) (MinHeap a)
    deriving (Show, Eq)

-----------------------
-- FUNCIONES
-----------------------

helpMinHeap :: IO ()
helpMinHeap = putStrLn "\nempty :: Ord a => MinHeap a\n\nisEmpty :: MinHeap a -> Bool\n\ninsert :: (Ord a) => MinHeap a -> a -> MinHeap a\n\nmerge :: (Ord a) => MinHeap a -> MinHeap a -> MinHeap a\n\nfindMax :: MinHeap a -> a\n\ndeleteMax :: (Ord a) => MinHeap a -> MinHeap a\n\nelements :: MinHeap a -> [a]\n\nequals :: Ord a => MinHeap a -> MinHeap a -> Bool\n\nrank :: MinHeap a -> Int"


empty :: Ord a => MinHeap a
empty = Empty

isEmpty :: MinHeap a -> Bool
isEmpty Empty = True
isEmpty (Tree _ _ _ _) = False

insert :: (Ord a) => MinHeap a -> a -> MinHeap a
-- Inserta un elemento en el montículo mediante la mezcla del mismo con uno que solo tiene el elemento.
-- Parámetros: Montículo
--             Elemento
-- Devuelve:   Montículo con el elemento
insert Empty x = Tree 1 x Empty Empty
insert t x = merge t (Tree 1 x Empty Empty)

merge :: (Ord a) => MinHeap a -> MinHeap a -> MinHeap a
-- Mezcla dos montículos.
-- Parámetros: Montículo 1
--             Montículo 2
-- Devuelve:   Montículo mezclando ambos
merge a Empty = a
merge Empty a = a
-- Crea un montículo cuya raíz será el menor elemento entre las raíces de los dos montículos que recibimos
merge h1@(Tree r1 x a1 b1) h2@(Tree r2 y a2 b2)
    -- Como hijo izquierdo tendremos el hijo izquierdo del montículo con menor raíz
    | x<=y = merge' x a1 (merge b1 h2)
    -- Como hijo derecho tendremos la mezcla del hijo derecho del montículo con menor raíz y el montículo completo con mayor raíz
    | otherwise = merge' y a2 (merge h1 b2)


merge' :: a -> MinHeap a -> MinHeap a -> MinHeap a
-- Coloca un elemento como raíz ajustando su rango en función de sus hijos que serán los dos montículos recibidos.
-- Parámetros: Elemento
--             Monticulo 1
--             Monticulo 2
-- Devuelve:   Monticulo con rango ajustado y con el elemento como raíz
merge' v Empty b = Tree 1 v b Empty
merge' v a Empty = Tree 1 v a Empty
merge' v h1 h2
    -- El rango del nuevo montículo que será el menor + 1 por el nuevo nodo añadido
    -- El montículo será leftist (acumulamos los subárboles de mayor tamaño en la parte izquierda del nuevo montículo) 
    | r1 >= r2 = Tree (r2 + 1) v h1 h2
    | otherwise = Tree (r1 + 1) v h2 h1 
    where r1 = rank h1
          r2 = rank h2

findMin :: MinHeap a -> a
findMin Empty = error "Empty heap"
findMin (Tree _ x _ _) = x

deleteMin :: (Ord a) => MinHeap a -> MinHeap a
deleteMin Empty = error "Empty heap"
deleteMin (Tree r x a b) = merge a b

elements :: MinHeap a -> [a]
elements Empty = []
elements (Tree _ x a b) = [x] ++ elements a ++ elements b

equals :: Ord a => MinHeap a -> MinHeap a -> Bool
equals Empty Empty = True
equals a Empty = False
equals Empty b = False
equals (Tree r1 x a1 b1) (Tree r2 y a2 b2) =
    r1 == r2 && x == y && equals a1 a2 && equals b1 b2

rank :: MinHeap a -> Int
rank Empty = 0
rank (Tree r _ _ _) = r

-- Ejemplos

t1, t2 :: MinHeap Int
t1 = Tree 2 10 (Tree 1 5 (Tree 1 2 Empty Empty) Empty) (Tree 1 6 Empty Empty)
t2 = Tree 2 12 (Tree 1 7 Empty Empty) (Tree 1 9 Empty Empty)