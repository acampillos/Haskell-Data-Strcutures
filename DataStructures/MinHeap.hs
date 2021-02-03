import Data.List
-- LEFTIST HEAP
-- empty
-- isEmpty
-- insert
-- merge
-- findMin
-- deleteMin

-- elements
-- equals

-- TODO
-- 

--                           rango val izq      der
data MinHeap a = Empty | Tree Int a (MinHeap a) (MinHeap a)
    deriving (Show, Eq)

empty :: Ord a => MinHeap a
empty = Empty

isEmpty :: MinHeap a -> Bool
isEmpty Empty = True
isEmpty (Tree _ _ _ _) = False

insert :: (Ord a) => MinHeap a -> a -> MinHeap a
insert Empty x = Tree 1 x Empty Empty
-- Suponemos un arbol con el valor a insertar y lo combinamos con el existente
insert t x = merge t (Tree 1 x Empty Empty)

merge :: (Ord a) => MinHeap a -> MinHeap a -> MinHeap a
merge a Empty = a
merge Empty a = a
merge h1@(Tree r1 x a1 b1) h2@(Tree r2 y a2 b2)
    -- Mantenemos x con su lado izquierdo y combinamos el derecho con h2 (ya que puede que los valores de h2 sean >= que los de b1)
    | x<=y = makeT x a1 (merge b1 h2)
    -- En caso contrario, acumulamos a la izquierda la parte menor de h2 con y y realizamos la misma combinacion pero con h1 y la parte derecha de h2
    | otherwise = makeT y a2 (merge h1 b2)

-- Hace que el monticulo sea leftist
-- En el caso de que la parte derecha sea mas larga (rango derecho mas grande), intercambia ambos arboles y aumenta el antiguo rango izquierdo debido a x
-- En el caso de que la parte izquierda sea mas larga (rango izq mas grande), intercambia ambos arboles y aumenta el antigo rango derecho 
makeT :: a -> MinHeap a -> MinHeap a -> MinHeap a
makeT v Empty b = Tree 0 v b Empty
makeT v a Empty = Tree 0 v a Empty
makeT v h1 h2
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