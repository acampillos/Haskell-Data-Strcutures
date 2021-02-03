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
data MaxHeap a = Empty | Tree Int a (MaxHeap a) (MaxHeap a)
    deriving (Show, Eq)

empty :: Ord a => MaxHeap a
empty = Empty

isEmpty :: MaxHeap a -> Bool
isEmpty Empty = True
isEmpty (Tree _ _ _ _) = False

insert :: (Ord a) => MaxHeap a -> a -> MaxHeap a
insert Empty x = Tree 1 x Empty Empty
-- Suponemos un arbol con el valor a insertar y lo combinamos con el existente
insert t x = merge t (Tree 1 x Empty Empty)

merge :: (Ord a) => MaxHeap a -> MaxHeap a -> MaxHeap a
merge a Empty = a
merge Empty a = a
merge h1@(Tree r1 x a1 b1) h2@(Tree r2 y a2 b2)
    | x>=y = makeT x (merge h2 a1) b1
    | otherwise = makeT y (merge h1 a2) b2

-- Hace que el monticulo sea leftist
-- En el caso de que la parte derecha sea mas larga (rango derecho mas grande), intercambia ambos arboles y aumenta el antiguo rango izquierdo debido a x
-- En el caso de que la parte izquierda sea mas larga (rango izq mas grande), intercambia ambos arboles y aumenta el antigo rango derecho 
makeT :: a -> MaxHeap a -> MaxHeap a -> MaxHeap a
makeT v Empty b = Tree 0 v b Empty
makeT v a Empty = Tree 0 v a Empty
makeT v h1 h2
    | r1 >= r2 = Tree (r2 + 1) v h1 h2
    | otherwise = Tree (r1 + 1) v h2 h1 
    where r1 = rank h1
          r2 = rank h2

findMax :: MaxHeap a -> a
findMax Empty = error "Empty heap"
findMax (Tree _ x _ _) = x

deleteMax :: (Ord a) => MaxHeap a -> MaxHeap a
deleteMax Empty = error "Empty heap"
deleteMax (Tree r x a b) = merge a b

elements :: MaxHeap a -> [a]
elements Empty = []
elements (Tree _ x a b) = [x] ++ elements a ++ elements b

equals :: Ord a => MaxHeap a -> MaxHeap a -> Bool
equals Empty Empty = True
equals a Empty = False
equals Empty b = False
equals (Tree r1 x a1 b1) (Tree r2 y a2 b2) =
    r1 == r2 && x == y && equals a1 a2 && equals b1 b2

rank :: MaxHeap a -> Int
rank Empty = 0
rank (Tree r _ _ _) = r

t1, t2 :: MaxHeap Int
t1 = Tree 2 10 (Tree 1 5 (Tree 1 2 Empty Empty) Empty) (Tree 1 6 Empty Empty)
t2 = Tree 2 12 (Tree 1 7 Empty Empty) (Tree 1 9 Empty Empty)