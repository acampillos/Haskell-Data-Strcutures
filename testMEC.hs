import Data.List as L
import DataStructures.BinarySearchTree as BST
import Data.Set as Set
import DataStructures.Graph as G
import DataStructures.Deque as D
--import DataStructures.VEB as VEB
--import Data.Map as Map
--import DataStructures.RedBlackTree as RBT
-- Zona de tests para marespcue1

{-
ejbrt1,ejbrt2,ejbrt3,ejbrt4,ejbrt5,ejbrt6,ejbrt7 :: RBTree Int
ejbrt1 = (N B 7 (N B 3 (L) (L)) (N R 18 (N B 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (L) (L)))))
ejbrt6 = L.foldr insertRBT (L) [6,3,18,1,4,14,22,9,16,20,26,8,10,15,17]
ejbrt7 = L.foldr insertRBT (L) [7,3,18,10,8,11,22,26]
ejbrt8 = L.foldr insertRBT (L) [1,2,4,5,3]
ejbrt9 = L.foldr insertRBT (L) [8,9,10,13,11]
ejbrt10 = (N B 6 ejbrt8 ejbrt9)
-- Ejemplos de RBT no valido
-- No tiene todos los caminos hasta las hojas con misma cantidad de negros
ejbrt2 = (N B 7 (N B 3 (L) (L)) (N R 18 (N B 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (N B 25 (L) (L)) (L)))))
-- La raiz es roja
ejbrt3 = (N R 7 (N B 3 (L) (L)) (N R 18 (N B 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (N B 25 (L) (L)) (L)))))
-- Un rojo tiene un hijo rojo
ejbrt4 = (N B 7 (N B 3 (L) (L)) (N R 18 (N R 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (N B 25 (L) (L)) (L)))))
-- No es BST
ejbrt5 = (N B 7 (N B 42 (L) (L)) (N R 18 (N R 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (N B 25 (L) (L)) (L)))))

--fuse (N R 2 (N B 1 (L) (L)) (N B 3 (L) (L))) (N B 5 (L) (L))
ejbrt11 = (N B 4 (N B 1 L (N R 3 L L)) (N B 8 (N R 6 L L) L))
ejbrt12 = (N B 15 (N B 10 L (N R 14 L L)) (N R 20 (N B 17 (N R 16 L L) (N R 18 L L)) (N B 26 (N R 22 L L) L)))
-}

-- GRAFOS---------------------------------------------------------------------

-- Grafo es una lista de vertices y una lista de indices o un grafo vacio
{--data D
Graph a = G [Vertex a] [Int] | EG 
    deriving (Show, Eq)-}
{-
-- Grafo es un map de claves vertice y valores lista de aristas
type D
Graph a = Map (Vertex a) [Path]

-- Un nodo es un Int indice de vertice (no debe repetirse en un grafo), a que es la info del vertice y una lista de 
-- aristas
data Vertex a = V Int a
    deriving (Show, Eq, Ord)
-- Un camino es una arista que indica el indice del vertice al que parte hasta el que llega
data Path = P Int Int
    deriving (Show, Eq,Ord)


emptyGraph :: D
Graph a
emptyGraph = empty

newGraph :: (Eq a) => a -> D
Graph a
newGraph a = singleton (V 1 a) []

addVertex ::  D
Graph a -> Vertex a -> D
Graph a
addVertex empty v = singleton v []
addVertex g v = Map.insert v [] g-}


{-addVertex :: Vertex a -> D
Graph a -> D
Graph a
addVertex v (G xs) = --}



--------------------------------------------------------------------
-- Arbol busqueda binaria---------------------------------------------------------

-- BST
test1, test2 :: BSTree Int
test1 = L.foldr BST.insert H [50,40,60,70,80]
test2 = undefined
ejbt1,ejbt2,ejbt3,ejbt4,ejbt5 :: BSTree Int
ejbt1 = (N (N (H) (H) 5) (N (N (N (H) (H) 9) (N (H) (H) 13) 12) (N (H) (N (H) (H) 23) 19) 15) 8)
-- Prueba para ver que no es SBT
ejbt2 = (N (N (H) (H) 5) (N (N (N (H) (H) 9) (N (H) (H) 13) 12) (N (H) (N (H) (H) 23) 19) 15) 2000) 
ejbt4 = (N (H) (H) 4)
ejbt3 = (H)
ejbt5 = (N (N (N (H) (H) 1) (N (N (N (H) (H) 122) (N (H) (H) 126) 125) (N (N (N (H) (H) 170) (N (H) (H) 174) 173) (N (H) (N (H) (H) 180) 176) 175) 150) 100) (N (H) (H) 400) 200)

{-
test1, test2 :: BSTree Int
test1 = (N (N (H) (H) 40) (N (N (H) (H) 60) (N (H) (H) 80) 70) 50)
test2 = (N (N (H) (H) 40) (N (N (H) (H) 60) (N (H) (H) 80) 70) 50)
-}

-- Grafos con conjuntos ----------------------------------------------------------------------------
gTest1,gTest2,gTest3 :: Graph Int
gTest1 = (Set.fromList [(V 1),(V 2),(V 3),(V 4)], Set.fromList
     [(P 1 (V 1) (V 2)),(P 3 (V 4) (V 2)),(P 2 (V 1) (V 3)),(P 1 (V 3) (V 4))])
{-
            1
    1  ---------->  2
    |              / \
   2|               |
    |              3|
   \ /      1       |
    3  ---------->  4
-}
gTest2 = (Set.fromList [(V 1),(V 2),(V 3),(V 4),(V 5),(V 6),(V 7)], 
    Set.fromList [(P 1 (V 5) (V 6)),(P 2 (V 2) (V 7)), (P 4 (V 2) (V 5)),(P 3 (V 1) (V 2)),
        (P 0 (V 4) (V 2)),(P 1 (V 1) (V 3)),(P 1 (V 3) (V 4))])
{-
                    7
                   / \
                    |
                   2|
           3        |       4
    1  ---------->  2  -----------> 5
    |              / \              |
   1|               |              1|
    |              0|               |
   \ /      1       |              \ /
    3  ---------->  4               6
-}


gTest3 = (Set.fromList [(V 1),(V 2),(V 3),(V 4),(V 5),(V 6),(V 7),(V 8),(V 9),(V 10),(V 11)], Set.fromList [(P 1 (V 5) (V 6)),
    (P 2 (V 2) (V 7)),
        (P 4 (V 2) (V 5)),
            (P 3 (V 1) (V 2)),
                (P 0 (V 4) (V 2)),
                    (P 1 (V 1) (V 3)),
                        (P 1 (V 3) (V 4)),
                            (P 1.5 (V 1) (V 7)),
                                (P 1 (V 5) (V 8)),
                                    (P 0 (V 8) (V 9)),
                                        (P 1 (V 11) (V 9)),
                                            (P 2 (V 10) (V 11)),
                                                (P 0 (V 10) (V 4))])


{-
          __ -----> 7
     1.5/          / \
      /             |
    /              2|
    |       3       |       4               1
    1  ---------->  2  -----------> 5 ------------> 8
    |              / \              |               |
   1|               |              1|               |0
    |              0|               |               |
   \ /      1       |              \ /             \ /
    3  ---------->  4               6               9
                   / \                             / \
                    |                               |
                    |0.5                            |1
                    |               2               |
                    10 ---------------------------> 11
-}


------------------- van Emde Boas ----------------------------------------------------------------
