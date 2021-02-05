import Data.List as L
import DataStructures.BinarySearchTree as BST
import Data.Set as Set
--import Data.Map as Map
--import DataStructures.RedBlackTree as RBT
-- Zona de tests para marespecue1

{-
ejbrt1,ejbrt2,ejbrt3,ejbrt4,ejbrt5,ejbrt6,ejbrt7 :: RBTree Int
ejbrt1 = (N B 7 (N B 3 (L) (L)) (N R 18 (N B 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (L) (L)))))
ejbrt6 = foldr insertRBT (L) [6,3,18,1,4,14,22,9,16,20,26,8,10,15,17]
ejbrt7 = foldr insertRBT (L) [7,3,18,10,8,11,22,26]
ejbrt8 = foldr insertRBT (L) [1,2,4,5,3]
ejbrt9 = foldr insertRBT (L) [8,9,10,13,11]
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
{--data Graph a = G [Vertex a] [Int] | EG 
    deriving (Show, Eq)-}
{-
-- Grafo es un map de claves vertice y valores lista de aristas
type Graph a = Map (Vertex a) [Path]

-- Un nodo es un Int indice de vertice (no debe repetirse en un grafo), a que es la info del vertice y una lista de 
-- aristas
data Vertex a = V Int a
    deriving (Show, Eq, Ord)
-- Un camino es una arista que indica el indice del vertice al que parte hasta el que llega
data Path = P Int Int
    deriving (Show, Eq,Ord)


emptyGraph :: Graph a
emptyGraph = empty

newGraph :: (Eq a) => a -> Graph a
newGraph a = singleton (V 1 a) []

addVertex ::  Graph a -> Vertex a -> Graph a
addVertex empty v = singleton v []
addVertex g v = Map.insert v [] g-}


{-addVertex :: Vertex a -> Graph a -> Graph a
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

{-test1, test2 :: BSTree Int
test1 = (N (N (H) (H) 40) (N (N (H) (H) 60) (N (H) (H) 80) 70) 50)
test2 = (N (N (H) (H) 40) (N (N (H) (H) 60) (N (H) (H) 80) 70) 50)-}

-- Grafos con conjuntos

type Graph a = (Set (Vertex a), Set (Path a))

data Vertex a = V a
    deriving (Show, Eq, Ord)

--                w        src       dst
data Path a = P Float (Vertex a) (Vertex a)
    deriving (Show, Eq,Ord)

gTest1,gTest2 :: Graph Int
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

newGraph :: (Eq a, Ord a) => Graph a
newGraph = (empty , empty)

addVertex :: (Eq a, Ord a) => Graph a -> Vertex a -> Graph a
addVertex (setV, setP) v = (Set.insert v setV, setP)

vertexSet :: (Eq a, Ord a) => Graph a -> Set (Vertex a)
vertexSet g = (fst g)

pathSet :: (Eq a, Ord a) => Graph a -> Set (Path a)
pathSet g = (snd g)

containsVertex :: (Eq a, Ord a) => Graph a -> Vertex a -> Bool
containsVertex g v = Set.size (fst g) == Set.size (Set.insert v (fst g))

containsPath :: (Eq a, Ord a) => Graph a -> Path a -> Bool
containsPath g p = Set.size (snd g) == Set.size (Set.insert p (snd g))

addPath :: (Eq a, Ord a) => Graph a -> Path a -> Graph a
addPath g@(bstV,bstP) p@(P _ v1 v2)
    | not (containsVertex g v1) && not (containsVertex g v2) = error " --> No existe ninguno de los vertices de la arista <-- "
    | not (containsVertex g v1) = error " --> No existe el primer vertice de la arista <-- "
    | not (containsVertex g v2) = error " --> No existe el segundo vertice de la arista <-- "
    | otherwise = (bstV, Set.insert p bstP)

srcVertex :: (Eq a, Ord a) => Path a -> Vertex a
srcVertex (P _ v _) = v

dstVertex :: (Eq a, Ord a) => Path a -> Vertex a
dstVertex (P _ _ v) = v

-- Lista de aristas que salen del vertice v
pathsFrom :: (Eq a, Ord a) => Graph a -> Vertex a -> Set (Path a)
pathsFrom g v = Set.filter (\x -> srcVertex x == v) (snd g)

-- Lista de aristas que llegan al vertice v
pathsTo :: (Eq a, Ord a) => Graph a -> Vertex a -> Set (Path a)
pathsTo g v = Set.filter (\x -> dstVertex x == v) (snd g)

-- Lista de vertices adyacentes a uno dado
{-
adjacents :: (Eq a, Ord a) => Graph a -> Vertex a -> Set (Vertex a)
adjacents g v = Set.map dstVertex $ (pathsTo g v ) `Set.union` (pathsFrom g v )
-}

adjacents :: (Eq a, Ord a) => Graph a -> Vertex a -> Set (Vertex a)
adjacents g v = Set.map dstVertex $ pathsFrom g v

-- Recorrido en profundidad
dfs :: (Eq a, Ord a) => Graph a -> Vertex a -> [Vertex a]
dfs g v = dfsAux g (Set.toList (adjacents g v)) [v]

{-
[1]         [2,3]       [1]
[2]       [7,5,3]       [1,2]
[7]         [5,3]       [1,2,7]
[5]         [6,3]       [1,2,7,5]
[6]           [3]       [1,2,7,5,6]
[3]           [4]       [1,2,7,5,6,3]
[4]            []       [1,2,7,5,6,3,4]
-}

--                                   "PILA" Vertices
--                          Grafo     a recorrer     recorridos      Sol
dfsAux :: (Eq a, Ord a) => Graph a -> [Vertex a] -> [Vertex a] -> [Vertex a]
dfsAux g [] ys = ys
dfsAux g (x:xs) ys = dfsAux g (newPaths++xs) (ys++[x]) 
    where
        adjToX = (adjacents g x)
        newPaths = Set.toList $ Set.difference adjToX (Set.fromList (ys++[x]))

-- Siendo x el vertice por el que se va, en cada llamada recursiva, se añade a la pila los adyacentes a x
-- exceptuando los que ya han sido recorridos (incluyendo x) y se añade x a los recorridos.


bfs :: (Eq a, Ord a) => Graph a -> Vertex a -> [Vertex a]
bfs g v = bfsAux g (Set.toList (adjacents g v)) [v]

bfsAux :: (Eq a, Ord a) => Graph a -> [Vertex a] -> [Vertex a] -> [Vertex a]
bfsAux g [] ys = ys
bfsAux g (x:xs) ys = bfsAux g (xs++newPaths) (ys++[x]) 
    where
        adjToX = (adjacents g x)
        newPaths = Set.toList $ Set.difference adjToX (Set.fromList (ys++[x]))

orderedPaths :: (Eq a, Ord a) => Graph a -> [Path a]
orderedPaths g = sort (Set.toList (pathSet g))

--kruskal :: (Eq a, Ord a) => Grafo a

{-
kruskal :: (Eq a, Ord a) => Grafo a -> [(p,v,v)]
kruskal g = kruskal’ cola           -- Cola de prioridad
        (tabla [(x,x) | x <- nodos g])  -- Tabla de raices
        []                              -- Árbol de expansión
        ((length (nodos g)) - 1)        -- Aristas por
                                    -- colocar
    where cola = sort [(p,x,y) | (x,y,p) <- aristas g]

kruskal’ ((p,x,y):as) t ae n
    | n==0 = ae
    | actualizado = kruskal’ as t’ ((p,x,y):ae) (n-1)
    | otherwise = kruskal’ as t ae n
    where (actualizado,t’) = buscaActualiza (x,y) t
-}