module DataStructures.DirectedGraph(
    DGraph(..),
    Vertex(..),
    Path(..),
    Tree(..),
    Forest(..),
    newGraph,
    addVertex,
    vertexSet,
    pathSet,
    containsVertex,
    containsPath,
    vertexNum,
    pathsNum,
    addPath,
    srcVertex,
    dstVertex,
    pathsFrom,
    pathsTo,
    adjacents,
    dfs,
    bfs,
    orderedPaths
)
where

import Data.Set as Set
import Data.List as L
-- Grafos con conjuntos

type DGraph a = (Set (Vertex a), Set (Path a))

data Vertex a = V a
    deriving (Show, Eq, Ord)

--                w        src       dst
data Path a = P Float (Vertex a) (Vertex a)
    deriving (Show, Eq,Ord)


newGraph :: (Eq a, Ord a) => DGraph a
newGraph = (empty , empty)

addVertex :: (Eq a, Ord a) => DGraph a -> Vertex a -> DGraph a
addVertex (setV, setP) v = (Set.insert v setV, setP)

vertexSet :: (Eq a, Ord a) => DGraph a -> Set (Vertex a)
vertexSet g = (fst g)

pathSet :: (Eq a, Ord a) => DGraph a -> Set (Path a)
pathSet g = (snd g)

containsVertex :: (Eq a, Ord a) => DGraph a -> Vertex a -> Bool
containsVertex g v = Set.size (fst g) == Set.size (Set.insert v (fst g))

containsPath :: (Eq a, Ord a) => DGraph a -> Path a -> Bool
containsPath g p = Set.size (snd g) == Set.size (Set.insert p (snd g))

vertexNum :: (Eq a, Ord a) => DGraph a -> Int
vertexNum g = length $ vertexSet g

pathsNum ::  (Eq a, Ord a) => DGraph a -> Int
pathsNum g = length $ pathSet g

addPath :: (Eq a, Ord a) => DGraph a -> Path a -> DGraph a
addPath g@(bstV,bstP) p@(P _ v1 v2)
    | not (containsVertex g v1) && not (containsVertex g v2) = 
        error " --> No existe ninguno de los vertices de la arista <-- "
    | not (containsVertex g v1) = error " --> No existe el primer vertice de la arista <-- "
    | not (containsVertex g v2) = error " --> No existe el segundo vertice de la arista <-- "
    | otherwise = (bstV, Set.insert p bstP)

srcVertex :: (Eq a, Ord a) => Path a -> Vertex a
srcVertex (P _ v _) = v

dstVertex :: (Eq a, Ord a) => Path a -> Vertex a
dstVertex (P _ _ v) = v

-- Lista de aristas que salen del vertice v
pathsFrom :: (Eq a, Ord a) => DGraph a -> Vertex a -> Set (Path a)
pathsFrom g v = Set.filter (\x -> srcVertex x == v) (snd g)

-- Lista de aristas que llegan al vertice v
pathsTo :: (Eq a, Ord a) => DGraph a -> Vertex a -> Set (Path a)
pathsTo g v = Set.filter (\x -> dstVertex x == v) (snd g)

-- Lista de vertices adyacentes a uno dado
{-
adjacents :: (Eq a, Ord a) => DGraph a -> Vertex a -> Set (Vertex a)
adjacents g v = Set.map dstVertex $ (pathsTo g v ) `Set.union` (pathsFrom g v )
-}

adjacents :: (Eq a, Ord a) => DGraph a -> Vertex a -> Set (Vertex a)
adjacents g v = Set.map dstVertex $ pathsFrom g v

-- Recorrido en profundidad
dfs :: (Eq a, Ord a) => DGraph a -> Vertex a -> [Vertex a]
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
dfsAux :: (Eq a, Ord a) => DGraph a -> [Vertex a] -> [Vertex a] -> [Vertex a]
dfsAux g [] ys = ys
dfsAux g (x:xs) ys = dfsAux g (newPaths++xs) (ys++[x]) 
    where
        adjToX = (adjacents g x)
        newPaths = Set.toList $ Set.difference adjToX (Set.fromList (ys++[x]))

-- Siendo x el vertice por el que se va, en cada llamada recursiva, se añade a la pila los adyacentes a x
-- exceptuando los que ya han sido recorridos (incluyendo x) y se añade x a los recorridos.


bfs :: (Eq a, Ord a) => DGraph a -> Vertex a -> [Vertex a]
bfs g v = bfsAux g (Set.toList (adjacents g v)) [v]

bfsAux :: (Eq a, Ord a) => DGraph a -> [Vertex a] -> [Vertex a] -> [Vertex a]
bfsAux g [] ys = ys
bfsAux g (x:xs) ys = bfsAux g (xs++newPaths) (ys++[x]) 
    where
        adjToX = (adjacents g x)
        newPaths = Set.toList $ Set.difference adjToX (Set.fromList (ys++[x]))

orderedPaths :: (Eq a, Ord a) => DGraph a -> [Path a]
orderedPaths g = sort (Set.toList (pathSet g))

data Tree a = Node a [Tree a]
            | Leaf
            deriving (Eq, Show)

type Forest a = [Tree a]

-- Considerando el grafo dirigido
{-
kruskal :: (Eq a, Ord a) => DGraph a -> Forest a
--                                        
kruskal g = kruskal' 
    g                   -- Grafo de entrada
    (orderedPaths g)    -- aristas ordenadas por peso
    []                  -- bosque solucion
    0                   -- indice del bosque (Indica por que arbol va)
    (vertexSet g)       -- vertices por poner

-- kruskal' :: (Eq a, Ord a) => DGraph a -> (Set (Path a)) -> Forest a -> Int -> (Set (Vertex a)) -> Forest a
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
-}