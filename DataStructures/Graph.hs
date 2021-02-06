module DataStructures.Graph(
    Graph(..),
    Vertex(..),
    Path(..),
    Tree(..),
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
    pathsOf,
    adjacents,
    dfs,
    bfs,
    orderedPaths,
    conexo,
    conectividad,
    kruskal
)
where

import Data.Set as Set
import Data.List as L
-- Grafos con conjuntos

type Graph a = (Set (Vertex a), Set (Path a))

data Vertex a = V a
    deriving (Show, Eq, Ord)

--                w        src       dst
data Path a = P Float (Vertex a) (Vertex a)
    deriving (Show, Eq,Ord)


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

vertexNum :: (Eq a, Ord a) => Graph a -> Int
vertexNum g = length $ vertexSet g

pathsNum ::  (Eq a, Ord a) => Graph a -> Int
pathsNum g = length $ pathSet g

addPath :: (Eq a, Ord a) => Graph a -> Path a -> Graph a
addPath g@(bstV,bstP) p@(P _ v1 v2)
    | not (containsVertex g v1) && not (containsVertex g v2) = 
        error " --> No existe ninguno de los vertices de la arista <-- "
    | not (containsVertex g v1) = error " --> No existe el primer vertice de la arista <-- "
    | not (containsVertex g v2) = error " --> No existe el segundo vertice de la arista <-- "
    | containsPath g p = g
    | otherwise = (bstV, Set.insert p bstP)


srcVertex :: (Eq a, Ord a) => Path a -> Vertex a
srcVertex (P _ v _) = v

dstVertex :: (Eq a, Ord a) => Path a -> Vertex a
dstVertex (P _ _ v) = v

-- Lista de aristas que salen de v o llegan a v
pathsOf :: (Eq a, Ord a) => Graph a -> Vertex a -> Set (Path a)
pathsOf g v = Set.filter (\x -> srcVertex x == v || dstVertex x == v) (snd g)

-- Lista de aristas que salen del vertice v
pathsFrom :: (Eq a, Ord a) => Graph a -> Vertex a -> Set (Path a)
pathsFrom g v = Set.filter (\x -> srcVertex x == v) (snd g)

-- Lista de aristas que llegan al vertice v
pathsTo :: (Eq a, Ord a) => Graph a -> Vertex a -> Set (Path a)
pathsTo g v = Set.filter (\x -> dstVertex x == v) (snd g)

-- Lista de vertices adyacentes a uno dado
adjacents :: (Eq a, Ord a) => Graph a -> Vertex a -> Set (Vertex a)
adjacents g v = Set.map dstVertex (pathsFrom g v) `Set.union` Set.map srcVertex (pathsTo g v)


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

data Tree a = Node a [Tree a]
            | Leaf
            deriving (Eq, Show)


-- Ver si es un grafo conexo
conexo :: (Eq a, Ord a) => Graph a -> Bool
--conexo (empty,_) = False
conexo g = vertexNum g == length (bfs g (Set.elemAt 0 (vertexSet g)))

conectividad :: (Eq a, Ord a) => Graph a -> Int
conectividad g = conectividad' g (vertexSet g) 0 empty

conectividad' :: (Eq a, Ord a) => Graph a -> Set (Vertex a) -> Int -> Set (Vertex a) -> Int
conectividad' g porVisitar num visitados
    | length visitados == length (vertexSet g) = num
    | otherwise = conectividad' g (Set.difference (vertexSet g) nuevoVisitados) (num+1) nuevoVisitados
        where 
            x = Set.elemAt 0 porVisitar
            nuevoVisitados = (Set.union (Set.fromList (dfs g x)) visitados)


-- Considerando el grafo no dirigido
kruskal :: (Eq a, Ord a) => (Eq a, Ord a) => Graph a -> Graph a                                
kruskal g@(vertex,_) = kruskal' (orderedPaths g) (vertex,empty) (vertexNum g)

--                 aristas que quedan , arbol de salida, componenetesConexas, conjunto de vertices
kruskal' :: (Eq a, Ord a) => [Path a] -> Graph a -> Int -> Graph a
kruskal' [] treeSol compConexas = treeSol
kruskal' (p:pathsList) treeSol compConexas
    | compConexas == conectividad agregandoArista = kruskal' pathsList treeSol compConexas
    | otherwise = kruskal' pathsList agregandoArista (compConexas-1)
        where
            agregandoArista = (addPath treeSol p)


    
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
