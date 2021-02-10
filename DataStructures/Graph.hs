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
    addListVertex,
    addListPath,
    fromTuple,
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

-- Añadir todos los vertices de una lista
addListVertex :: (Eq a, Ord a) => [Vertex a] -> Graph a -> Graph a
addListVertex xs g = L.foldl addVertex g xs

-- Añadir todas las aristas de una lista
addListPath :: (Eq a, Ord a) => [Path a] -> Graph a -> Graph a
addListPath xs g = L.foldl addPath g xs

-- Nuevo grafo a partir de un par de listas de vertices y aristas
fromTuple :: (Eq a, Ord a) => ([Vertex a],[Path a]) -> Graph a
fromTuple (vert, path) = (Set.fromList vert, Set.fromList path)

-- Recorrido en profundidad
dfs :: (Eq a, Ord a) => Graph a -> Vertex a -> [Vertex a]
dfs g v = dfsAux g (Set.toList (adjacents g v)) [v]

{-
Actual      Pila        Recorrido
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
dfsAux g (x:xs) ys = if elem x ys then dfsAux g xs ys else dfsAux g (newPaths++xs) (ys++[x]) -- Hay que tener cuidado por si se ha añadido ya
    where
        adjToX = (adjacents g x)
        newPaths = Set.toList $ Set.difference adjToX (Set.fromList (ys++[x]))

-- Siendo x el vertice por el que se va, en cada llamada recursiva, se añade a la pila los adyacentes a x
-- exceptuando los que ya han sido recorridos (incluyendo x) y se añade x a los recorridos.


bfs :: (Eq a, Ord a) => Graph a -> Vertex a -> [Vertex a]
bfs g v = bfsAux g (Set.toList (adjacents g v)) [v]

bfsAux :: (Eq a, Ord a) => Graph a -> [Vertex a] -> [Vertex a] -> [Vertex a]
bfsAux g [] ys = ys
bfsAux g (x:xs) ys = if elem x ys then bfsAux g xs ys else bfsAux g (xs++newPaths) (ys++[x]) 
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
--                  El grafo, vertice de salida, vertice de destino
dijkstra :: (Eq a, Ord a) => Graph a -> Vertex a -> Vertex a -> [Path a]
dijkstra g@(vertex,path) vSrc vDst = getShortestPath (dijkstra' (,empty) path) vSrc vDst
    where 
        getShortestPath g vSrc vDst
            | 

--                  (Grafo con el vertices anteriores, distancia recorrida desde origen, Maybe vertice anterior)
dijkstra' :: (Eq a, Ord a) => Graph (a, Int, Maybe a) -> Vertex a -> Vertex a -> Graph (a, Int, Maybe a)
dijkstra' g@(vertex,path) vSrc vDst
-}


{-
--    El vertice del grafo anterior, (La distancia desde el orignen, Vertice anterior)
type DNode a = (Vertex a, (Float, Vertex a))

-- Given a graph and a start node
dijkstra :: Graph a -> Vertex a -> [DNode a]
dijkstra g start = 
  let dnodes = initD g start
      unchecked = L.map fst dnodes
  in  dijkstra' g dnodes unchecked

  -- Given a graph and a start node, construct an initial list of Dnodes
initD :: Graph a -> Vertex a -> [DNode a]
initD g start =
  let initDist (n, es) = 
        if n == start 
        then 0 
        else if start `elem` connectedNodes es
             then weightFor start es
             else 1.0/0.0
  in L.map (\pr@(n, _) -> (n, ((initDist pr), start))) g

-- Dijkstra's algorithm (recursive)
-- get a list of Dnodes that haven't been checked yet
-- select the one with minimal distance and add it to the checked list. Call it current.
-- update each Dnode that connects to current by comparing 
-- the Dnode's current distance to the sum: (weight of the connecting edge + current's distance)
-- the algorithm terminates when all nodes have been checked.
dijkstra' :: Graph a -> [DNode a] -> [Vertex a] -> [DNode a]
dijkstra' g dnodes [] = dnodes
dijkstra' g dnodes unchecked = 
  let dunchecked = L.filter (\dn -> (fst dn) `elem` unchecked) dnodes
      current = head . sortBy (\(_,(d1,_)) (_,(d2,_)) -> compare d1 d2) $ dunchecked
      c = fst current
      unchecked' = L.delete c unchecked
      edges = edgesFor g c
      cnodes = intersect (connectedNodes edges) unchecked'
      dnodes' = L.map (\dn -> update dn current cnodes edges) dnodes
  in dijkstra' g dnodes' unchecked' 

-- given a Dnode to update, the current Dnode, the Nodes connected to current 
-- and current's edges, return a (possibly) updated Dnode
update :: DNode a -> DNode a -> [Vertex a] -> [Path a] -> DNode a
update dn@(n, (nd, p)) (c, (cd, _)) cnodes edges =
  let wt = weightFor n edges
  in  if n `notElem` cnodes then dn
      else if cd+wt < nd then (n, (cd+wt, c)) else dn

-- given a Dijkstra solution and a destination node, return the path to it.
pathToNode :: [DNode a] -> Vertex a -> [Vertex a]
pathToNode dnodes dest = 
  let dn@(n, (d, p)) = dnodeForNode dnodes dest
  in if n == p then [n] else pathToNode dnodes p ++ [n]


------------------------

appendReversed :: [((String, String), Float)] -> [((String, String), Float)]
appendReversed es = es ++ L.map (\((n1,n2),w) -> ((n2,n1),w)) es

{-
-- Takes a list of pairs where the first element is a two-member list 
-- of nodes in any order and the second element is the weight for the edge connecting them.
fromList :: [((String, String), Float)] -> Graph a
fromList es =
  let nodes = nub . L.map (fst . fst) $ es
      edgesFor es node = 
        let connected = L.filter (\((n,_),_) -> node == n) $ es
        in L.map (\((_,n),wt) -> Edge n wt) connected 
  in L.map (\n -> (n, edgesFor es n)) nodes
-}

-- Given a weighted graph and a node, return the edges incident on the node
edgesFor :: Graph a -> Vertex a -> [Path a]
edgesFor g n = snd . head . L.filter (\(nd, _) -> nd == n) $ g

-- Given a node and a list of edges, one of which is incident on the node, return the weight
weightFor :: Vertex a -> [Path a] -> Float
weightFor n = weight . head . L.filter (\e -> n == node e)

-- Given a list of edges, return their nodes
connectedNodes :: [Path a] -> [Vertex a]
connectedNodes = L.map node

dnodeForNode :: [DNode a] -> Vertex a -> DNode a
dnodeForNode dnodes n = head . L.filter (\(x, _) -> x == n) $ dnodes
-}