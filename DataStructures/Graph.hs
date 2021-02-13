module DataStructures.Graph(
    Graph(..),
    Vertex(..),
    Path(..),
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
    pathsFrom,
    adjacents,
    addListVertex,
    addListPath,
    fromTupleL,
    fromTupleS,
    dfs,
    bfs,
    orderedPaths,
    conexo,
    conectividad,
    kruskal,
    helpGraph
) where

import Data.Set as Set
import Data.List as L

-- Grafo implementado por la definición matemática, un grafo es un par G = (V, E)
-- donde V es un conjunto de vertices y E un conjunto de aristas.

-- Graph a = (V a, E a)
type Graph a = (Set (Vertex a), Set (Path a))

data Vertex a = V a
    deriving (Show, Eq, Ord)

-- Cada arista contiene el peso, y los vertices que une
data Path a = P Float (Vertex a) (Vertex a)
    deriving (Show, Eq,Ord)


helpGraph :: IO ()
helpGraph = putStrLn "\nnewGraph :: (Eq a, Ord a, Show a) => Graph a\n\naddVertex :: (Eq a, Ord a, Show a) => Graph a -> Vertex a -> Graph a\n\nvertexSet :: (Eq a, Ord a, Show a) => Graph a -> Set (Vertex a)\n\npathSet :: (Eq a, Ord a, Show a) => Graph a -> Set (Path a)\n\ncontainsVertex :: (Eq a, Ord a, Show a) => Graph a -> Vertex a -> Bool\n\ncontainsPath :: (Eq a, Ord a, Show a) => Graph a -> Path a -> Bool\n\nvertexNum :: (Eq a, Ord a, Show a) => Graph a -> Int\n\npathsNum ::  (Eq a, Ord a, Show a) => Graph a -> Int\n\naddPath :: (Eq a, Ord a, Show a) => Graph a -> Path a -> Graph a\n\nsrcVertex :: (Eq a, Ord a, Show a) => Path a -> Vertex a\n\ndstVertex :: (Eq a, Ord a, Show a) => Path a -> Vertex a\n\npathsOf :: (Eq a, Ord a, Show a) => Graph a -> Vertex a -> Set (Path a)\n\npathsFrom :: (Eq a, Ord a, Show a) => Graph a -> Vertex a -> Set (Path a)\n\npathsTo :: (Eq a, Ord a, Show a) => Graph a -> Vertex a -> Set (Path a)\n\ngetWeight :: (Eq a, Ord a, Show a) => Path a -> Float\n\ngetSource :: (Eq a, Ord a, Show a) => Path a -> Vertex a\n\ngetDest :: (Eq a, Ord a, Show a) => Path a -> Vertex a\n\nadjacents :: (Eq a, Ord a, Show a) => Graph a -> Vertex a -> Set (Vertex a)\n\naddListVertex :: (Eq a, Ord a, Show a) => [Vertex a] -> Graph a -> Graph a\n\naddListPath :: (Eq a, Ord a, Show a) => [Path a] -> Graph a -> Graph a\n\nfromTuple :: (Eq a, Ord a, Show a) => ([Vertex a],[Path a]) -> Graph a\n\ndfs :: (Eq a, Ord a, Show a) => Graph a -> Vertex a -> [Vertex a]\n\nbfs :: (Eq a, Ord a, Show a) => Graph a -> Vertex a -> [Vertex a]\n\norderedPaths :: (Eq a, Ord a, Show a) => Graph a -> [Path a]\n\nconexo :: (Eq a, Ord a, Show a) => Graph a -> Bool\n\nconectividad :: (Eq a, Ord a, Show a) => Graph a -> Int\n\nkruskal :: (Eq a, Ord a, Show a) => (Eq a, Ord a, Show a) => Graph a -> Graph a "

newGraph :: (Eq a, Ord a, Show a) => Graph a
-- Crea un grafo vacio
-- Entradas: 
-- Salidas: grafo
--      grafo (Graph a): Un grafo de la forma (empty, empty)
newGraph = (empty , empty)

addVertex :: (Eq a, Ord a, Show a) => Graph a -> Vertex a -> Graph a
-- Añade un vertice al grafo
-- Entradas: grafo -> v
--      grafo (Graph a): Grafo al que se añade el vertice
--      v (Vertex a): Vertice a añadir
--  Salidas: insertado
--      insertado (Graph a): Un grafo con el vertice insertado
addVertex (setV, setP) v = (Set.insert v setV, setP)

vertexSet :: (Eq a, Ord a, Show a) => Graph a -> Set (Vertex a)
-- Conjunto de vertices
-- Entradas: grafo
--      grafo (Graph a): El grafo del que queremos el conjunto de vertices
-- Salidas: s
--      s (Set(Vertex a)): Conjunto de vertices del grafo
vertexSet g = (fst g)

pathSet :: (Eq a, Ord a, Show a) => Graph a -> Set (Path a)
-- Conjunto de aristas
-- Entradas: grafo
--      grafo (Graph a): El grafo del que queremos el conjunto de aristas
-- Salidas: s
--      s (Set(Path a)): Conjunto de aristas del grafo
pathSet g = (snd g)

containsVertex :: (Eq a, Ord a, Show a) => Graph a -> Vertex a -> Bool
-- Comprueba si el grafo contiene un vertice dado
-- Entradas: g -> v
--      g (Graph a): Grafo del que queremos comprobar si contiene a v
--      v (Vertex a): Vertice sobre el que nos preguntamos si es contenido por g
-- Salidas: p
--      p (Bool): True en caso de v estar contenida en g, False en otro caso
containsVertex g v = Set.size (fst g) == Set.size (Set.insert v (fst g))

containsPath :: (Eq a, Ord a, Show a) => Graph a -> Path a -> Bool
-- Comprueba si el grafo contiene una arista dada
-- Entradas: g -> v
--      g (Graph a): Grafo del que queremos comprobar si contiene a p
--      p (Path a): Vertice sobre el que nos preguntamos si es contenido por g
-- Salidas: s
--      s (Bool): True en caso de p estar contenida en g, False en otro caso
containsPath g p = Set.size (snd g) == Set.size (Set.insert p (snd g))

vertexNum :: (Eq a, Ord a, Show a) => Graph a -> Int
-- Consigue el numero de vertices del grafo
-- Entradas: g
--      g (Graph a): Grafo del que queremos saber el numero de vertices
-- Salidas: n
--      n (Int): Numero de vertices del grafo
vertexNum g = length $ vertexSet g

pathsNum ::  (Eq a, Ord a, Show a) => Graph a -> Int
-- Consigue el numero de aristas del grafo
-- Entradas: g
--      g (Graph a): Grafo del que queremos saber el numero de aristas
-- Salidas: n
--      n (Int): Numero de aristas del grafo
pathsNum g = length $ pathSet g

addPath :: (Eq a, Ord a, Show a) => Graph a -> Path a -> Graph a
-- Añade una arista al grafo
-- Entradas: grafo -> v
--      grafo (Graph a): Grafo al que se añade la arista
--      v (Path a): Arista a añadir
--  Salidas: insertado
--      insertado (Graph a): Un grafo con la arista insertado
addPath g@(bstV,bstP) p@(P _ v1 v2)
    | not (containsVertex g v1) && not (containsVertex g v2) = 
        error $ concat [" --> No existe ninguno de los vertices de la arista <-- ", (show v1), " ", (show v2)]
    | not (containsVertex g v1) = error $ concat [" --> No existe el primer vertice de la arista <-- ", (show v1)]
    | not (containsVertex g v2) = error $ concat [" --> No existe el segundo vertice de la arista <-- ", (show v2)]
    | containsPath g p = g
    | otherwise = (bstV, Set.insert p bstP)


srcVertex :: (Eq a, Ord a, Show a) => Path a -> Vertex a
-- Vertice fuente de una arista
-- Entradas: p
--      p (Path a): Arista sobre la que queremos saber su vertice fuente
-- Salidas: v
--      v (Vertex a): Vertice fuente de p
srcVertex (P _ v _) = v

dstVertex :: (Eq a, Ord a, Show a) => Path a -> Vertex a
-- Vertice destino de una arista
-- Entradas: p
--      p (Path a): Arista sobre la que queremos saber su vertice destino
-- Salidas: v
--      v (Vertex a): Vertice destino de p
dstVertex (P _ _ v) = v


pathsOf :: (Eq a, Ord a, Show a) => Graph a -> Vertex a -> Set (Path a)
-- Conjunto de aristas que salen de v o llegan a v
-- Entradas: g -> v
--      g (Graph a): Grafo en el que buscamos
--      v (Vertex a): Vertice del que queremos sus aristas
-- Salidas: s
--      s (Set (Path a)): Conjunto de aristas relacionadas con v
pathsOf g v = Set.filter (\x -> srcVertex x == v || dstVertex x == v) (snd g)


pathsFrom :: (Eq a, Ord a, Show a) => Graph a -> Vertex a -> Set (Path a)
-- Conjunto de aristas que salen de v
-- Entradas: g -> v
--      g (Graph a): Grafo en el que buscamos
--      v (Vertex a): Vertice del que queremos sus aristas de las cuales es vertice fuente
-- Salidas: s
--      s (Set (Path a)): Conjunto de aristas de las cuales v es vertice fuente
pathsFrom g v = Set.filter (\x -> srcVertex x == v) (snd g)

-- Lista de aristas que llegan al vertice v
pathsTo :: (Eq a, Ord a, Show a) => Graph a -> Vertex a -> Set (Path a)
-- Conjunto de aristas que llegan a v
-- Entradas: g -> v
--      g (Graph a): Grafo en el que buscamos
--      v (Vertex a): Vertice del que queremos sus aristas de las cuales es vertice destino
-- Salidas: s
--      s (Set (Path a)): Conjunto de aristas de las cuales v es vertice destino
pathsTo g v = Set.filter (\x -> dstVertex x == v) (snd g)

getWeight :: (Eq a, Ord a, Show a) => Path a -> Float
-- Obtener el peso de una arista
-- Entradas: p
--      p (Path a): Arista sobre la que nos preguntamos su peso
-- Salidas: w
--      w (Float): Peso de la arista
getWeight (P w _ _) = w

-- Lista de vertices adyacentes a uno dado
adjacents :: (Eq a, Ord a, Show a) => Graph a -> Vertex a -> Set (Vertex a)
-- Obtener el conjunto de vertices adyacentes a uno dado en un grafo
-- Entradas: g -> v
--      g (Graph a): Grafo en el que buscamos
--      v (Vertex a): Vertice del cual buscamos sus vertices adyacentes
-- Salidas: s
--      s (Set (Vertex a)): Conjunto de vertices que son adyacentes a v
adjacents g v = Set.map dstVertex (pathsFrom g v) `Set.union` Set.map srcVertex (pathsTo g v)


addListVertex :: (Eq a, Ord a, Show a) => [Vertex a] -> Graph a -> Graph a
-- Añadir todos los vertices de una lista a un grafo
-- Entradas: xs -> g
--      xs ([Vertex a]): Lista de vertices a añadir en el grafo
--      g (Graph a): Grafo donde queremos añadir los vertices
-- Salidas: grafo
--      grafo (Graph a): Grafo con los nuevos vertices añadidos
addListVertex xs g = L.foldl addVertex g xs

-- Añadir todas las aristas de una lista
addListPath :: (Eq a, Ord a, Show a) => [Path a] -> Graph a -> Graph a
-- Añadir todos las aristas de una lista a un grafo
-- Entradas: xs -> g
--      xs ([Vertex a]): Lista de aristas a añadir en el grafo
--      g (Graph a): Grafo donde queremos añadir las aristas
-- Salidas: grafo
--      grafo (Graph a): Grafo con los nuevas aristas añadidos
addListPath xs g = L.foldl addPath g xs


fromTupleL :: (Eq a, Ord a, Show a) => ([Vertex a],[Path a]) -> Graph a
-- Nuevo grafo a partir de un par de listas de vertices y aristas
-- Entradas: (vert, path)
--      vert ([Vertex a]): Lista de vertices a añadir
--      path ([Path a]): Lista de aristas a añadir
-- Salidas: grafo
--      grafo (Graph a): El grado descrito
fromTupleL (vert, path) = addListPath path (Set.fromList vert, empty)

fromTupleS :: (Eq a, Ord a, Show a) => (Set (Vertex a),Set (Path a)) -> Graph a
-- Nuevo grafo a partir de un par de conjuntos de vertices y aristas
-- Entradas: (vert, path)
--      vert (Set (Vertex a)): Conjunto de vertices a añadir
--      path (Set (Path a)): Conjunto de aristas a añadir
-- Salidas: grafo
--      grafo (Graph a): El grado descrito
fromTupleS (vert, path) = Set.foldl addPath (vert,empty) path

-- Recorrido en profundidad
dfs :: (Eq a, Ord a, Show a) => Graph a -> Vertex a -> [Vertex a]
-- Realiza un recorrido en profundidad del grafo.
-- Entradas: g -> v
--      g (Graph a): Grafo sobre el que se va a realizar el recorrido
--      v (Vertex a): Vertice de inicio del recorrido
-- Salidas: xs
--      xs ([Vertex a]): Lista de vertices en orden de visitados, los primeros son
--                       los que se han visitado antes, y los ultimo los que se han
--                       visitado mas tarde
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
dfsAux :: (Eq a, Ord a, Show a) => Graph a -> [Vertex a] -> [Vertex a] -> [Vertex a]
dfsAux g [] ys = ys
dfsAux g (x:xs) ys = if elem x ys then dfsAux g xs ys else dfsAux g (newPaths++xs) (ys++[x]) -- Hay que tener cuidado por si se ha añadido ya
    where
        adjToX = (adjacents g x)
        newPaths = Set.toList $ Set.difference adjToX (Set.fromList (ys++[x]))

-- Siendo x el vertice por el que se va, en cada llamada recursiva, se añade a la pila los adyacentes a x
-- exceptuando los que ya han sido recorridos (incluyendo x) y se añade x a los recorridos.


bfs :: (Eq a, Ord a, Show a) => Graph a -> Vertex a -> [Vertex a]
-- Realiza un recorrido en anchura del grafo.
-- Entradas: g -> v
--      g (Graph a): Grafo sobre el que se va a realizar el recorrido
--      v (Vertex a): Vertice de inicio del recorrido
-- Salidas: xs
--      xs ([Vertex a]): Lista de vertices en orden de visitados, los primeros son
--                       los que se han visitado antes, y los ultimo los que se han
--                       visitado mas tarde
bfs g v = bfsAux g (Set.toList (adjacents g v)) [v]

bfsAux :: (Eq a, Ord a, Show a) => Graph a -> [Vertex a] -> [Vertex a] -> [Vertex a]
bfsAux g [] ys = ys
bfsAux g (x:xs) ys = if elem x ys then bfsAux g xs ys else bfsAux g (xs++newPaths) (ys++[x]) 
    where
        adjToX = (adjacents g x)
        newPaths = Set.toList $ Set.difference adjToX (Set.fromList (ys++[x]))

orderedPaths :: (Eq a, Ord a, Show a) => Graph a -> [Path a]
-- Lista de aristas del grafo ordenadas por peso
-- Entradas: g
--      g (Graph a): Grafo del que se quieren las aristas ordenadas
-- Salidas: xs
--      xs ([Path a]): Lista de aristas ordenadas por peso
orderedPaths g = sort (Set.toList (pathSet g))


conexo :: (Eq a, Ord a, Show a) => Graph a -> Bool
-- Ver si es un grafo conexo
-- Entradas: g
--      g (Graph a): Grafo sobre el que nos preguntamos si es conexo
-- Salidas: p
--      p (Bool): En caso de ser conexo es True, en otro caso False
conexo g = vertexNum g == length (bfs g (Set.elemAt 0 (vertexSet g)))

conectividad :: (Eq a, Ord a, Show a) => Graph a -> Int
-- Obtiene el numero de componentes conexas de un grafo
-- Entradas: g
--      g (Graph a): Grafo del que queremos saber sus componentes conexas
-- Salidas: n
--      n (Int): Numero del componentes conexas de g
conectividad g = conectividad' g (vertexSet g) 0 empty

conectividad' :: (Eq a, Ord a, Show a) => Graph a -> Set (Vertex a) -> Int -> Set (Vertex a) -> Int
conectividad' g porVisitar num visitados
    | length visitados == length (vertexSet g) = num
    | otherwise = conectividad' g (Set.difference (vertexSet g) nuevoVisitados) (num+1) nuevoVisitados
        where 
            x = Set.elemAt 0 porVisitar
            nuevoVisitados = (Set.union (Set.fromList (dfs g x)) visitados)



kruskal :: (Eq a, Ord a, Show a) => (Eq a, Ord a, Show a) => Graph a -> Graph a 
-- Obtiene un arbol recubridor minimo (con menor suma de pesos), el arbol en formato de grafo.
-- En caso de que el grafo no fuese conexo inicialmente, se obtiene un bosque.
-- Entradas: g
--      g (Graph a): El grafo del que queremos el arbol recubridor de minimos pesos
-- Salidas: arbol
--      arbol (Graph a): Arbol recubridor de pesos minimos en formato de arbol
kruskal g@(vertex,_) = kruskal' (orderedPaths g) (vertex,empty) (vertexNum g) (conectividad g)

--aristas que quedan-> arbol de salida-> componenetesConexas-> componentesConexas del original-> conjunto de vertices
kruskal' :: (Eq a, Ord a, Show a) => [Path a] -> Graph a -> Int -> Int -> Graph a
kruskal' [] treeSol compConexas _ = treeSol
kruskal' (p:pathsList) treeSol compConexas compConexasOriginal
    -- Caso donde ya se tienen las mismas componentes conexas que al inicio
    | compConexas == compConexasOriginal = treeSol
    -- Caso donde el numero de componentes conexas tras agregar la arista es el mismo
    | compConexas == conectividad agregandoArista = kruskal' pathsList treeSol compConexas compConexasOriginal
    -- En otro caso se deja agragada la nueva arista
    | otherwise = kruskal' pathsList agregandoArista (compConexas-1) compConexasOriginal
        where
            agregandoArista = (addPath treeSol p)


g, g' :: Graph Int
g = addListVertex [(V 1), (V 2), (V 3),(V 4),(V 5),(V 6),(V 7), (V 8), (V 9), (V 10),(V 11), (V 12),(V 13),(V 14)] (empty,empty)
g' = addListPath [(P 1 (V 1) (V 2)),
    (P 1 (V 1) (V 10)),
        (P 1 (V 2) (V 3)),
            (P 1 (V 2) (V 12)),
                (P 1 (V 3) (V 4)),
                    (P 1 (V 3) (V 12)),
                        (P 1 (V 4) (V 11)),
                            (P 1 (V 5) (V 6)),
                                (P 1 (V 5) (V 9)),
                                    (P 1 (V 5) (V 7)),
                                        (P 1 (V 6) (V 8)),
                                            (P 1 (V 6) (V 7)),
                                                (P 1 (V 7) (V 8)),
                                                    (P 1 (V 8) (V 9)),
                                                        (P 1 (V 10) (V 11)),
                                                            (P 1 (V 10) (V 12)),
                                                                (P 1 (V 13) (V 14))] g
{-
--                  El grafo, vertice de salida, vertice de destino
dijkstra :: (Eq a, Ord a, Show a) => Graph a -> Vertex a -> Vertex a -> [Path a]
dijkstra g@(vertex,path) vSrc vDst = getShortestPath (dijkstra' (,empty) path) vSrc vDst
    where 
        getShortestPath g vSrc vDst
            | 

--                  (Grafo con el vertices anteriores, distancia recorrida desde origen, Maybe vertice anterior)
dijkstra' :: (Eq a, Ord a, Show a) => Graph (a, Int, Maybe a) -> Vertex a -> Vertex a -> Graph (a, Int, Maybe a)
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