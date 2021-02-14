module DataStructures.DirectedGraph(
    DGraph(..),
    Vertex(..),
    Edge(..),
    newGraph,
    addVertex,
    vertexSet,
    edgeSet,
    containsVertex,
    containsEdge,
    vertexNum,
    edgesNum,
    addEdge,
    srcVertex,
    dstVertex,
    edgesFrom,
    edgesTo,
    adjacents,
    dfs,
    bfs,
    orderedEdges,
    helpDGraph
)
where

import Data.Set as Set
import Data.List as L

-- Grafo dirigido implementado por la definición matemática, un grafo es un par G = (V, E)
-- donde V es un conjunto de vertices y E un conjunto de aristas.

-- Graph a = (V a, E a)

type DGraph a = (Set (Vertex a), Set (Edge a))

data Vertex a = V a
    deriving (Show, Eq, Ord)

-- Cada arista contiene el peso, y los vertices que une
data Edge a = P Float (Vertex a) (Vertex a)
    deriving (Show, Eq,Ord)

helpDGraph :: IO ()
helpDGraph = putStrLn "\nnewGraph :: (Eq a, Ord a) => DGraph a\n\naddVertex :: (Eq a, Ord a) => DGraph a -> Vertex a -> DGraph a\n\nvertexSet :: (Eq a, Ord a) => DGraph a -> Set (Vertex a)\n\nedgeSet :: (Eq a, Ord a) => DGraph a -> Set (Edge a)\n\ncontainsVertex :: (Eq a, Ord a) => DGraph a -> Vertex a -> Bool\n\ncontainsEdge :: (Eq a, Ord a) => DGraph a -> Edge a -> Bool\n\nvertexNum :: (Eq a, Ord a) => DGraph a -> Int\n\nedgesNum ::  (Eq a, Ord a) => DGraph a -> Int\n\naddEdge :: (Eq a, Ord a) => DGraph a -> Edge a -> DGraph a\n\nsrcVertex :: (Eq a, Ord a) => Edge a -> Vertex a\n\ndstVertex :: (Eq a, Ord a) => Edge a -> Vertex a\n\nedgesFrom :: (Eq a, Ord a) => DGraph a -> Vertex a -> Set (Edge a)\n\nedgesTo :: (Eq a, Ord a) => DGraph a -> Vertex a -> Set (Edge a)\n\nadjacents :: (Eq a, Ord a) => DGraph a -> Vertex a -> Set (Vertex a)\n\naddListVertex :: (Eq a, Ord a, Show a) => [Vertex a] -> DGraph a -> DGraph a\n\naddListEdge :: (Eq a, Ord a, Show a) => [Edge a] -> DGraph a -> DGraph a\n\nfromTupleS :: (Eq a, Ord a, Show a) => (Set (Vertex a),Set (Edge a)) -> DGraph a\n\nfromTupleL :: (Eq a, Ord a, Show a) => ([Vertex a],[Edge a]) -> DGraph a\n\ndfs :: (Eq a, Ord a) => DGraph a -> Vertex a -> [Vertex a]\n\nbfs :: (Eq a, Ord a) => DGraph a -> Vertex a -> [Vertex a]\n\norderedEdges :: (Eq a, Ord a) => DGraph a -> [Edge a]"

newGraph :: (Eq a, Ord a) => DGraph a
-- Crea un grafo dirigido vacio
-- Entradas: 
-- Salidas: grafo
--      grafo (DGraph a): Un grafo de la forma (empty, empty)
newGraph = (empty , empty)

addVertex :: (Eq a, Ord a) => DGraph a -> Vertex a -> DGraph a
-- Añade un vertice al grafo
-- Entradas: grafo -> v
--      grafo (DGraph a): Grafo al que se añade el vertice
--      v (Vertex a): Vertice a añadir
--  Salidas: insertado
--      insertado (DGraph a): Un grafo con el vertice insertado
addVertex (setV, setP) v = (Set.insert v setV, setP)

vertexSet :: (Eq a, Ord a) => DGraph a -> Set (Vertex a)
-- Conjunto de vertices
-- Entradas: grafo
--      grafo (DGraph a): El grafo del que queremos el conjunto de vertices
-- Salidas: s
--      s (Set(Vertex a)): Conjunto de vertices del grafo
vertexSet g = (fst g)

edgeSet :: (Eq a, Ord a) => DGraph a -> Set (Edge a)
-- Conjunto de aristas
-- Entradas: grafo
--      grafo (DGraph a): El grafo del que queremos el conjunto de aristas
-- Salidas: s
--      s (Set(Edge a)): Conjunto de aristas del grafo
edgeSet g = (snd g)

containsVertex :: (Eq a, Ord a) => DGraph a -> Vertex a -> Bool
-- Comprueba si el grafo contiene un vertice dado
-- Entradas: g -> v
--      g (DGraph a): Grafo del que queremos comprobar si contiene a v
--      v (Vertex a): Vertice sobre el que nos preguntamos si es contenido por g
-- Salidas: p
--      p (Bool): True en caso de v estar contenida en g, False en otro caso
containsVertex g v = Set.size (fst g) == Set.size (Set.insert v (fst g))

containsEdge :: (Eq a, Ord a) => DGraph a -> Edge a -> Bool
-- Comprueba si el grafo contiene una arista dada
-- Entradas: g -> v
--      g (DGraph a): Grafo del que queremos comprobar si contiene a p
--      p (Edge a): Vertice sobre el que nos preguntamos si es contenido por g
-- Salidas: s
--      s (Bool): True en caso de p estar contenida en g, False en otro caso
containsEdge g p = Set.size (snd g) == Set.size (Set.insert p (snd g))

vertexNum :: (Eq a, Ord a) => DGraph a -> Int
-- Consigue el numero de vertices del grafo
-- Entradas: g
--      g (DGraph a): Grafo del que queremos saber el numero de vertices
-- Salidas: n
--      n (Int): Numero de vertices del grafo
vertexNum g = length $ vertexSet g

edgesNum ::  (Eq a, Ord a) => DGraph a -> Int
-- Consigue el numero de aristas del grafo
-- Entradas: g
--      g (DGraph a): Grafo del que queremos saber el numero de aristas
-- Salidas: n
--      n (Int): Numero de aristas del grafo
edgesNum g = length $ edgeSet g

addEdge :: (Eq a, Ord a) => DGraph a -> Edge a -> DGraph a
-- Añade una arista al grafo
-- Entradas: grafo -> v
--      grafo (DGraph a): Grafo al que se añade la arista
--      v (Edge a): Arista a añadir
--  Salidas: insertado
--      insertado (DGraph a): Un grafo con la arista insertado
addEdge g@(bstV,bstP) p@(P _ v1 v2)
    | not (containsVertex g v1) && not (containsVertex g v2) = 
        error " --> No existe ninguno de los vertices de la arista <-- "
    | not (containsVertex g v1) = error " --> No existe el primer vertice de la arista <-- "
    | not (containsVertex g v2) = error " --> No existe el segundo vertice de la arista <-- "
    | otherwise = (bstV, Set.insert p bstP)

srcVertex :: (Eq a, Ord a) => Edge a -> Vertex a
-- Vertice fuente de una arista
-- Entradas: p
--      p (Edge a): Arista sobre la que queremos saber su vertice fuente
-- Salidas: v
--      v (Vertex a): Vertice fuente de p
srcVertex (P _ v _) = v

dstVertex :: (Eq a, Ord a) => Edge a -> Vertex a
-- Vertice destino de una arista
-- Entradas: p
--      p (Edge a): Arista sobre la que queremos saber su vertice destino
-- Salidas: v
--      v (Vertex a): Vertice destino de p
dstVertex (P _ _ v) = v


edgesFrom :: (Eq a, Ord a) => DGraph a -> Vertex a -> Set (Edge a)
-- Conjunto de aristas que salen de v
-- Entradas: g -> v
--      g (Graph a): Grafo en el que buscamos
--      v (Vertex a): Vertice del que queremos sus aristas de las cuales es vertice fuente
-- Salidas: s
--      s (Set (Edge a)): Conjunto de aristas de las cuales v es vertice fuente
edgesFrom g v = Set.filter (\x -> srcVertex x == v) (snd g)

-- Lista de aristas que llegan al vertice v
edgesTo :: (Eq a, Ord a) => DGraph a -> Vertex a -> Set (Edge a)
-- Conjunto de aristas que llegan a v
-- Entradas: g -> v
--      g (Graph a): Grafo en el que buscamos
--      v (Vertex a): Vertice del que queremos sus aristas de las cuales es vertice destino
-- Salidas: s
--      s (Set (Edge a)): Conjunto de aristas de las cuales v es vertice destino
edgesTo g v = Set.filter (\x -> dstVertex x == v) (snd g)


adjacents :: (Eq a, Ord a) => DGraph a -> Vertex a -> Set (Vertex a)
-- Obtener el conjunto de vertices adyacentes a uno dado en un grafo, un vertice a es 
-- adyacente a un vertice b si y solo si existe una arista que toma como vertice 
-- fuente a y vertice destino b. 
-- Entradas: g -> v
--      g (DGraph a): Grafo en el que buscamos
--      v (Vertex a): Vertice del cual buscamos sus vertices adyacentes
-- Salidas: s
--      s (Set (Vertex a)): Conjunto de vertices que son adyacentes a v
adjacents g v = Set.map dstVertex $ edgesFrom g v


addListVertex :: (Eq a, Ord a, Show a) => [Vertex a] -> DGraph a -> DGraph a
-- Añadir todos los vertices de una lista a un grafo
-- Entradas: xs -> g
--      xs ([Vertex a]): Lista de vertices a añadir en el grafo
--      g (DGraph a): Grafo donde queremos añadir los vertices
-- Salidas: grafo
--      grafo (DGraph a): Grafo con los nuevos vertices añadidos
addListVertex xs g = L.foldl addVertex g xs

-- Añadir todas las aristas de una lista
addListEdge :: (Eq a, Ord a, Show a) => [Edge a] -> DGraph a -> DGraph a
-- Añadir todos las aristas de una lista a un grafo
-- Entradas: xs -> g
--      xs ([Vertex a]): Lista de aristas a añadir en el grafo
--      g (DGraph a): Grafo donde queremos añadir las aristas
-- Salidas: grafo
--      grafo (DGraph a): Grafo con los nuevas aristas añadidos
addListEdge xs g = L.foldl addEdge g xs


fromTupleL :: (Eq a, Ord a, Show a) => ([Vertex a],[Edge a]) -> DGraph a
-- Nuevo grafo a partir de un par de listas de vertices y aristas
-- Entradas: (vert, edge)
--      vert ([Vertex a]): Lista de vertices a añadir
--      edge ([Edge a]): Lista de aristas a añadir
-- Salidas: grafo
--      grafo (DGraph a): El grado descrito
fromTupleL (vert, edge) = addListEdge edge (Set.fromList vert, empty)


fromTupleS :: (Eq a, Ord a, Show a) => (Set (Vertex a),Set (Edge a)) -> DGraph a
-- Nuevo grafo a partir de un par de conjuntos de vertices y aristas
-- Entradas: (vert, edge)
--      vert (Set (Vertex a)): Conjunto de vertices a añadir
--      edge (Set (Edge a)): Conjunto de aristas a añadir
-- Salidas: grafo
--      grafo (DGraph a): El grado descrito
fromTupleS (vert, edge) = Set.foldl addEdge (vert,empty) edge


-- Recorrido en profundidad
dfs :: (Eq a, Ord a) => DGraph a -> Vertex a -> [Vertex a]
-- Realiza un recorrido en profundidad del grafo.
-- Entradas: g -> v
--      g (DGraph a): Grafo sobre el que se va a realizar el recorrido
--      v (Vertex a): Vertice de inicio del recorrido
-- Salidas: xs
--      xs ([Vertex a]): Lista de vertices en orden de visitados, los primeros son
--                       los que se han visitado antes, y los ultimo los que se han
--                       visitado mas tarde
dfs g v = dfsAux g (Set.toList (adjacents g v)) [v]

--                                   "PILA" Vertices
--                          Grafo     a recorrer     recorridos      Sol
dfsAux :: (Eq a, Ord a) => DGraph a -> [Vertex a] -> [Vertex a] -> [Vertex a]
dfsAux g [] ys = ys
dfsAux g (x:xs) ys = if elem x ys then dfsAux g xs ys else dfsAux g (newEdges++xs) (ys++[x])
    where
        adjToX = (adjacents g x)
        newEdges = Set.toList $ Set.difference adjToX (Set.fromList (ys++[x]))


-- Siendo x el vertice por el que se va, en cada llamada recursiva, se añade a la pila los adyacentes a x
-- exceptuando los que ya han sido recorridos (incluyendo x) y se añade x a los recorridos.


bfs :: (Eq a, Ord a) => DGraph a -> Vertex a -> [Vertex a]
-- Realiza un recorrido en anchura del grafo.
-- Entradas: g -> v
--      g (DGraph a): Grafo sobre el que se va a realizar el recorrido
--      v (Vertex a): Vertice de inicio del recorrido
-- Salidas: xs
--      xs ([Vertex a]): Lista de vertices en orden de visitados, los primeros son
--                       los que se han visitado antes, y los ultimo los que se han
--                       visitado mas tarde
bfs g v = bfsAux g (Set.toList (adjacents g v)) [v]

bfsAux :: (Eq a, Ord a) => DGraph a -> [Vertex a] -> [Vertex a] -> [Vertex a]
bfsAux g [] ys = ys
bfsAux g (x:xs) ys = if elem x ys then bfsAux g xs ys else bfsAux g (xs++newEdges) (ys++[x])
    where
        adjToX = (adjacents g x)
        newEdges = Set.toList $ Set.difference adjToX (Set.fromList (ys++[x]))

orderedEdges :: (Eq a, Ord a) => DGraph a -> [Edge a]
orderedEdges g = sort (Set.toList (edgeSet g))


g, g' :: DGraph Int
g = addListVertex [(V 1), (V 2), (V 3),(V 4),(V 5),(V 6),(V 7), (V 8), (V 9), (V 10),(V 11), (V 12),(V 13),(V 14)] (empty,empty)
g' = addListEdge [(P 1 (V 1) (V 2)),
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



-- Considerando el grafo dirigido
{-
kruskal :: (Eq a, Ord a) => DGraph a -> Forest a
--                                        
kruskal g = kruskal' 
    g                   -- Grafo de entrada
    (orderedEdges g)    -- aristas ordenadas por peso
    []                  -- bosque solucion
    0                   -- indice del bosque (Indica por que arbol va)
    (vertexSet g)       -- vertices por poner

-- kruskal' :: (Eq a, Ord a) => DGraph a -> (Set (Edge a)) -> Forest a -> Int -> (Set (Vertex a)) -> Forest a
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