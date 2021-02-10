-----------------------
-- CONSTRUCTORES
-----------------------

-- Árbol biselado/splay tree.
data SplayTree a = Leaf | Node a (SplayTree a) (SplayTree a)
    deriving (Eq, Show)

-- Dirección tomada en el recorrido del árbol.
data Direction = LH | RH
    deriving (Eq, Show)

-----------------------
-- FUNCIONES
-----------------------

path :: (Ord a) => a -> SplayTree a -> [(Direction, SplayTree a)] -> [(Direction, SplayTree a)]
-- Obtiene el camino en el árbol hasta el valor dado. Si no existe, su último valor será la hoja donde se insertaría un nuevo nodo 
-- Parámetros: Valor hasta el que queremos obtener el camino
--             Árbol en el que realizamos la búsqueda
--             Acumulador dónde guardamos la dirección que tomamos en cada nodo junto al subárbol en el que nos introducimos
-- Devuelve:   Lista con la con las direcciones tomadas y los subárboles correspondientes, desde la raíz al nodo con el valor.
path a Leaf ps = ps
path a (Node x l r) ps
    | a < x = path a l ((LH, l):ps)     -- Tomamos la rama izquierda
    | a > x = path a r ((RH, r):ps)     -- Tomamos la rama derecha
    | otherwise = ps                    -- Hemos alcanzado el valor a

path2tree :: (Ord a) => [(Direction,SplayTree a)] -> SplayTree a
-- Reconstruye el árbol correspondiente a partir de una lista de direcciones tomadas en un árbol y los subárboles que le corresponden a cada dirección tomada.
-- Parámetros: Lista con las direcciones tomadas en un árbol y los subárboles que le corresponden a cada decisión.
-- Devuelve:   Árbol reconstruido a partir del parámetro anterior.
path2tree ((_,n):[]) = n
path2tree ((LH,x):(_,p):[]) = zigL x p
path2tree ((RH,x):(_,p):[]) = zigR x p
path2tree ((LH,x):(LH,p):(z,g):ps) = path2tree $ (z, zigzigL x p g):ps
path2tree ((LH,x):(RH,p):(z,g):ps) = path2tree $ (z, zigzagR x p g):ps
path2tree ((RH,x):(LH,p):(z,g):ps) = path2tree $ (z, zigzagL x p g):ps
path2tree ((RH,x):(RH,p):(z,g):ps) = path2tree $ (z, zigzigR x p g):ps



--  EXPLICAR TIPOS DE ROTACIONES

zigL (Node x a b) (Node p _ c) = Node x a (Node p b c)
zigR (Node x a b) (Node p c _) = Node x (Node p c a) b

zigzigL (Node x a b) (Node p _ c) (Node g _ d) =
    Node x a (Node p b (Node g c d))

zigzigR (Node x a b) (Node p c _) (Node g d _) =
    Node x (Node p (Node g d c) a) b

zigzagL (Node x b c) (Node p a _) (Node g _ d) =
    Node x (Node p a b) (Node g c d)

zigzagR (Node x b c) (Node p _ a) (Node g d _) =
    Node x (Node g d b) (Node p c a)

splay :: (Ord a) => a -> SplayTree a -> SplayTree a
splay a t = path2tree $ path a t [(undefined,t)]

search :: (Ord a) => a -> SplayTree a -> SplayTree a
-- Busca un valor en el árbol correspondiente y aplica la operación de splay
-- Parámetros: Valor a buscar en el árbol.
--             Árbol en el que buscamos el valor.
-- Devuelve:   Árbol tras la operación de splay.
search v Leaf = error "The tree does not contain the value"
search v t@(Node x l r) = splay (getValue (snd v')) t
    where ps = path v t []                              -- Camino hasta el valor que buscamos
          contained = snd (ps !! 0) /= Leaf            -- Comprobamos si el nodo esta contenido en el árbol
          v' = if contained then ps!!0 else ps!!1
          -- Si está contenido, tomamos el último elemento del camino hasta este (será él mismo).
          -- En otro caso, tomamos el penúltimo elemento del camino hasta este (último elemento en el árbol 
          --    antes de la posición donde se encontraría el que buscamos).
          
insert :: (Ord a) => a -> SplayTree a -> SplayTree a
-- Inserta un valor en el árbol y aplica la operación de splay
-- Parámetros: Valor que introducimos.
--             Árbol en el que introducimos el valor.
-- Devuelve:   Árbol con el nuevo valor introducido tras la operación de splay.
insert v Leaf = Node v Leaf Leaf
insert v t@(Node x l r) = splay v' t'
    where ps = path v t []                                      -- Camino hasta el valor que insertamos (comprobamos así si está contenido para no insertarlo)
          contained = snd (ps !! 0) /= Leaf
          temp = if contained then (getValue (snd (ps!!0)), t) -- Si el valor está contenido solo aplicamos la operación de splay
                else (v, insert' v t)                          --   En otro caso, insertamos el valor en el árbol y aplicamos splay sobre el nuevo nodo
          v' = fst temp
          t' = snd temp

insert' :: (Eq a, Ord a) => a -> SplayTree a -> SplayTree a
-- Inserta el valor en el árbol recibido
-- Parámetros: Valor a insertar.
--             Árbol en el que insertamos el valor.
-- Devuelve:   Árbol con el valor insertado.
insert' v Leaf = Node v Leaf Leaf 
insert' v (Node n l r)
    | v < n && (isLeaf l) = Node n (Node v Leaf Leaf) r
    | v > n && (isLeaf r) = Node n l (Node v Leaf Leaf)
    | v < n = Node n (insert' v l) r
    | v > n = Node n l (insert' v r)
    | otherwise = Node n l r

delete :: (Eq a, Ord a) => a -> SplayTree a -> SplayTree a
-- Elimina el valor dado en el árbol y aplica splay sobre el padre del mismo.
-- Parámetros: Valor a eliminar.
--             Árbol en el que lo eliminamos.
-- Devuelve:   Árbol con el valor eliminado y splay aplicado sobre el padre de este
delete _ Leaf = Leaf
delete v t@(Node x l r) = splay parent removed
    where removed = delete' v t                                                 -- Eliminamos el valor del árbol
          ps = path v t []                                                      -- Camino hasta el nodo a eliminar
          contained = snd (ps !! 0) /= Leaf                                    -- Si está contenido, obtenemos el padre del mismo a partir del camino
          parent = if contained then getValue (snd (ps!!1))                     --  y aplicamos splay sobre este
                   else error "Cannot delete a node that isnt in the tree"

delete' :: (Eq a, Ord a) => a -> SplayTree a -> SplayTree a
-- Elmina el valor en el árbol dado como en un BST.
delete' _ Leaf = Leaf
delete' v (Node n l r)
    | v == n && (isLeaf l) && (isLeaf r) = Leaf  -- Caso donde no tiene hijos
    | v == n && (isLeaf l) = r                    -- Caso donde tiene un hijo
    | v == n && (isLeaf r) = l                    -- Caso donde tiene un hijo
    | v == n = Node maximo (delete' maximo l) r   -- Caso donde tiene dos hijos -> encontrar el maximo del izq
    | v < n = Node n (delete' v l) r
    | v > n = Node n l (delete' v r)
    | otherwise = error "Value not in the tree"
    where
        Just maximo = maxST l
    -- Tomamos el máximo valor en el subarbol izquierdo como nueva raíz para sustituir al nodo que hemos eliminado

maxST :: (Eq a, Ord a) => SplayTree a -> Maybe a
maxST Leaf = Nothing
maxST (Node n _ Leaf) = Just n
maxST (Node _ _ r) = maxST r

isLeaf Leaf = True
isLeaf (Node _ _ _) = False

contains v Leaf = False
contains v t@(Node x l r)
    | v < x = contains v l
    | v > x = contains v r
    | otherwise = True

getValue Leaf = error "E"
getValue (Node x l r) = x


t :: SplayTree Int
t = Node 100 (Node 50 (Node 40 (Node 30 (Node 20 Leaf Leaf) Leaf) Leaf) Leaf) (Node 200 Leaf Leaf) 

t2 :: SplayTree Int
t2 = Node 50 (Node 30 (Node 10 Leaf (Node 20 (Node 15 Leaf Leaf) Leaf)) (Node 40 Leaf Leaf)) (Node 60 Leaf (Node 90 (Node 70 Leaf Leaf) (Node 100 Leaf Leaf)))

t3 :: SplayTree Int
t3 = Node 80 (Node 60 (Node 50 (Node 30 (Node 10 Leaf (Node 20 (Node 15 Leaf Leaf) Leaf)) (Node 40 Leaf Leaf)) Leaf) (Node 70 Leaf Leaf)) (Node 90 Leaf (Node 100 Leaf Leaf))

t4 :: SplayTree Int
t4 = Node 80 (Node 60 (Node 50 (Node 20 (Node 10 Leaf (Node 15 Leaf Leaf)) (Node 40 Leaf Leaf)) Leaf) (Node 70 Leaf Leaf)) (Node 90 Leaf (Node 100 Leaf Leaf))

--Node 80 (Node 60 (Node 50 (Node 20 (Node 10 Leaf (Node 15 Leaf Leaf)) (Node 40 Leaf Leaf)) Leaf) (Node 70 Leaf Leaf)) (Node 90 Leaf (Node 100 Leaf Leaf))