module DataStructures.AVLTree(
  AVL(..),
  insert,
  contains,
  value,
  height,
  given,
  depth,
  depthOf,
  maxAVL,
  minAVL,
  getBalance,
  equals,
  inorder,
  postorder,
  preorder
) where


-- ÁRBOL AVL:
-- Árbol binario de búsqueda en la que la diferencia entre las alturas de sus dos
-- hijos no pueden diferir en más de uno. Permite la búsqueda, inserción y eliminación
-- en O(logn) como complejidad esperada y como peor caso.

-----------------------
-- CONSTRUCTORES
-----------------------
-- Mantenemos en la estructura un entero que representará la altura del nodo, la cuál
-- ultilizaremos para mantener el balance en el árbol.
data AVL a = Leaf | Node Int a (AVL a) (AVL a)
  deriving (Show, Eq)

-- Dirección tomada en el recorrido del árbol
-- Incluimos la raíz puesto que puede que se necesite rotar el
-- árbol utilizándola como pivote
data Direction = LH | RH | ROOT
    deriving (Eq, Show)

-----------------------
-- FUNCIONES
-----------------------

value :: AVL a -> a
-- Obtiene el valor del nodo raiz del arbol
-- Entradas: arbol
--      arbol (AVL): Arbol de cuyo nodo raiz se quiere sacar el valor
-- Salidas: valor
--      valor (Maybe a): Si el arbol es una hoja, obtiene Nothing, obtiene un Just valor en otro caso
value Leaf = error "Leaves dont have value"
value (Node h x l r) = x

isLeaf :: AVL a -> Bool
-- Comprueba si el arbol es una hoja
-- Entradas: arbol
--      arbol (AVL): Arbol del que se quiere comprobar si es una hoja
-- Salidas: propiedad
--      propiedad (Bool): False en lugar de no ser una hoja, True en otro caso
isLeaf Leaf = True
isLeaf (Node h x l r) = False

height :: AVL a -> Int
-- Obtiene la altura almacenada en el nodo
-- Entradas: arbol del que se quiere obtener la altura
-- Salidas:  Altura como entero almacenado en el nodo
height Leaf = -1
height (Node h x l r) = h

left :: AVL a -> AVL a
left Leaf = error "Leaves dont have child nodes"
left (Node h x l r) = l

right :: AVL a -> AVL a
right Leaf = error "Leaves dont have child nodes"
right (Node h x l r) = r

given :: (Eq a, Ord a) => (a -> Bool) -> AVL a -> [a]
-- Obtiene una lista con los valores de los nodos en el árbol que cumplen el predicado dado.
given _ Leaf = []
given p (Node h x l r) = if p x then x : (given p l ++ given p r) else (given p l ++ given p r)

equals :: (Ord a) => AVL a -> AVL a -> Bool
-- Compara dos árboles comprobando que coincidan sus valores y alturas a lo largo
-- de sus estrutruras, que también han de ser iguales
equals Leaf Leaf = True
equals t1@(Node h1 x l1 r1) t2@(Node h2 y l2 r2) =
  h1==h2 && x==y && equals l1 l2 && equals r1 r2 

depth :: AVL a -> Int
depth Leaf = 0
depth (Node h x l r) = 1 + max (depth l) (depth r)

depthOf :: (Ord a, Eq a) => a -> AVL a -> Int
-- Obtiene la profundidad de un valor en un árbol.
-- Si no está contenido, devuelve -1.
depthOf _ Leaf = -1
depthOf v (Node h x l r)
  | v==x = 0
  | otherwise = if v<x then 1 + (depthOf v l) else 1 + (depthOf v r)

contains :: (Ord a, Eq a) => a -> AVL a -> Bool
contains _ Leaf = False
contains v (Node h x l r) = v==x || contains v l || contains v r

minAVL :: (Eq a, Ord a) => AVL a -> Maybe a
-- Elemento mínimo del árbol
minAVL Leaf = Nothing
minAVL (Node _ n Leaf r) = Just n
minAVL (Node _ _ l _) = minAVL l

maxAVL :: (Eq a, Ord a) => AVL a -> Maybe a
-- Elemento máximo del árbol
maxAVL Leaf = Nothing
maxAVL (Node _ n _ Leaf) = Just n
maxAVL (Node _ _ _ r) = maxAVL r

setHeight :: AVL a -> AVL a
-- Reajusta la altura de un árbol a partir de la altura de sus hijos.
setHeight Leaf = Leaf
setHeight (Node h x l r) = Node (1 + max hl hr) x l r
  where hl = height l
        hr = height r

getBalance :: AVL a -> Int
-- Calcula la diferencia entre las alturas de los hijos de un nodo
-- Lo utilizamos para el rebalanceo del árbol.
getBalance Leaf = 0
getBalance (Node h x l r) = abs(height l - height r)

path :: (Ord a) => a -> AVL a -> [(Direction, AVL a)] -> [(Direction, AVL a)]
-- Obtiene el camino en el árbol hasta el valor dado. Si no existe, su último valor será la hoja donde se insertaría un nuevo nodo 
-- Parámetros: Valor hasta el que queremos obtener el camino
--             Árbol en el que realizamos la búsqueda
--             Acumulador dónde guardamos la dirección que tomamos en cada nodo junto al subárbol en el que nos introducimos
-- Devuelve:   Lista con la con las direcciones tomadas y los subárboles correspondientes, desde la raíz al nodo con el valor.
path a Leaf ps = ps
path a (Node h x l r) ps
    | a < x = path a l ((LH, l):ps)     -- Tomamos la rama izquierda
    | a > x = path a r ((RH, r):ps)     -- Tomamos la rama derecha
    | otherwise = ps                    -- Hemos alcanzado el valor a


-- Casos de desbalanceos que ocurren en el AVL y las rotaciones necesarias para balancearlo
-- (estos casos utilizan 2 tipos de rotaciones que combinamos entre sí):

--  > Left left case: El camino desde el primer nodo desbalanceado hasta el nodo
--      insertado toma desde el desbalanceado la dirección izquierda y en su hijo también.

--               z                                      y 
--              / \                                   /   \
--             y   T4      Right Rotate (z)          x      z
--            / \           ------------>          /  \    /  \ 
--           x   T3                               T1  T2  T3  T4
--          / \
--        T1   T2


--  > Right right case: El camino desde el primer nodo desbalanceado hasta el nodo
--      insertado toma desde el desbalanceado la dirección izquierda y en su hijo también.

--            z                                y
--           /  \                            /   \ 
--          T1   y      Left Rotate(z)      z      x
--              /  \     ------------>     / \    / \
--             T2   x                     T1  T2 T3  T4
--                 / \
--               T3  T4


--  > Left right case: El camino desde el primer nodo desbalanceado hasta el nodo
--      insertado toma desde el desbalanceado la dirección izquierda y en su hijo derecha.

--             z                               z                             x
--            / \                            /   \                          /  \ 
--           y   T4     Left Rotate (y)     x    T4   Right Rotate(z)     y      z
--          / \          ------------>     /  \        ------------>     / \    / \
--        T1   x                          y    T3                      T1  T2 T3  T4
--            / \                        / \
--          T2   T3                    T1   T2

--  > Right left case: El camino desde el primer nodo desbalanceado hasta el nodo
--      insertado toma desde el desbalanceado la dirección derecha y en su hijo izquierda.

--             z                              z                               x
--            / \                            / \                             /  \ 
--          T1   y     Right Rotate(z)     T1   x      Left Rotate(z)      z      y
--              / \     ------------>         /  \      ------------>     / \    / \
--             x   T4                        T2   y                     T1  T2  T3  T4
--            / \                                /  \
--          T2   T3                             T3   T4


-- En ambas funciones de las dos rotaciones rotamos el nodo izquierdo que recibimos 
-- sobre el derecho, solo varía la dirección de la rotación
rotateRight :: (Ord a) => AVL a -> AVL a -> AVL a
rotateRight (Node h1 x a b) (Node h2 p _ c)
  -- Si el nodo a rotar se trata de un nodo sin hijos debemos aumentar su altura en 1
  -- Las alturas del resto de elementos (T1, T2, T3, T4)
  | h1==0 = Node (h1+1) x a (Node ((height c)+1) p b c)
  | otherwise = Node h1 x a (Node ((height c)+1) p b c)

rotateLeft :: (Ord a) => AVL a -> AVL a -> AVL a
rotateLeft (Node h1 x a b) (Node h2 p c _)
  -- Si el nodo a rotar se trata de un nodo sin hijos debemos aumentar su altura en 1
  -- Las alturas del resto de elementos (T1, T2, T3, T4)
  | h1==0 = Node (h1+1) x (Node ((height c)+1) p c a) b
  | otherwise = Node h1 x (Node ((height c)+1) p c a) b

path2tree :: (Ord a) => [(Direction, AVL a)] -> AVL a
-- Reconstruye el árbol correspondiente a partir de una lista de direcciones tomadas en un árbol y los subárboles que le corresponden a cada dirección tomada.
-- Parámetros: Lista con las direcciones tomadas en un árbol y los subárboles que le corresponden a cada decisión.
-- Devuelve:   Árbol reconstruido a partir del parámetro anterior.


-- Si el desbalance se produce en la raíz del árbol debemos realizar una rotación
-- entre el subárbol anterior al árbol completo en el camino hasta el nodo.

-- En las variables merged unificamos el último subárbol antes de la raíz con 
-- el árbol completo teniendo en cuenta la dirección que se toma desde la raíz.
path2tree ((LH, x):(ROOT, p):[])
  | getBalance merged > 1 = rotateRight x p
  | otherwise = merged
  where merged = Node ((height x)+1) (value p) x (right p)
path2tree ((RH, x):(ROOT, p):[])
  | getBalance merged > 1 = rotateLeft x p
  | otherwise = merged
  where merged = Node ((height x)+1) (value p) (left p) x

-- Si llegamos a la primera dirección en el camino durante el descenso en el árbol
-- devolvemos el árbol con sus alturas ajustadas.
path2tree ((_,p):[]) = setHeight p

-- Comprobamos en el recorrido inverso del camino si nos encontramos ante algunos de los 
-- casos de desbalanceo comentados anteriormente. Si no se encuentra desbalanceado devolveremos
-- el árbol comprendido por su padre con su altura ajustada puesto que no se habrá producido
-- ninguna rotación entre el subárbol y el el árbol formado por su padre.

-- Si se ha producido alguna rotación en el subárbol estará comprendida en otro caso que
-- sustituirá al que sería el árbol con el padre como raíz en el camino que quede por recorrer.

--  > Left left case:
path2tree ((LH,x):(LH,p):ps)
  | abs(height x - height (right p)) > 1 = path2tree ((LH, rotateRight x p):ps)
  | otherwise = path2tree ((LH,setHeight p):ps)

--  > Right right case:
path2tree ((RH,x):(RH,p):ps)
  | abs(height x - height (right p)) > 1 = path2tree ((RH, rotateLeft x p):ps)
  | otherwise = path2tree ((RH,setHeight p):ps)

-- Para los dos siguientes casos comprobamos si se produce un cambio de dirección
-- en el camino desde la raíz y si tiene lugar miramos si existe un desbalance 
-- en el nodo abuelo desde el nodo donde se produce el primer cambio de dirección.

-- El árbol doblemente rotado pasa a ser el hijo izquierdo o derecho (según el caso) 
-- de un nuevo nodo que tendrá como valor el del padre del primer nodo desbalanceado
-- y cuya altura será la altura del nodo. La dirección tomada será la misma que la que
-- tomaría en el árbol original en dicho nodo.

--  > Right left case:
path2tree ((RH,x):(LH,p):(z,g):(y, Node h v l r):ps)
  | getBalance g > 1 = let rotated = rotateRight (rotateLeft x p) g in 
                        let rotated' = setHeight rotated in 
                          path2tree ((y, Node (1 + max (height r) (height rotated')) v rotated' r):ps)
  | otherwise = path2tree ((z,setHeight g):ps)

--  > Left right case:
path2tree ((LH,x):(RH,p):(z,g):(y, Node h v l r):ps)
  | getBalance g > 1 = let rotated = rotateLeft (rotateRight x p) g in 
                        let rotated' = setHeight rotated in 
                          path2tree ((y, Node (1 + max (height l) (height rotated')) v l rotated'):ps)
  | otherwise = path2tree ((z,setHeight g):ps)


rotate :: (Ord a) => a -> AVL a -> AVL a
-- Realiza las rotaciones necesarias en el árbol recibido a partir del camino hasta el nodo con valor a en el mismo.
-- Parámetros: Valor del nodo hasta el que llegamos
--             Árbol
-- Devuelve:   Árbol balanceado
rotate a t = path2tree (path a t [(ROOT, t)])

insert :: (Ord a) => a -> AVL a -> AVL a
-- Inserta el nodo y balancea el árbol con el nodo insertado si es necesario
insert v Leaf = Node 0 v Leaf Leaf
insert v t@(Node h x l r) = rotate v inserted
  where inserted = insert' v t

insert' :: (Ord a) => a -> AVL a -> AVL a
insert' v Leaf = Node 0 v Leaf Leaf
insert' v (Node h x l r)
    | v < x && (isLeaf l) = Node (1 + max 0 (height r)) x (Node 0 v Leaf Leaf) r
    | v > x && (isLeaf r) = Node (1 + max 0 (height l)) x l (Node 0 v Leaf Leaf)
    | v < x = Node (h+1) x (insert' v l) r
    | v > x = Node (h+1) x l (insert' v r)
    | otherwise = Node h x l r

-- RECORRIDOS

inorder :: AVL a -> [a]
inorder Leaf = []
inorder (Node h x l r) = (inorder l) ++ [x] ++ (inorder r)

preorder :: AVL a -> [a]
preorder Leaf = []
preorder (Node h x l r) = [x] ++ (preorder l) ++ (preorder r)

postorder :: AVL a -> [a]
postorder Leaf = []
postorder (Node h x l r) = (postorder l) ++ (postorder r) ++ [x]


-- Ejemplos

avl1, avl2, avl3, avl4 :: AVL Int
-- INSERTS (que producen rebalanceo)
-- left left - insert 3
avl1 = Node 3 13 (Node 2 10 (Node 1 5 (Node 0 4 Leaf Leaf) (Node 0 8 Leaf Leaf)) (Node 0 11 Leaf Leaf)) (Node 1 15 Leaf (Node 0 16 Leaf Leaf))

-- right right - insert 45
avl2 = Node 2 30 (Node 0 5 Leaf Leaf) (Node 1 35 (Node 0 32 Leaf Leaf) (Node 0 40 Leaf Leaf))

-- left right - insert 7
avl3 = Node 3 13 (Node 2 10 (Node 1 5 (Node 0 4 Leaf Leaf) (Node 0 6 Leaf Leaf)) (Node 0 11 Leaf Leaf)) (Node 1 15 Leaf (Node 0 16 Leaf Leaf))

-- right left - insert 15
avl4 = Node 3 5 (Node 2 2 (Node 0 1 Leaf Leaf) (Node 1 4 (Node 0 3 Leaf Leaf) Leaf)) (Node 2 7 (Node 0 6 Leaf Leaf) (Node 1 9 Leaf (Node 0 16 Leaf Leaf)))


