module DataStructures.RedBlackTree(
    RBTree(..),
    Color(..),
    depthRBT,
    blackDepth,
    getKeyRBT,
    isBlackRBT,
    makeBlack,
    countBlack,
    allRedHasBlackChilds,
    isBST_RBT,
    isRBT,
    depthRBTOf,
    minRBT,
    maxRBT,
    insertRBT,
    getColor,
    deleteRBT,
    helpRBT
) where

-- Red-Black Tree es una estructura de datos basada en un arbol binario
-- donde cada nodo contiene un bit extra de información que indica el
-- color (rojo o negro), cada Red-Black Tree (RBT) debe cumplir las 
-- siguientes reglas:

-- 1) Todos los nodos deben ser rojos o negros (Las hojas son negras)
-- 2) La raiz es negra
-- 3) Todos los nodos rojos solo tienen hijos negros
-- 4) Todos los caminos simples a cada hoja deben tener el mismo
--    blackDepth (nodos negros en el camino)

-- La ventaja que supone mantener siempre cumplidas estas restricciones
-- supone que a la hora de insertar, eliminar o buscar un elemento del
-- arbol, se consigue con una complejidad de log(n).

-----------------------
-- CONSTRUCTORES
-----------------------

-- Información del color
-- R = Rojo; B = Negro
data Color = R | B deriving (Show, Eq)

-- RBT:
-- Nodo
-- |N Color Valor HijoIzquierdo HijoDerecho
-- Hoja
-- |L
data RBTree a = N Color a (RBTree a) (RBTree a)
              | L
              deriving (Show, Eq)

helpRBT :: IO ()
-- Función que muestra por consola una ayuda sobre RBT
helpRBT = putStrLn "\ndata RBTree a = N Color a (RBTree a) (RBTree a)\n              | L\n              deriving (Show, Eq)\n\ndata Color = R | B deriving (Show, Eq)\n\ndepthRBT :: (Eq a, Ord a) => RBTree a -> Int\n\nblackDepth :: (Eq a, Ord a) => RBTree a -> Int\n\ngetKeyRBT :: (Eq a, Ord a) => RBTree a -> Maybe a\n\nisBlackRBT :: (Eq a, Ord a) => RBTree a -> Bool\n\ngetColor :: (Eq a, Ord a) => RBTree a -> Color\n\nchangeColor :: (Eq a, Ord a) => RBTree a -> RBTree a\n\nmakeBlack :: (Eq a, Ord a) => RBTree a -> RBTree a\n\nmakeRed :: (Eq a, Ord a) => RBTree a -> RBTree a\n\ncountBlack :: (Eq a, Ord a) => RBTree a -> Maybe Int\n\nallRedHasBlackChilds :: (Eq a, Ord a) => RBTree a -> Bool\n\nisBST_RBT :: (Eq a, Ord a) => RBTree a -> Bool\n\nisRBT :: (Eq a, Ord a) => RBTree a -> Bool\n\ndepthRBTOf :: (Eq a, Ord a) => a -> RBTree a -> Int\n\nminRBT :: (Eq a, Ord a) => RBTree a -> Maybe a\n\nmaxRBT :: (Eq a, Ord a) => RBTree a -> Maybe a\n\ninsertRBT :: (Eq a, Ord a) => a -> RBTree a -> RBTree a\n\ndeleteRBT :: (Eq a, Ord a) => a -> RBTree a -> RBTree a"

-- Comprobar depth
depthRBT :: (Eq a, Ord a) => RBTree a -> Int
-- Comprueba la profundidad del arbol
-- Parámetros: Arbol
--    Arbol (RBTree): El RBT del que se busca la profundidad
-- Salidas: profundidad
--    profundidad (Int): la profundidad del arbol
depthRBT L = 0
depthRBT (N _ _ lef rig) = 1 + max (depthRBT lef) (depthRBT rig)

-- Comprobar blackDepth
blackDepth :: (Eq a, Ord a) => RBTree a -> Int
-- Comprueba la suma de nodos negros hasta una hoja (suponiendo que es un RBT)
-- Parámetros: arbol
--    arbol (RBTree): Arbol del que se busca el blackDepth
-- Salidas: blackDepth
--    blackDepth (Int): suma de nodos negros hasta una hoja
blackDepth L = 0
blackDepth (N B _ lef _) = 1 + (blackDepth lef)
blackDepth (N R _ lef _) = (blackDepth lef)

-- Sacar el valor
getKeyRBT :: (Eq a, Ord a) => RBTree a -> Maybe a
-- Obtiene el valor del nodo raiz
-- Parámetros: arbol
--    arbol (RBTree): arbol del que se quiere sacar el valor de su raiz
-- Salidas: valor
--    valor (Maybe a): Valor del nodo raiz
getKeyRBT (L) = Nothing
getKeyRBT (N _ k _ _) = Just k

isBlackRBT :: (Eq a, Ord a) => RBTree a -> Bool
-- Comprueba si el nodo raiz es negro
-- Entradas: arbol
--    arbol (RBTree): Arbol cuyo nodo raiz se comprueba si es negro
-- Salidas: esNegro
--    esNegro (Bool): Si el nodo es negro u hoja, es True, False en otro caso
isBlackRBT (N R _ _ _) = False
isBlackRBT _ = True

getColor :: (Eq a, Ord a) => RBTree a -> Color
-- Obtiene el color del nodo raiz
-- Entrada: arbol
--    arbol (RBTree): Arbol de cuyo nodo raiz se quiere obtener el color
-- Salidas (Color): color
--    color: El color del nodo raiz
getColor (N color _ _ _) = color
getColor (L) = B

changeColor :: (Eq a, Ord a) => RBTree a -> RBTree a
-- Cambia el color del nodo raiz (las hojas son siempre negras)
-- Entradas: arbol
--    arbol (RBTree): El arbol de cuyo nodo raiz se quiere cambiar el color
-- Salidas: arbolSalida
--    arbolSalida (RBTree): Arbol con el nodo raiz cambiado de color
changeColor (L) = (L)
changeColor (N R n l r) = (N B n l r)
changeColor (N B n l r) = (N R n l r)

makeBlack :: (Eq a, Ord a) => RBTree a -> RBTree a
-- Transforma en negro el nodo raiz del arbol
-- Entradas: arbol
--    arbol (RBTree): El arbol de cuyo nodo raiz se quiere cambiar a negro
-- Salidas: arbolSalida
--    arbolSalida (RBTree): Arbol con el nodo raiz cambiado de color a negro
makeBlack (N _ n lef rig) = (N B n lef rig)
makeBlack (L) = (L)

makeRed :: (Eq a, Ord a) => RBTree a -> RBTree a
-- Transforma en rojo el nodo raiz del arbol
-- Entradas: arbol
--    arbol (RBTree): El arbol de cuyo nodo raiz se quiere cambiar a rojo
-- Salidas: arbolSalida
--    arbolSalida (RBTree): Arbol con el nodo raiz cambiado de color a rojo
makeRed (N _ n lef rig) = (N R n lef rig)
makeRed (L) = (L)
--------------------------------------
-- Comprobando si se trata de un RBT--
--------------------------------------

{- Si es un Red-Black Tree se deben  cumplir las siguientes propiedades:
-- Todos los nodos deben ser rojos o negros  (B, R, L se considera B) x
-- La raiz es negra x
-- Todo nodo rojo tiene hijos negros x
-- Todos los caminos simples a cada L deben tener el mismo blackDepth (nodos negros en el camino) x
-- Debe ser binario x
-}


countBlack :: (Eq a, Ord a) => RBTree a -> Maybe Int
-- Comprueba si hay el mismo número de nodos negros en todos los caminos simples hasta las hojas
-- Entradas: arbol
--    arbol (RBTree): Arbol del que se quiere contar el numero de nodos 
-- Salidas: nodosNegros
--    nodosNegros (Maybe Int): En caso de no tener el mismo blackDepth obtiene Nothing, en otro caso
--                             Just numeroDeNodosNegros
countBlack (L) = Just 0
countBlack (N color _ lef rig)
    | countBlack lef == Nothing || countBlack rig == Nothing = Nothing
    | color == R = if lefCount == rigCount then Just (lefCount) else Nothing
    | otherwise = if lefCount == rigCount then Just (1 + lefCount) else Nothing
    where Just lefCount = countBlack lef
          Just rigCount = countBlack rig

allRedHasBlackChilds :: (Eq a, Ord a) => RBTree a -> Bool
-- Comprobar si todos los rojos tienen hijos negros
-- Entradas: arbol
--    arbol (RBTree): El arbol del que se quiere comprobar la propiedad
-- Salidas: propiedad
--    propiedad (Bool): En caso de cumplirse obtiene True, en otro caso False.
allRedHasBlackChilds (L) = True
allRedHasBlackChilds (N B _ lef rig) = allRedHasBlackChilds lef && allRedHasBlackChilds rig
allRedHasBlackChilds (N R _ lef rig) = if isBlackRBT lef && isBlackRBT rig then allRedHasBlackChilds lef && allRedHasBlackChilds rig else False


isBST_RBT :: (Eq a, Ord a) => RBTree a -> Bool
-- Comprobar si cumple las propiedades de un arbol de busqueda binaria
-- Entradas: arbol
--    arbol (RBTree): El arbol del que se quiere comprobar la propiedad
-- Salidas: propiedad
--    propiedad (Bool): En caso de cumplirse obtiene True, en otro caso False.
isBST_RBT (N _ n izq der)
    | getKeyRBT izq == getKeyRBT der && getKeyRBT der == Nothing = True
    | getKeyRBT izq == Nothing = (valD > n) && isBST_RBT der
    | getKeyRBT der == Nothing = (valI < n) && isBST_RBT izq
    | otherwise = (valD > n) && (valI < n) && isBST_RBT der && isBST_RBT izq
    where
        Just valI = getKeyRBT izq
        Just valD = getKeyRBT der
isBST_RBT (L) = True

isRBT :: (Eq a, Ord a) => RBTree a -> Bool
-- Comprueba si cumple las propiedades de un RBT
-- Entradas: arbol
--    arbol (RBTree): El arbol del que se quiere comprobar la propiedad
-- Salidas: propiedad
--    propiedad (Bool): En caso de cumplirse obtiene True, en otro caso False.
isRBT L = True
isRBT (N R _ _ _) = False
isRBT t = allRedHasBlackChilds t && isBST_RBT t && sameBlackPath
    where sameBlackPath = if countBlack t == Nothing then False else True

-----------
--Queries--
-----------

depthRBTOf :: (Eq a, Ord a) => a -> RBTree a -> Int
-- Da la profundidad de un elemento, si no está da -1
-- Entradas: v -> arbol
--    v (a): Elemento buscado en el arbol
--    arbol (RBTree): Arbol en el que se busca
-- Salidas: profundidad
--    profunidad (Int): En caso de no estar en el arbol, -1, en otro caso su profundidad
depthRBTOf _ L = -1
depthRBTOf v (N _ n izq der)
    | n == v = 0
    | otherwise = if v < n then 1 + valueOfDepthLef else 1 + valueOfDepthRig
    where depthLef = (depthRBTOf v izq) 
          depthRig = (depthRBTOf v der)
          valueOfDepthLef = if 0 > depthLef then -2 else depthLef
          valueOfDepthRig = if 0 > depthRig then -2 else depthRig



minRBT :: (Eq a, Ord a) => RBTree a -> Maybe a
-- Elemento minimo del arbol
-- Entradas: arbol
--    arbol (RBTree): Arbol donde se busca
--  Salidas: valor
--    valor (Maybe a): En caso de ser un arbol hoja, es Nothing, en otro caso el valor minimo
minRBT (L) = Nothing
minRBT (N _ n (L) _) = Just n
minRBT (N _ _ lef _) = minRBT lef


maxRBT :: (Eq a, Ord a) => RBTree a -> Maybe a
-- Elemento maximo del arbol
-- Entradas: arbol
--    arbol (RBTree): Arbol donde se busca
--  Salidas: valor
--    valor (Maybe a): En caso de ser un arbol hoja, es Nothing, en otro caso el valor maximo
maxRBT (L) = Nothing
maxRBT (N _ n _ (L)) = Just n
maxRBT (N _ _ _ rig) = maxRBT rig

-----------
--Updates--
-----------

insertRBT :: (Eq a, Ord a) => a -> RBTree a -> RBTree a
-- Insertar elemento en Red Black Tree
-- Entradas: v -> t
--    v (a): Valor que se quiere introducir en el arbol
--    t (RBTree): Arbol donde se quiere insertar el elemento
-- Salidas: arbol
--    arbol (RBTree): Arbol con el elemento insertado
insertRBT v t = makeBlack (ins t)                       -- La raiz es negra
  where ins (L) = N R v (L) (L)                         -- Introduce el nuevo valor si está en hoja
        ins (N color n lef rig)
          | v < n = balanceInsertRBT color n (ins lef) rig
          | v == n = N color n lef rig
          | v > n = balanceInsertRBT color n lef (ins rig)


deleteRBT :: (Eq a, Ord a) => a -> RBTree a -> RBTree a
-- Elimina elemento en Red Black Tree
-- Entradas: v -> t
--    v (a): Valor que se quiere eliminar en el arbol
--    t (RBTree): Arbol del que se quiere eliminar el elemento
-- Salidas: arbol
--    arbol (RBTree): Arbol con el elemento eliminado
deleteRBT v t =
  case del t of -- en cualquier salida de eliminacion, raiz debe ser B
    N _ n a b -> N B n a b
    _ -> L 
  where
    del L = L
    del (N _ n a b)
      | v < n = deleteLeft n a b
      | v > n = deleteRight n a b
      | otherwise = fuse a b
    deleteLeft n a@(N B _ _ _) b = balanceLeft n (del a) b
    deleteLeft n a b = N R n (del a) b
    deleteRight n a b@(N B _ _ _) = balanceRight n a (del b)
    deleteRight n a b = N R n a (del b)
-------------------
--Auxiliar Insert--
-------------------


balanceInsertRBT :: Color -> a -> RBTree a -> RBTree a -> RBTree a
-- Hace las rotaciones del Red Black Tree en una insercion
-- Entradas: color -> valor -> hijoIzquierdo -> hijoDerecho
--    color (Color): Color del nodo
--    valor (a): Valor del nodo
--    hijoIzquierdo (RBTree): Hijo izquierdo
--    hijoDerecho (RBTree): Hijo derecho
-- Salidas: arbol
--    arbol: El arbol rebalanceado
-- Hay 4 posible situaciones de colores que infrigen las reglas a la hora de insertar un valor
{-
1)
        Bz          |           Ry
      /   \         |          /  \
     Ry    d        |        Bx    Bz
   /   \            |       /  \  /  \
  Rx    c           |      a   b  c   d
 /  \               |
a    b              |

2)
        Bz          |           Ry
      /   \         |          /  \
     Ry    d        |        Bx    Bz
   /   \            |       /  \  /  \
  c    Rx           |      a   b  c   d
      /  \          |
     a    b         |

3)
        Bx          |           Ry
       /  \         |          /  \
      a    Rz       |        Bx    Bz
          /  \      |       /  \  /  \
         Ry   b     |      a   b  c   d
        /  \        |
       c    d       |

4)
        Bx          |           Ry
       /  \         |          /  \
      a    Rz       |        Bx    Bz
          /  \      |       /  \  /  \
         b    Ry    |      a   b  c   d
             /  \   |
            c    d  |


-}
--1)
balanceInsertRBT B z (N R y (N R x a b) c) d = N R y (N B x a b) (N B z c d)
--2)
balanceInsertRBT B z (N R x a (N R y b c)) d = N R y (N B x a b) (N B z c d)
--3)
balanceInsertRBT B x a (N R z (N R y b c) d) = N R y (N B x a b) (N B z c d)
--4)
balanceInsertRBT B x a (N R y b (N R z c d)) = N R y (N B x a b) (N B z c d)
-- En otro caso se deja como esta y se subira al siguiente nodo si no se ha terminado
balanceInsertRBT color x a b = N color x a b


-------------------
--Auxiliar Delete--
-------------------
-- Al eliminar un elemento del arbol, puede ser que se elimine de una rama izqueirda o derecha
-- en esos casos se hace un tratamiento simetrico para el rebalanceo del arbol

balanceLeft :: (Eq a, Ord a) => a -> RBTree a -> RBTree a -> RBTree a
-- Rebalanceo cuando se va a eliminar hacia la izquierda
-- Entradas: valor -> hijoIzquierdo -> hijoDerecho
--    valor (a): Valor del nodo raiz del arbol
--    hijoIzquierdo (RBTree): Hijo izquierdo del nodo raiz
--    hijoDerecho (RBTree): Hijo derecho del nodo raiz
--  Salidas: arbol
--    arbol: El arbol rebalanceado
balanceLeft y (N R x a b) c = N R y (N B x a b) c
balanceLeft x bl (N B y a b) = balanceDeleteRBT x bl (N R y a b)
balanceLeft x bl (N R z (N B y a b) c) = N R y (N B x bl a) (balanceDeleteRBT z b (makeRed c))

balanceRight :: (Eq a, Ord a) => a -> RBTree a -> RBTree a -> RBTree a
-- Rebalanceo cuando se va a eliminar hacia la derecha
-- Entradas: valor -> hijoIzquierdo -> hijoDerecho
--    valor (a): Valor del nodo raiz del arbol
--    hijoIzquierdo (RBTree): Hijo izquierdo del nodo raiz
--    hijoDerecho (RBTree): Hijo derecho del nodo raiz
--  Salidas: arbol
--    arbol: El arbol rebalanceado
balanceRight x a (N R y b c) = N R x a (N B y b c)
balanceRight y (N B x a b) bl = balanceDeleteRBT y (N R x a b) bl
balanceRight z (N R x a (N B y b c)) bl = N R y (balanceDeleteRBT x (makeRed a) b) (N B z c bl)


fuse :: (Eq a, Ord a) => RBTree a -> RBTree a -> RBTree a
-- Funcion de fusion de dos ramas de un arbol
-- Entradas: b -> c
--    b (RBTree): Uno de los dos arboles a fusionar
--    c (RBTree): La otra rama a fusionar
--  Salidas: arbol
--    arbol: El arbol resultante de fusionar las dos ramas
-- Casos donde hay una hoja
fuse L x = x
fuse x L = x
-- Casos recursivos
fuse (N R x a b) (N R y c d) =
  case fuse b c of
    N R z b' c' -> N R z (N R x a b') (N R y c' d)
    bc -> N R x a (N R y bc d)
fuse (N B x a b) (N B y c d) = 
  case fuse b c of
    N R z b' c' -> N R z (N B x a b') (N B y c' d)
    bc -> balanceLeft x a (N B y bc d)
-- En otros casos
fuse a (N R x b c) = N R x (fuse a b) c
fuse (N R x a b) c = N R x a (fuse b c)

balanceDeleteRBT :: (Eq a, Ord a) => a -> RBTree a -> RBTree a -> RBTree a
-- En este caso tenemos un caso mas que en el balanceo del insert, si los colores de los hijos
-- son rojos, se cambia el color del padre a rojo y los hijos a negro
-- Hace las rotaciones del Red Black Tree en una eliminacion
-- Entradas: color -> valor -> hijoIzquierdo -> hijoDerecho
--    color (Color): Color del nodo
--    valor (a): Valor del nodo
--    hijoIzquierdo (RBTree): Hijo izquierdo
--    hijoDerecho (RBTree): Hijo derecho
-- Salidas: arbol
--    arbol: El arbol rebalanceado
balanceDeleteRBT y (N R x a b) (N R z c d) = N R y (N B x a b) (N B z c d)
balanceDeleteRBT z (N R y (N R x a b) c) d = N R y (N B x a b) (N B z c d)
balanceDeleteRBT z (N R x a (N R y b c)) d = N R y (N B x a b) (N B z c d)
balanceDeleteRBT x a (N R y b (N R z c d)) = N R y (N B x a b) (N B z c d)
balanceDeleteRBT x a (N R z (N R y b c) d) = N R y (N B x a b) (N B z c d)
balanceDeleteRBT x a b = N B x a b


-- Ejemplos de RBTree

ejbrt1,ejbrt2,ejbrt3,ejbrt4,ejbrt5,ejbrt6,ejbrt7,ejbrt8,ejbrt9,ejbrt10,ejbrt11,ejbrt12 :: RBTree Int
ejbrt1 = (N B 7 (N B 3 (L) (L)) (N R 18 (N B 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (L) (L)))))
ejbrt2 = foldr insertRBT (L) [6,3,18,1,4,14,22,9,16,20,26,8,10,15,17]
ejbrt3 = foldr insertRBT (L) [7,3,18,10,8,11,22,26]
ejbrt4 = foldr insertRBT (L) [1,2,4,5,3]
ejbrt5 = foldr insertRBT (L) [8,9,10,13,11]
ejbrt6 = (N B 6 ejbrt8 ejbrt9)

-- Ejemplos de RBT no valido
-- No tiene todos los caminos hasta las hojas con misma cantidad de negros
ejbrt7 = (N B 7 (N B 3 (L) (L)) (N R 18 (N B 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (N B 25 (L) (L)) (L)))))
-- La raiz es roja
ejbrt8 = (N R 7 (N B 3 (L) (L)) (N R 18 (N B 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (N B 25 (L) (L)) (L)))))
-- Un rojo tiene un hijo rojo
ejbrt9 = (N B 7 (N B 3 (L) (L)) (N R 18 (N R 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (N B 25 (L) (L)) (L)))))
-- No es BST
ejbrt10 = (N B 7 (N B 42 (L) (L)) (N R 18 (N R 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (N B 25 (L) (L)) (L)))))
--fuse (N R 2 (N B 1 (L) (L)) (N B 3 (L) (L))) (N B 5 (L) (L))
ejbrt11 = (N B 4 (N B 1 L (N R 3 L L)) (N B 8 (N R 6 L L) L))
ejbrt12 = (N B 15 (N B 10 L (N R 14 L L)) (N R 20 (N B 17 (N R 16 L L) (N R 18 L L)) (N B 26 (N R 22 L L) L)))