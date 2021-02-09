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
    --fuse,
    --balR,
    --balL
) where
data Color = R | B deriving (Show, Eq)
data RBTree a = N Color a (RBTree a) (RBTree a)
              | L
              deriving (Show, Eq)


-- Comprobar depth
depthRBT :: (Eq a, Ord a) => RBTree a -> Int
depthRBT L = 0
depthRBT (N _ _ lef rig) = 1 + max (depthRBT lef) (depthRBT rig)

-- Comprobar blackDepth
blackDepth :: (Eq a, Ord a) => RBTree a -> Int
blackDepth L = 0
blackDepth (N B _ lef _) = 1 + (blackDepth lef)
blackDepth (N R _ lef _) = (blackDepth lef)

-- Sacar el valor
getKeyRBT :: (Eq a, Ord a) => RBTree a -> Maybe a
getKeyRBT (L) = Nothing
getKeyRBT (N _ k _ _) = Just k

-- Es negro?
isBlackRBT :: (Eq a, Ord a) => RBTree a -> Bool
isBlackRBT (N R _ _ _) = False
isBlackRBT _ = True

getColor :: (Eq a, Ord a) => RBTree a -> Color
getColor (N color _ _ _) = color
getColor (L) = B

changeColor :: (Eq a, Ord a) => RBTree a -> RBTree a
changeColor (L) = (L)
changeColor (N R n l r) = (N B n l r)
changeColor (N B n l r) = (N R n l r)

makeBlack :: (Eq a, Ord a) => RBTree a -> RBTree a
makeBlack (N _ n lef rig) = (N B n lef rig)
makeBlack (L) = (L)

makeRed :: (Eq a, Ord a) => RBTree a -> RBTree a
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

-- Comprueba si hay el mismo número de nodos negros en todos los caminos simples hasta las hojas
countBlack :: (Eq a, Ord a) => RBTree a -> Maybe Int
countBlack (L) = Just 0
countBlack (N color _ lef rig)
    | countBlack lef == Nothing || countBlack rig == Nothing = Nothing
    | color == R = if lefCount == rigCount then Just (lefCount) else Nothing
    | otherwise = if lefCount == rigCount then Just (1 + lefCount) else Nothing
    where Just lefCount = countBlack lef
          Just rigCount = countBlack rig

-- Comprobar si todos los rojos tienen hijos negros
allRedHasBlackChilds :: (Eq a, Ord a) => RBTree a -> Bool
allRedHasBlackChilds (L) = True
allRedHasBlackChilds (N B _ lef rig) = allRedHasBlackChilds lef && allRedHasBlackChilds rig
allRedHasBlackChilds (N R _ lef rig) = if isBlackRBT lef && isBlackRBT rig then allRedHasBlackChilds lef && allRedHasBlackChilds rig else False

-- Comprobar si es Binario
isBST_RBT :: (Eq a, Ord a) => RBTree a -> Bool
isBST_RBT (N _ n izq der)
    | getKeyRBT izq == getKeyRBT der && getKeyRBT der == Nothing = True
    | getKeyRBT izq == Nothing = (valD > n) && isBST_RBT der
    | getKeyRBT der == Nothing = (valI < n) && isBST_RBT izq
    | otherwise = (valD > n) && (valI < n) && isBST_RBT der && isBST_RBT izq
    where
        Just valI = getKeyRBT izq
        Just valD = getKeyRBT der
isBST_RBT (L) = True

-- Comprueba si cumple las propiedades de un RBT
isRBT :: (Eq a, Ord a) => RBTree a -> Bool
isRBT L = True
isRBT (N R _ _ _) = False
isRBT t = allRedHasBlackChilds t && isBST_RBT t && sameBlackPath
    where sameBlackPath = if countBlack t == Nothing then False else True

-----------
--Queries--
-----------
-- Da la profundidad de un elemento, si no está da -1
depthRBTOf :: (Eq a, Ord a) => a -> RBTree a -> Int
depthRBTOf _ L = -1
depthRBTOf v (N _ n izq der)
    | n == v = 0
    | otherwise = if v < n then 1 + valueOfDepthLef else 1 + valueOfDepthRig
    where depthLef = (depthRBTOf v izq) 
          depthRig = (depthRBTOf v der)
          valueOfDepthLef = if 0 > depthLef then -2 else depthLef
          valueOfDepthRig = if 0 > depthRig then -2 else depthRig


-- Elemento minimo del arbol
minRBT :: (Eq a, Ord a) => RBTree a -> Maybe a
minRBT (L) = Nothing
minRBT (N _ n (L) _) = Just n
minRBT (N _ _ lef _) = minRBT lef

-- Elemento maximo del arbol
maxRBT :: (Eq a, Ord a) => RBTree a -> Maybe a
maxRBT (L) = Nothing
maxRBT (N _ n _ (L)) = Just n
maxRBT (N _ _ _ rig) = maxRBT rig

-----------
--Updates--
-----------
-- Insertar elemento en Red Black Tree
insertRBT :: (Eq a, Ord a) => a -> RBTree a -> RBTree a
insertRBT v t = makeBlack (ins t)                       -- La raiz es negra
  where ins (L) = N R v (L) (L)                         -- Introduce el nuevo valor si está en hoja
        ins (N color n lef rig)
          | v < n = balanceInsertRBT color n (ins lef) rig
          | v == n = N color n lef rig
          | v > n = balanceInsertRBT color n lef (ins rig)

-- Al eliminar un elemento del arbol, puede ser que se elimine de una rama izqueirda o derecha
-- en esos casos se hace un tratamiento simetrico para el rebalanceo del arbol
deleteRBT :: (Eq a, Ord a) => a -> RBTree a -> RBTree a
deleteRBT x t =
  case del t of {N _ y a b -> N B y a b; _ -> L} -- en cualquier salida de eliminacion, raiz debe ser B
  where
    del L = L
    del (N _ y a b)
      | x < y = deleteLeft y a b
      | x > y = deleteRight y a b
      | otherwise = fuse a b
    deleteLeft y a@(N B _ _ _) b = balanceLeft y (del a) b
    deleteLeft y a b = N R y (del a) b
    deleteRight y a b@(N B _ _ _) = balanceRight y a (del b)
    deleteRight y a b = N R y a (del b)
-------------------
--Auxiliar Insert--
-------------------

-- Hace las rotaciones del Red Black Tree
balanceInsertRBT :: Color -> a -> RBTree a -> RBTree a -> RBTree a
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
-- Tratar el rebalanceo 
balanceLeft :: (Eq a, Ord a) => a -> RBTree a -> RBTree a -> RBTree a
balanceLeft y (N R x a b) c = N R y (N B x a b) c
balanceLeft x bl (N B y a b) = balanceDeleteRBT x bl (N R y a b)
balanceLeft x bl (N R z (N B y a b) c) = N R y (N B x bl a) (balanceDeleteRBT z b (makeRed c))

balanceRight :: (Eq a, Ord a) => a -> RBTree a -> RBTree a -> RBTree a
balanceRight x a (N R y b c) = N R x a (N B y b c)
balanceRight y (N B x a b) bl = balanceDeleteRBT y (N R x a b) bl
balanceRight z (N R x a (N B y b c)) bl = N R y (balanceDeleteRBT x (makeRed a) b) (N B z c bl)

-- Funcion de fusion de dos ramas de un arbol
fuse :: (Eq a, Ord a) => RBTree a -> RBTree a -> RBTree a
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
-- Casos base
fuse a (N R x b c) = N R x (fuse a b) c
fuse (N R x a b) c = N R x a (fuse b c)

-- En este caso tenemos un caso mas que en el balanceo del insert, si los colores de los hijos
-- son rojos, se cambia el color del padre a rojo y los hijos a negro
balanceDeleteRBT :: (Eq a, Ord a) => a -> RBTree a -> RBTree a -> RBTree a
balanceDeleteRBT y (N R x a b) (N R z c d) = N R y (N B x a b) (N B z c d)
balanceDeleteRBT z (N R y (N R x a b) c) d = N R y (N B x a b) (N B z c d)
balanceDeleteRBT z (N R x a (N R y b c)) d = N R y (N B x a b) (N B z c d)
balanceDeleteRBT x a (N R y b (N R z c d)) = N R y (N B x a b) (N B z c d)
balanceDeleteRBT x a (N R z (N R y b c) d) = N R y (N B x a b) (N B z c d)
balanceDeleteRBT x a b = N B x a b