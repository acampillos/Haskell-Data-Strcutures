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
    balanceRBT,
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
          | v < n = balanceRBT color n (ins lef) rig
          | v == n = N color n lef rig
          | v > n = balanceRBT color n lef (ins rig)

------------
--Auxiliar--
------------
data Side = LEFT | RIGHT

-- Rotacion de un arbol dado un elemento, un lado al que rotar y el arbol.
rotation :: (Eq a, Ord a) => a -> Side -> RBTree a -> RBTree a
rotation v LEFT t = undefined
rotation v RIGHT t = undefined



-- Hace las rotaciones del Red Black Tree
balanceRBT :: Color -> a -> RBTree a -> RBTree a -> RBTree a
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
balanceRBT B z (N R y (N R x a b) c) d = N R y (N B x a b) (N B z c d)
--2)
balanceRBT B z (N R x a (N R y b c)) d = N R y (N B x a b) (N B z c d)
--3)
balanceRBT B x a (N R z (N R y b c) d) = N R y (N B x a b) (N B z c d)
--4)
balanceRBT B x a (N R y b (N R z c d)) = N R y (N B x a b) (N B z c d)
-- En otro caso se deja como esta y se subira al siguiente nodo si no se ha terminado
balanceRBT color x a b = N color x a b

-- Da el nodo padre de un nodo
{-parent :: (Eq a, Ord a) => RBTree -> RBTree a -> -}





{-
Casos posibles al eliminar un valor:

1) El facil:
Eliminar un elemento que tiene dos hijos hoja y ademas es rojo

2) El segundo mas facil:
Eliminar un elemento rojo que solo tiene un hijo hoja y su otro hijo es negro,
se sustituye por su hijo hoja.

-}
-- Hay que comprobar antes si el nodo raiz es el que se quiere eliminar
deleteRBT :: (Eq a, Ord a) => a -> RBTree a -> RBTree a
deleteRBT x t@(N B y lef rig) = if x == y then makeBlack (replacement t) else makeBlack (delRBT x t)
--deleteRBT x t = makeBlack (delRBT x t)          -- El nodo raiz debe ser negro

delRootRBT :: (Eq a, Ord a) => a -> RBTree a -> RBTree a
delRootRBT _ (L) = (L)
delRootRBT x t@(N color y lef rig)
  | x == y = replacementRoot t
  | x < y = (N color y (delRBT x t lef) rig)
  | x > y = (N color y lef (delRBT x t rig))

replacementRoot :: (Eq a, Ord a) => RBTree a -> RBTree a
replacementRoot (N col _ (L) (L)) = makeBlack L
replacementRoot (N col _ t1@(N _ _ _ _) (L)) = makeBlack t1
replacementRoot (N col _ (L) t2@(N _ _ _ _)) = makeBlack t2
-- if the node deleted has 2 non nil children, set x to the replacement's 
-- right child before the replacement is spliced out
replacementRoot (N col x t1 t2@(N col2 y _ _)) = makeBlack (N col2 y t1 (replacement t2))


-- Se pasa el elemento a eliminar y el PADRE del nodo que se está estudiando
--                        elem   padre       
delRBT :: (Eq a, Ord a) => a -> RBTree a -> RBTree a
delRBT x t@(N _ _ (L) (L)) = t

delRBT x t@(N col p t1@(N col1 y lef1 rig1) t2@(N col2 z lef2 rig2))
  | x == y = (N col p (replacement x t) t2)
  | x == z = (N col p t1 (replacement x t))
  | x < p = (N col p (delRBT x t1) t2)
  | x > p = (N col p t1 (delRBT x t2))

delRBT x t@(N col p t1@(L) t2@(N col2 z lef2 rig2))
  | x == z = (N col p t1 (replacement x t))
  | otherwise = (N col p t1 (delRBT x t2))

delRBT x t@(N col p t1@(N col1 y lef1 rig1) t2@(L))
  | x == y = (N col p (replacement x t) t2)
  | otherwise = (N col p (delRBT x t1) t2)

-- Se le pasa el elemento a eliminar y el nodo PADRE del nodo a eliminar
replacement :: (Eq a, Ord a) => a -> RBTree a -> RBTree a
-- Si el elemento a eliminar tiene dos hijos hoja
replacement x t@(N _ p t1 t2)
  | x < p = rep t1
  | x > p = rep t2

rep (N _ _ (L) (L)) = (L)
rep (N _ _ t1@(N _ _ _ _) (L)) = t1
rep (N _ _ (L) t2@(N _ _ _ _)) = t2
--rep (N _ _ t1@(N _ y _ _) t2@(N _ z _ _)) = 

{-
replacement x (N col _ t1@(N _ _ _ _) (L)) = recoloring col t1
replacement x (N col _ (L) t2@(N _ _ _ _)) = recoloring col t2
-- if the node deleted has 2 non nil children, set x to the replacement's 
-- right child before the replacement is spliced out
replacement x (N col p t1 t2@(N col2 y _ _)) = recoloring col (N col2 y t1 (replacement t2))
-}

-- Se le pasa el color que tenia antes y el nodo padre del reemplazo y se devuelve el 
-- nodo con el color que le corresponde



{-recoloring :: (Eq a, Ord a) => Color -> RBTree a -> RBTree a
recoloring R t@(N R _ _ _) = t
recoloring R (L) = (L)
recoloring R t@(N B _ _ _) = recoloringCases (makeRed t)
recoloring B t@(N R _ _ _) = makeBlack t
recoloring B t@(N B _ _ _) = recoloringCases t
recoloring B t@(L) = recoloringCases t-}

--recoloringCases :: 

{-
balanceRBT' :: (Eq a, Ord a) => RBTree a -> RBTree a
--1)
balanceRBT' (N B z (N R y (N R x a b) c) d) = N R y (N B x a b) (N B z c d)
--2)
balanceRBT' (N B z (N R x a (N R y b c)) d) = N R y (N B x a b) (N B z c d)
--3)
balanceRBT' (N B x a (N R z (N R y b c) d)) = N R y (N B x a b) (N B z c d)
--4)
balanceRBT' (N B x a (N R y b (N R z c d))) = N R y (N B x a b) (N B z c d)

balanceRBT' t = t




deleteRBT :: (Eq a, Ord a) => a -> RBTree a -> RBTree a
deleteRBT x t = makeBlack (delRBT x t)          -- El nodo raiz debe ser negro

delRBT :: (Eq a, Ord a) => a -> RBTree a -> RBTree a
-- Algunos casos base:
{-
        By        |x==z     By       |x==y    Bz 
      /    \      |       /    \     |      /    \
     t1    Rz     |      t1    (L)   |     t1    (L)
            |     |
           (L)    | 
-}
delRBT x (N B y (L) (N R z (L) (L)))
  | x == z = (N R y (L) (L))
  | x == y = (N R z (L) (L))
  | otherwise = (N B y (L) (N R z (L) (L)))
{-
        By        |x==z     By       |x==y    Bz 
      /    \      |       /    \     |      /    \
     Rz    t1     |      (L)    t1   |     (L)    t1
      |           |
     (L)     
-}
delRBT x (N B y (N R z (L) (L)) (L))
  | x == z = (N R y (L) (L))
  | x == y = (N R z (L) (L))
  | otherwise = (N B y (N R z (L) (L)) (L))
{-
      a

-}
{-delRBT x (N B y (N B u t1 t2) (N R v t3 t4))
  | x == y = (N B (minRBT (N R v t3 t4)) ())-}
-- Otros casos no base
delRBT x t@(N _ y l r)
  | x < y = delL x t
  | x > y = delR x t
  | otherwise = fuse l r

{-delRBT :: (Eq a, Ord a) => a -> RBTree a -> RBTree a
delRBT x t@(N R y (L) (L)) = if x==y then (L) else t
delRBT x t@(N color y l r)
  | x < y = balL (N color y (delRBT x l) r)
  | x > y = balR (N color y l (delRBT x r))
  | x == y = fuse l r                   -- Si se encuentra el valor a eliminar, se fusionan las dos ramas
  | otherwise = t
delRBT x (L) = (L)-}

{-delL :: (Eq a, Ord a) => a -> RBTree a -> RBTree a
delL x t@(N B y t1 t2) = balL (N B y (delRBT x t1) t2)
delL x t@(N R y t1 t2) = N R y (delRBT x t1) t2
delL x (L) = (L)-}

{-
delL :: (Eq a, Ord a) => a -> RBTree a -> RBTree a
delL x t@(N B y t1 t2) = if R == newColor then (N R y (changeColor deleted) t2) else balL (N B y deleted t2)
  where 
    deleted = (delRBT x t1)
    newColor = getColor deleted
    deletedColor = getColor t1
delL x t@(N R y t1 t2) = if deletedColor == B && newColor == R then (N B y (changeColor deleted) t2) else balL (N R y deleted t2)
  where 
    deleted = (delRBT x t1)
    newColor = getColor deleted
    deletedColor = getColor t1
delL x (L) = (L)
-}

delL :: (Eq a, Ord a) => a -> RBTree a -> RBTree a
delL x t@(N B y t1 t2) = if deletedColor == R && R == newColor then (N R y deleted t2) else balL (N B y deleted t2)
  where 
    deleted = (delRBT x t1)
    newColor = getColor deleted
    deletedColor = getColor t1
delL x t@(N R y t1 t2) = if deletedColor == B && newColor == R then (N B y (changeColor deleted) t2) else balL (N R y deleted t2)
  where 
    deleted = (delRBT x t1)
    newColor = getColor deleted
    deletedColor = getColor t1
delL x (L) = (L)

-- Si se ha borrado, es el de la izquierda
balL :: (Eq a, Ord a) => RBTree a -> RBTree a
balL (N B y (N R x t1 t2) t3) = N R y (N B x t1 t2) t3
--balL (N B y t1 (N B z t2 t3)) = balanceRBT' (N B y t1 (N R z t2 t3))
balL (N B y t1 (N R z (N B u t2 t3) t4@(N B value l r))) =
  N R u (N B y t1 t2) (balanceRBT' (N B z t3 (N R value l r)))
balL t = t  -- MOD para que no pete

{-
delR :: (Eq a, Ord a) => a -> RBTree a -> RBTree a
delR x t@(N B y t1 t2) = if deletedColor == R && R == newColor then (N B y t1 deleted) else balR (N B y t1 deleted)
  where 
    deleted = (delRBT x t2)
    newColor = getColor deleted
    deletedColor = getColor t2
delR x t@(N R y t1 t2) = if deletedColor == B && newColor == R then (N R y t1 (changeColor deleted)) else balR (N R y t1 deleted)
  where 
    deleted = (delRBT x t2)
    newColor = getColor deleted
    deletedColor = getColor t2
delR x (L) = (L)
-}

delR :: (Eq a, Ord a) => a -> RBTree a -> RBTree a
delR x t@(N B y t1 t2) = if deletedColor == R && R == newColor then (N B y t1 deleted) else balR (N B y t1 deleted)
  where 
    deleted = (delRBT x t2)
    newColor = getColor deleted
    deletedColor = getColor t2
delR x t@(N R y t1 t2) = if deletedColor == B && newColor == R then (N R y t1 (changeColor deleted)) else balR (N R y t1 deleted)
  where 
    deleted = (delRBT x t2)
    newColor = getColor deleted
    deletedColor = getColor t2
delR x (L) = (L)

balR :: (Eq a, Ord a) => RBTree a -> RBTree a
balR (N B y t1 (N R x t2 t3)) = N R y t1 (N B x t2 t3)
--balR (N B y (N B z t1 t2) t3) = balanceRBT' (N B y (N R z t1 t2) t3)
balR (N B y (N R z t1@(N B value l r) (N B u t2 t3)) t4) =
  N R u (balanceRBT' (N B z (N R value l r) t2)) (N B y t3 t4)
balR t = t  -- MOD

fuse :: (Eq a, Ord a) => RBTree a -> RBTree a -> RBTree a
fuse L t = t                -- Fusionar una rama con una hoja es la rama
fuse t L = t
fuse t1@(N B _ _ _) (N R y t3 t4) = N R y (fuse t1 t3) t4
--fuse (N B x t1 t2) (N R y t3 t4) = N R y (N B x t1 (fuse t2 t3)) t4  -- NO HACE BIEN EL FUSE CON COLORES DISTINTOS
fuse (N R x t1 t2) t3@(N B _ _ _) = N R x t1 (fuse t2 t3)
--fuse (N R x t1 t2) (N B y t3 t4) = N R x t1 (N B y (fuse t2 t3) t4)
-- Cuando hay fusion entre dos nodos que son del mismo color hay problema de muchos casos base:
-- Hay 4 tipos de casos base:
-- Con 0 hijos entre los dos ---------------------------------------------------------
{-
        _x         _y       |       By
      /   \       /   \     |     /   \
    (L)   (L)   (L)   (L)   |    Rx   (L) 
                            |  /   \
                            |(L)  (L)
 -}
fuse (N _ x (L) (L)) (N _ y (L) (L)) = (N B y (N R x (L) (L)) (L))
-- Con 1 hijo entre los dos -------------------------------------------------------------
{-
        _x          _y          |           Ry
      /   \       /   \         |         /   \
    (L)   (L)   (L)   _z        |       Bx     Bz
                    /   \       |     /   \   /   \
                  (L)  (L)      |   (L)   (L)(L)  (L)
-}
fuse (N _ x (L) (L)) (N _ y (L) (N _ z (L) (L))) = (N R y (N B x (L) (L)) (N B z (L) (L)))
{-
        _x          _y          |           Rz
      /   \       /   \         |         /   \
    (L)   (L)   _z    (L)       |       Bx     By
               /   \            |     /   \   /   \
              (L)  (L)          |   (L)   (L)(L)  (L)
-}
fuse (N _ x (L) (L)) (N _ y (N _ z (L) (L)) (L)) = (N R z (N B x (L) (L)) (N B y (L) (L)))
{-
        _x          _y          |           Rz
      /   \       /   \         |         /   \
    (L)   _z    (L)   (L)       |       Bx     By
         /   \                  |     /   \   /   \
        (L)  (L)                |   (L)   (L)(L)  (L)
-}
fuse (N _ x (L) (N _ z (L) (L))) (N _ y (L) (L)) = (N R z (N B x (L) (L)) (N B y (L) (L)))
{-
        _x          _y          |           Rx
      /   \       /   \         |         /   \
     _z    (L)   (L)  (L)       |       Bz     By
    /   \                       |     /   \   /   \
   (L)  (L)                     |   (L)   (L)(L)  (L)
-}
fuse (N _ x (N _ z (L) (L)) (L)) (N _ y (L) (L)) = (N R x (N B z (L) (L)) (N B y (L) (L)))
-- Con 2 hijos entre los dos ----------------------------------------------------------------
{-
        _x          _y          |          Ry        
      /    \      /   \         |        /   \
     _u    (L)   (L)   _v       |       Bx   Bv
      |                 |       |     /   \   
     (L)               (L)      |   Ru    (L)
-}
fuse (N _ x (N _ u (L) (L)) (L)) (N _ y (L) (N _ v (L) (L))) = (N R y (N B x (N R u (L) (L)) (L)) (N B v (L) (L)))
{-
        _x          _y          |          Rv        
      /    \      /   \         |        /   \
     _u    (L)   _v   (L)       |       Bx   By
      |           |             |     /   \   
     (L)         (L)            |   Ru    (L)
-}
fuse (N _ x (N _ u (L) (L)) (L)) (N _ y (N _ v (L) (L)) (L)) = (N R v (N B x (N R u (L) (L)) (L)) (N B y (L) (L)))
{-
        _x          _y          |          Rv        
      /    \      /   \         |        /   \
     (L)   _u    _v   (L)       |       Bu   By
            |     |             |     /   \   
           (L)   (L)            |   Rx    (L)
-}
fuse (N _ x (L) (N _ u (L) (L))) (N _ y (N _ v (L) (L)) (L)) = (N R v (N B u (N R x (L) (L)) (L)) (N B y (L) (L)))
{-
        _x          _y          |          Ry        
      /    \      /   \         |        /   \
     (L)   _u    (L)   _v       |       Bx   Bv
            |          |        |     /   \   
           (L)        (L)       |    (L)  Ru
-}
fuse (N _ x (L) (N _ u (L) (L))) (N _ y (L) (N _ v (L) (L))) = (N R y (N B x (L) (N R u (L) (L))) (N B v (L) (L)))
-- 3 hijos entre los dos -------------------------------------------------------------------------------------------
{-
        _x          _y          |          Rv        
      /    \      /   \         |        /   \
     (L)   _u    _v   _w        |       Bu    By
            |     |    |        |     /   \     \
           (L)   (L)  (L)       |   Rx    (L)   Rw
-}
fuse (N _ x (L) (N _ u (L) (L))) (N _ y (N _ v (L) (L)) (N _ w (L) (L))) = (N R v (N B u (N R x (L) (L)) (L)) (N B y (L) (N R w (L) (L))))
{-
        _x          _y          |          Rv        
      /    \      /   \         |        /   \
     _u    _v   _w    (L)       |       Bx    Bw
      |     |    |              |     /   \     \
     (L)   (L)  (L)             |   Ru    (L)   Ry       
-}
fuse (N _ x (N _ u (L) (L)) (N _ v (L) (L))) (N _ y (N _ w (L) (L)) (L)) = (N R v (N B x (N R u (L) (L)) (L)) (N B w (L) (N R y (L) (L))))
{-
        _x          _y          |          Rv        
      /    \      /   \         |        /   \
     _u    _v   (L)   _w        |       Bx    By
      |     |          |        |     /   \     \
     (L)   (L)        (L)       |   Ru    (L)   Rw       
-}
fuse (N _ x (N _ u (L) (L)) (N _ v (L) (L))) (N _ y (L) (N _ w (L) (L))) = (N R v (N B x (N R u (L) (L)) (L)) (N B y (L) (N R w (L) (L))))
{-
        _x          _y          |          Rv        
      /    \      /   \         |        /   \
     _u    (L)   _v   _w        |       Bx    By
      |           |    |        |     /   \     \
     (L)         (L)  (L)       |   Ru    (L)   Rw       
-}
fuse (N _ x (N _ u (L) (L)) (L)) (N _ y (N _ v (L) (L)) (N _ w (L) (L))) = (N R v (N B x (N R u (L) (L)) (L)) (N B y (L) (N R w (L) (L))))

fuse (N R x t1 t2) (N R y t3 t4)  =
  let s = fuse t2 t3
  in case s of
       (N R z s1 s2) -> (N R z (N R x t1 s1) (N R y s2 t4))
       (N B _ _ _)   -> (N R x t1 (N R y s t4))
fuse (N B x t1 t2) (N B y t3 t4)  =
  let s = fuse t2 t3
  in case s of
       (N R z s1 s2) -> (N R z (N B x t1 s1) (N B y s2 t4))
       (N B z s1 s2) -> balL (N B x t1 (N B y s t4))
-}
