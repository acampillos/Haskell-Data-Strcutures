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
    balanceRBT
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

makeBlack :: (Eq a, Ord a) => RBTree a -> RBTree a
makeBlack (N _ n lef rig) = (N B n lef rig)
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

------------
--Auxiliar--
------------
data Side = LEFT | RIGHT

-- Rotacion de un arbol dado un elemento, un lado al que rotar y el arbol.
rotation :: (Eq a, Ord a) => a -> Side -> RBTree a -> RBTree a
rotation v LEFT t = undefined
rotation v RIGHT t = undefined

-- Insertar elemento en Red Black Tree
insertRBT :: (Eq a, Ord a) => a -> RBTree a -> RBTree a
insertRBT x s = makeBlack $ ins s
  where ins (L) = N R x (L) (L)
        ins (N color y a b)
          | x < y  = balanceRBT color y (ins a) b
          | x == y = N color y a b
          | x > y  = balanceRBT color y a (ins b)

-- Hace las rotaciones del Red Black Tree
balanceRBT :: Color -> a -> RBTree a -> RBTree a -> RBTree a
balanceRBT B z (N R y (N R x a b) c) d = N R y (N B x a b) (N B z c d)
balanceRBT B z (N R x a (N R y b c)) d = N R y (N B x a b) (N B z c d)
balanceRBT B x a (N R z (N R y b c) d) = N R y (N B x a b) (N B z c d)
balanceRBT B x a (N R y b (N R z c d)) = N R y (N B x a b) (N B z c d)
balanceRBT color x a b = N color x a b

-- Da el nodo padre de un nodo
{-parent :: (Eq a, Ord a) => RBTree -> RBTree a -> -}
