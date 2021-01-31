module DataStructures.RedBlackTree(
    RBTree(..),
    depthRBT,
    blackDepth,
    getKeyRBT,
    isBlackRBT,
    countBlack,
    allRedHasBlackChilds,
    isBST_RBT,
    isRBT,
    depthRBTOf,
    minRBT,
    maxRBT
) where

data RBTree a = B a (RBTree a) (RBTree a)
             | R a (RBTree a) (RBTree a)
             | L
             deriving (Show, Eq)


-- Comprobar depth
depthRBT :: (Eq a, Ord a) => RBTree a -> Int
depthRBT L = 0
depthRBT (B _ lef rig) = 1 + max (depthRBT lef) (depthRBT rig)
depthRBT (R _ lef rig) = 1 + max (depthRBT lef) (depthRBT rig)

-- Comprobar blackDepth
blackDepth :: (Eq a, Ord a) => RBTree a -> Int
blackDepth L = 0
blackDepth (B _ lef _) = 1 + (depthRBT lef)
blackDepth (R _ lef _) = (depthRBT lef)

-- Sacar el valor
getKeyRBT :: (Eq a, Ord a) => RBTree a -> Maybe a
getKeyRBT (L) = Nothing
getKeyRBT (B k _ _) = Just k
getKeyRBT (R k _ _) = Just k

-- Es negro?
isBlackRBT :: (Eq a, Ord a) => RBTree a -> Bool
isBlackRBT (L) = True
isBlackRBT (B _ _ _) = True
isBlackRBT (R _ _ _) = False

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
countBlack (B _ lef rig)
    | countBlack lef == Nothing || countBlack rig == Nothing = Nothing
    | otherwise = if lefCount == rigCount then Just (1 + lefCount) else Nothing
    where Just lefCount = countBlack lef
          Just rigCount = countBlack rig
countBlack (R _ lef rig)
    | countBlack lef == Nothing || countBlack rig == Nothing = Nothing
    | otherwise = if lefCount == rigCount then Just (lefCount) else Nothing
    where Just lefCount = countBlack lef
          Just rigCount = countBlack rig

-- Comprobar si todos los rojos tienen hijos negros
allRedHasBlackChilds :: (Eq a, Ord a) => RBTree a -> Bool
allRedHasBlackChilds (L) = True
allRedHasBlackChilds (B _ lef rig) = allRedHasBlackChilds lef && allRedHasBlackChilds rig
allRedHasBlackChilds (R _ lef rig) = if isBlackRBT lef && isBlackRBT rig then allRedHasBlackChilds lef && allRedHasBlackChilds rig else False

-- Comprobar si es Binario
isBST_RBT :: (Eq a, Ord a) => RBTree a -> Bool
isBST_RBT (B n izq der)
    | getKeyRBT izq == getKeyRBT der && getKeyRBT der == Nothing = True
    | getKeyRBT izq == Nothing = (valD > n) && isBST_RBT der
    | getKeyRBT der == Nothing = (valI < n) && isBST_RBT izq
    | otherwise = (valD > n) && (valI < n) && isBST_RBT der && isBST_RBT izq
    where
        Just valI = getKeyRBT izq
        Just valD = getKeyRBT der
isBST_RBT (R n izq der)
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
isRBT (R _ _ _) = False
isRBT t = allRedHasBlackChilds t && isBST_RBT t && sameBlackPath
    where sameBlackPath = if countBlack t == Nothing then False else True

-----------
--Queries--
-----------
-- Da la profundidad de un elemento, si no está da -1
depthRBTOf :: (Eq a, Ord a) => a -> RBTree a -> Int
depthRBTOf _ L = -1
depthRBTOf v (B n izq der)
    | n == v = 0
    | otherwise = if v < n then 1 + valueOfDepthLef else 1 + valueOfDepthRig
    where depthLef = (depthRBTOf v izq) 
          depthRig = (depthRBTOf v der)
          valueOfDepthLef = if 0 > depthLef then -2 else depthLef
          valueOfDepthRig = if 0 > depthRig then -2 else depthRig
depthRBTOf v (R n izq der)
    | n == v = 0
    | otherwise = if v < n then 1 + valueOfDepthLef else 1 + valueOfDepthRig
    where depthLef = (depthRBTOf v izq) 
          depthRig = (depthRBTOf v der)
          valueOfDepthLef = if 0 > depthLef then -2 else depthLef
          valueOfDepthRig = if 0 > depthRig then -2 else depthRig

-- Elemento minimo del arbol
minRBT :: (Eq a, Ord a) => RBTree a -> Maybe a
minRBT (L) = Nothing
minRBT (B n (L) _) = Just n
minRBT (B _ lef _) = minRBT lef
minRBT (R n (L) _) = Just n
minRBT (R _ lef _) = minRBT lef 

-- Elemento maximo del arbol
maxRBT :: (Eq a, Ord a) => RBTree a -> Maybe a
maxRBT (L) = Nothing
maxRBT (B n _ (L)) = Just n
maxRBT (B _ _ rig) = maxRBT rig
maxRBT (R n _ (L)) = Just n
maxRBT (R _ _ rig) = maxRBT rig 

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