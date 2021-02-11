-- TODO
--  search       X
--  insertion    X
--  delete       X
--  merges
--  fromlist     X
--  toList       X
--  min          X
--  max          X
--  isBST        X
--  equals       X
--  inorder      X
--  preorder     X
--  postorder    X

module DataStructures.BinarySearchTree(
    BSTree(..),
    insert,
    delete,
    contains,
    depth,
    depthOf,
    isBST,
    equals,
    minBST,
    maxBST,
    merge,
    given,
    inorder,
    preorder,
    postorder,
    bst2list,
    list2bst
) where

data BSTree a = N (BSTree a) (BSTree a) a
              | H
              deriving (Show, Eq)

------------------------
-- FUNCIONES AUXILIARES
------------------------

-- Valor del nodo raiz del arbol
valor :: (Eq a, Ord a) => BSTree a -> Maybe a
valor (N izq der n) = Just n
valor (H) = Nothing

-- Comprueba si es hoja
esHoja :: (Eq a, Ord a) => BSTree a -> Bool
esHoja (H) = True
esHoja _ = False

-- Comprobar si tiene hijo izquierdo
tieneIzq :: (Eq a, Ord a) => BSTree a -> Bool
tieneIzq (N (H) _ _) = False
tieneIzq (H) = False
tieneIzq _ = True

-- Comprobar si tiene hijo derecho
tieneDer :: (Eq a, Ord a) => BSTree a -> Bool
tieneDer (N _ (H) _) = False
tieneDer (H) = False
tieneDer _ = True

-- Comprobar si es Binario
isBST :: (Eq a, Ord a) => BSTree a -> Bool
isBST (N izq der n)
    | valor izq == valor der && valor der == Nothing = True
    | valor izq == Nothing = (valD > n) && isBST der
    | valor der == Nothing = (valI < n) && isBST izq
    | otherwise = (valD > n) && (valI < n) && isBST der && isBST izq
    where
        Just valI = valor izq
        Just valD = valor der
isBST (H) = True

-- BST a lista
bst2list :: (Eq a, Ord a) => BSTree a -> [a]
bst2list H = []
bst2list (N izq der n) = n : (bst2list izq ++ bst2list der)

-- Lista de elementos que cumplen una propiedad
given :: (Eq a, Ord a) => (a -> Bool) -> BSTree a -> [a]
given _ H = []
given p (N lef rig n) = if p n then n : (given p lef ++ given p rig) else (given p lef ++ given p rig)

-- Lista a BST (ha de estar ordenada)
list2bst :: (Eq a, Ord a) => [a] -> BSTree a
list2bst xs
   | l == [] && r == [] = N H H m
   | l == [] = N H (list2bst r) m
   | r == [] = N (list2bst l) H m
   | otherwise = N (list2bst l) (list2bst r) m
   where (l, m, r) = splitMid xs

splitMid :: [a] -> ([a], a, [a])
splitMid xs = (take indiceMid xs, xs!!indiceMid, drop (indiceMid+1) xs)
   where indiceMid = div (length xs) 2

equals :: (Eq a, Ord a) => BSTree a -> BSTree a -> Bool
equals (N l1 r1 a) (N l2 r2 b) = a==b && equals l1 l2 && equals r1 r2
equals (N _ _ _) H = False
equals H (N _ _ _) = False
equals H H = True


-- Comprobar depth
depth :: (Eq a, Ord a) => BSTree a -> Int
depth H = 0
depth (N izq der _) = 1 + max (depth izq) (depth der)

-- Si se encuentra en el arbol da su depth, sino da -1
depthOf :: (Eq a, Ord a) => a -> BSTree a -> Int
depthOf _ H = -1
depthOf v (N izq der n)
    | n == v = 0
    | otherwise = if v < n then 1 + (depthOf v izq) else 1 + (depthOf v der)

-- Comprueba si un elemento contains al arbol
contains :: (Eq a, Ord a) => a -> BSTree a -> Bool
contains v H = False
contains v (N izq der n) = v == n || contains v izq || contains v der

-- Encontrar el minimo
minBST :: (Eq a, Ord a) => BSTree a -> Maybe a
minBST (H) = Nothing
minBST (N (H) _ n) = Just n
minBST (N izq _ _) = minBST izq

maxBST :: (Eq a, Ord a) => BSTree a -> Maybe a
maxBST (H) = Nothing
maxBST (N _ (H) n) = Just n
maxBST (N _ der _) = maxBST der


-- agregar elemento
insert :: (Eq a, Ord a) => a -> BSTree a -> BSTree a
insert v (H) = (N (H) (H) v)
insert v (N izq der n)
    | v < n && (esHoja izq) = (N (N (H) (H) v) der n)
    | v > n && (esHoja der) = (N izq (N (H) (H) v) n)
    | v < n = (N (insert v izq) der n)
    | v > n = (N izq (insert v der) n)
    | otherwise = (N izq der n)

-- delete elemento
delete :: (Eq a, Ord a) => a -> BSTree a -> BSTree a
delete _ (H) = (H)
delete v (N izq der n)
    | v == n && (esHoja izq) && (esHoja der) = (H)  -- Caso donde no tiene hijos
    | v == n && (esHoja izq) = der                  -- Caso donde tiene un hijo
    | v == n && (esHoja der) = izq                  -- Caso donde tiene un hijo
    | v == n = (N izq (delete minimo der) minimo)  -- Caso donde tiene dos hijos -> encontrar el minimo del der
    | v < n = (N (delete v izq) der n)
    | v > n = (N izq (delete v der) n)
    where
        Just minimo = minBST der


merge :: (Eq a, Ord a) => BSTree a -> BSTree a -> BSTree a
merge a b = list2bst merged
    where inorder1 = inorder a
          inorder2 = inorder b
          merged = mergeSortedLists inorder1 inorder2

mergeSortedLists :: (Eq a, Ord a) => [a] -> [a] -> [a]
mergeSortedLists [] ys = ys
mergeSortedLists xs [] = xs
mergeSortedLists (x:xs) (y:ys) | y < x = y : mergeSortedLists (x:xs) ys
                               | otherwise = x : mergeSortedLists xs (y:ys)


-- RECORRIDOS
inorder :: BSTree a -> [a]
inorder (N l r v) = (inorder l) ++ [v] ++ (inorder r)
inorder H = []

preorder :: BSTree a -> [a]
preorder (N l r v) = [v] ++ (preorder l) ++ (preorder r)
preorder H = []

postorder :: BSTree a -> [a]
postorder (N l r v) = (postorder l) ++ (postorder r) ++ [v]
postorder H = []