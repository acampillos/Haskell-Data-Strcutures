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
    postorder
) where

import Data.Maybe

-- ÁRBOL DE BÚSQUEDA BINARIO/BINARY SEARCH TREE (BST):
-- Árbol binario cuyos nodos contienen un valor mayor que todos los del subárbol izquierdo y 
-- menor que todos los del subárbol derecho. 

-----------------------
-- CONSTRUCTORES
-----------------------

data BSTree a = N (BSTree a) (BSTree a) a
              | H
              deriving (Show, Eq)

-----------------------
-- FUNCIONES
-----------------------

valor :: (Eq a, Ord a) => BSTree a -> Maybe a
valor (N izq der n) = Just n
valor (H) = Nothing

esHoja :: (Eq a, Ord a) => BSTree a -> Bool
esHoja (H) = True
esHoja _ = False

tieneIzq :: (Eq a, Ord a) => BSTree a -> Bool
tieneIzq (N (H) _ _) = False
tieneIzq (H) = False
tieneIzq _ = True

tieneDer :: (Eq a, Ord a) => BSTree a -> Bool
tieneDer (N _ (H) _) = False
tieneDer (H) = False
tieneDer _ = True

isBST :: (Eq a, Ord a) => BSTree a -> Bool
isBST (H) = True
isBST (N izq der n)
    | valor izq == Nothing && valor der == Nothing = True
    | valor izq == Nothing = (valD > n) && isBST der
    | valor der == Nothing = (valI < n) && isBST izq
    | otherwise = (valD > n) && (valI < n) && isBST der && isBST izq
    where
        Just valI = valor izq
        Just valD = valor der

given :: (Eq a, Ord a) => (a -> Bool) -> BSTree a -> [a]
-- Lista de elementos que cumplen una propiedad
given _ H = []
given p (N lef rig n) = if p n then n : (given p lef ++ given p rig) else (given p lef ++ given p rig)


equals :: (Eq a, Ord a) => BSTree a -> BSTree a -> Bool
-- Compara dos árboles y determina si son iguales.
-- Han de tener igual valor en sus nodos y la misma estructura.
-- Parámetros: Árbol 1
--             Árbol 2
-- Devuelve:   Booleano
equals (N l1 r1 a) (N l2 r2 b) = a==b && equals l1 l2 && equals r1 r2
equals (N _ _ _) H = False
equals H (N _ _ _) = False
equals H H = True


depth :: (Eq a, Ord a) => BSTree a -> Int
-- Obtiene la profundidad del árbol.
depth H = 0
depth (N izq der _) = 1 + max (depth izq) (depth der)

depthOf :: (Eq a, Ord a) => a -> BSTree a -> Int
-- Profundidad de un elemento. Si no está contenido devuelve -1.
depthOf _ H = -1
depthOf v (N izq der n)
    | n == v = 0
    | otherwise = if v < n then 1 + (depthOf v izq) else 1 + (depthOf v der)

contains :: (Eq a, Ord a) => a -> BSTree a -> Bool
-- Comprueba si un elemento está contenido en un árbol.
contains v H = False
contains v (N izq der n) = v == n || contains v izq || contains v der

minBST :: (Eq a, Ord a) => BSTree a -> Maybe a
minBST (H) = Nothing
minBST (N (H) _ n) = Just n
minBST (N izq _ _) = minBST izq

maxBST :: (Eq a, Ord a) => BSTree a -> Maybe a
maxBST (H) = Nothing
maxBST (N _ (H) n) = Just n
maxBST (N _ der _) = maxBST der


insert :: (Eq a, Ord a) => a -> BSTree a -> BSTree a
-- Agrega un elemento al árbol.
-- Parámetros: Elemento
--             Árbol
-- Devuelve:   Árbol con el elemento
insert v (H) = (N (H) (H) v)
insert v (N izq der n)
    | v < n && (esHoja izq) = (N (N (H) (H) v) der n)   -- Si es menor que el nodo actual pero la izquierda es una hoja
    | v > n && (esHoja der) = (N izq (N (H) (H) v) n)   -- Si es mayor que el nodo actual pero la derecha es una hoja
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

-- BST a lista
bst2list :: (Eq a, Ord a) => BSTree a -> [a]
-- Obtiene un BST y devuelve sus nodos en una lista (preorder)
-- Parámetros: BST
-- Devuelve:   Lista de a
bst2list t = preorder t



{-inorder2bst :: (Eq a, Ord a) => [a] -> BSTree a
inorder2bst xs = undefined

t :: BSTree Int
t = N (N (N H H 4) (N H H 5) 2) (N H H 3) 1

pre,post,ino :: [Int]
pre = [1,2,4,5,3]
--post = [4,5,2,3,1]
post = [3,9,14,10,8]
ino = [4,2,5,1,3]

preorder2bst :: (Eq a, Ord a) => [a] -> BSTree a
preorder2bst (x:y:z:xs) = undefined

postorder2bst :: [Int] -> BSTree Int
postorder2bst xs = postorder2bst' xs ((length xs)-1) (minBound :: Int) (maxBound :: Int)

postorder2bst' xs i l u
    | i > 0 = if key < u && key > l
                then if i>1
                        then N (postorder2bst' (take (i-1) xs) (i-2) key u) (postorder2bst' (take (i-1) xs) (i-2) l key) key
                     else N (postorder2bst' (take (i-1) xs) (i-1) key u) (postorder2bst' (take (i-1) xs) (i-1) l key) key
               else N H H (xs!!i)
    | otherwise = N H H (xs!!i)
    where key = if i > 0 then (xs!!i) else error "A"-}
