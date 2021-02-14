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
    helpBST
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

helpBST :: IO ()
helpBST = putStrLn "\ndata BSTree a = N (BSTree a) (BSTree a) a\n              | H\n              deriving (Show, Eq)\n\nvalor :: (Eq a, Ord a) => BSTree a -> Maybe a\n\nesHoja :: (Eq a, Ord a) => BSTree a -> Bool\n\ntieneIzq :: (Eq a, Ord a) => BSTree a -> Bool\n\ntieneDer :: (Eq a, Ord a) => BSTree a -> Bool\n\nisBST :: (Eq a, Ord a) => BSTree a -> Bool\n\ngiven :: (Eq a, Ord a) => (a -> Bool) -> BSTree a -> [a]\n\nequals :: (Eq a, Ord a) => BSTree a -> BSTree a -> Bool\n\ndepth :: (Eq a, Ord a) => BSTree a -> Int\n\ndepthOf :: (Eq a, Ord a) => a -> BSTree a -> Int\n\ncontains :: (Eq a, Ord a) => a -> BSTree a -> Bool\n\nminBST :: (Eq a, Ord a) => BSTree a -> Maybe a\n\nmaxBST :: (Eq a, Ord a) => BSTree a -> Maybe a\n\ninsert :: (Eq a, Ord a) => a -> BSTree a -> BSTree a\n\ndelete :: (Eq a, Ord a) => a -> BSTree a -> BSTree a\n\nmerge :: (Eq a, Ord a) => BSTree a -> BSTree a -> BSTree a\n\ninorder :: BSTree a -> [a]\n\npreorder :: BSTree a -> [a]\n\npostorder :: BSTree a -> [a]"

valor :: (Eq a, Ord a) => BSTree a -> Maybe a
-- Obtiene el valor del nodo raiz del arbol
-- Entradas: arbol
--      arbol (BSTree): Arbol de cuyo nodo raiz se quiere sacar el valor
-- Salidas: valor
--      valor (Maybe a): Si el arbol es una hoja, obtiene Nothing, obtiene un Just valor en otro caso
valor (N izq der n) = Just n
valor (H) = Nothing

esHoja :: (Eq a, Ord a) => BSTree a -> Bool
-- Comprueba si el arbol es una hoja
-- Entradas: arbol
--      arbol (BSTree): Arbol del que se quiere comprobar si es una hoja
-- Salidas: propiedad
--      propiedad (Bool): False en lugar de no ser una hoja, True en otro caso
esHoja (H) = True
esHoja _ = False

tieneIzq :: (Eq a, Ord a) => BSTree a -> Bool
-- Comprueba si el hijo izquierdo no es una hoja
-- Entradas: arbol
--      arbol (BSTree): Arbol del que se quiere comprobar si tiene hijo izquierdo
-- Salidas: propiedad
--      propiedad (Bool): True en caso de tener hijo izquierdo, False en otro caso.
tieneIzq (N (H) _ _) = False
tieneIzq (H) = False
tieneIzq _ = True

tieneDer :: (Eq a, Ord a) => BSTree a -> Bool
-- Comprueba si el hijo derecho no es una hoja
-- Entradas: arbol
--      arbol (BSTree): Arbol del que se quiere comprobar si tiene hijo derecho
-- Salidas: propiedad
--      propiedad (Bool): True en caso de tener hijo derecho, False en otro caso.
tieneDer (N _ (H) _) = False
tieneDer (H) = False
tieneDer _ = True

isBST :: (Eq a, Ord a) => BSTree a -> Bool
-- Comprueba si se se cumplen las propiedades de ser un arbol de búsqueda binaria
-- Entradas: arbol
--      arbol (BSTree): Arbol del que se quiere comprobar las propiedades
-- Salidas: propiedad
--      propiedad (Bool): True si cumple las propiedades, False en otro caso
isBST (H) = True
isBST (N izq der n)
    | valor izq == Nothing && valor der == Nothing = True               -- Caso de no tener hijos
    | valor izq == Nothing = (valD > n) && isBST der                    -- Casos de tener un hijo
    | valor der == Nothing = (valI < n) && isBST izq
    | otherwise = (valD > n) && (valI < n) && isBST der && isBST izq    -- Caso de tener dos hijos
    where
        Just valI = valor izq
        Just valD = valor der

given :: (Eq a, Ord a) => (a -> Bool) -> BSTree a -> [a]
-- Lista de elementos que cumplen una propiedad en un arbol
-- Entradas: p -> arbol
--      p (a -> Bool): Propiedad aplicada a los elementos del arbol
--      arbol (BSTree): Arbol en el que se comprueba la propiedad
-- Salidas: l
--      l ([a]): Lista de elementos del arbol que cumplen la propiedad
given _ H = []
given p (N lef rig n) = if p n then n : (given p lef ++ given p rig) else (given p lef ++ given p rig)


equals :: (Eq a, Ord a) => BSTree a -> BSTree a -> Bool
-- Compara dos árboles y determina si son iguales.
-- Han de tener igual valor en sus nodos y la misma estructura.
-- Entradas: arbol1 -> arbol2
--      arbol1 (BSTree): Uno de los dos arboles del que se quiere comprobar la igualdad
--      arbol2 (BSTree): El otro arbol del que se quiere comprobar la igualdad
-- Salidas: igual
--      igual (Bool): True en caso de ser iguales, False en otro caso
equals (N l1 r1 a) (N l2 r2 b) = a==b && equals l1 l2 && equals r1 r2
equals (N _ _ _) H = False
equals H (N _ _ _) = False
equals H H = True


depth :: (Eq a, Ord a) => BSTree a -> Int
-- Obtiene la profundidad del árbol.
-- Entradas: arbol
--      arbol (BSTree): Arbol del que se quiere comprobar la profundidad
-- Salidas: p
--      p (Int): Profundidad del arbol
depth H = 0
depth (N izq der _) = 1 + max (depth izq) (depth der)

depthOf :: (Eq a, Ord a) => a -> BSTree a -> Int
-- Profundidad de un elemento. Si no está contenido devuelve -1.
-- Entradas: v -> arbol
--      v (a): Valor buscado en el arbol
--      arbol (BSTree): Arbol en el que se busca
-- Salidas: d
--      d (Int): Profundidad del elemento v, en caso de no estar, es -1
depthOf _ H = -1
depthOf v (N izq der n)
    | n == v = 0
    | otherwise = if v < n then 1 + (depthOf v izq) else 1 + (depthOf v der)

contains :: (Eq a, Ord a) => a -> BSTree a -> Bool
-- Comprueba si un elemento está contenido en un árbol.
-- Entradas: v -> arbol
--      v (a): Valor buscado en el arbol
--      arbol (BSTree): Arbol en el que se busca
-- Salidas: p
--      p (Bool): True en caso de estar v en el arbol, False en otro caso.
contains v H = False
contains v (N izq der n) = v == n || contains v izq || contains v der

minBST :: (Eq a, Ord a) => BSTree a -> Maybe a
-- Elemento minimo del arbol
-- Entradas: arbol
--    arbol (BSTree): Arbol donde se busca
--  Salidas: valor
--    valor (Maybe a): En caso de ser un arbol hoja, es Nothing, en otro caso el valor minimo
minBST (H) = Nothing
minBST (N (H) _ n) = Just n
minBST (N izq _ _) = minBST izq

maxBST :: (Eq a, Ord a) => BSTree a -> Maybe a
-- Elemento maximo del arbol
-- Entradas: arbol
--    arbol (BSTree): Arbol donde se busca
--  Salidas: valor
--    valor (Maybe a): En caso de ser un arbol hoja, es Nothing, en otro caso el valor maximo
maxBST (H) = Nothing
maxBST (N _ (H) n) = Just n
maxBST (N _ der _) = maxBST der


insert :: (Eq a, Ord a) => a -> BSTree a -> BSTree a
-- Agrega un elemento al árbol.
-- Entradas: v -> arbol
--      v (a): Elemento que se quiere insertar
--      arbol (BSTree): Arbol donde se quiere insertar
-- Devuelve: insertado
--      insertado (BSTree): Arbol con el elemento insertado
insert v (H) = (N (H) (H) v)
insert v (N izq der n)
    | v < n && (esHoja izq) = (N (N (H) (H) v) der n)   -- Si es menor que el nodo actual pero la izquierda es una hoja
    | v > n && (esHoja der) = (N izq (N (H) (H) v) n)   -- Si es mayor que el nodo actual pero la derecha es una hoja
    | v < n = (N (insert v izq) der n)
    | v > n = (N izq (insert v der) n)
    | otherwise = (N izq der n)

-- delete elemento
delete :: (Eq a, Ord a) => a -> BSTree a -> BSTree a
-- Elimina un elemento del arbol
-- Entradas: v -> arbol
--      v (a): Elemento que se quiere eliminar
--      arbol (BSTree): Arbol donde se quiere eliminar
-- Devuelve: eliminado
--      eliminado (BSTree): Arbol con el elemento eliminado
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
-- Fusion de dos ramas
-- Entradas: arbol1 -> arbol2
--      arbol1 (BSTree): Uno de los dos arboles que se quieren fusionar
--      arbol2 (BSTree): El otro arbol que va a ser fusionado
-- Salidas: fusionado
--      fusionado (BSTree): Arbol fusion de los dos anteriores
merge a b = list2bst merged
    where inorder1 = inorder a
          inorder2 = inorder b
          merged = mergeSortedLists inorder1 inorder2

mergeSortedLists :: (Eq a, Ord a) => [a] -> [a] -> [a]
-- Fusion de listas ordenadas
-- Entradas: xs -> ys
--      xs ([a]): Una de las dos listas a ser fusionadas
--      ys ([a]): La otra lista a ser fusionada
-- Salidas: zs
--      zs ([a]): La fusion de las dos listas
mergeSortedLists [] ys = ys
mergeSortedLists xs [] = xs
mergeSortedLists (x:xs) (y:ys) | y < x = y : mergeSortedLists (x:xs) ys
                               | otherwise = x : mergeSortedLists xs (y:ys)

list2bst :: (Eq a, Ord a) => [a] -> BSTree a
-- Transforma una lista de elementos a un BST
-- Entradas: xs
--      xs ([a]): Lista de elementos a
-- Salidas: arbol
--      arbol (BSTree): Arbol BST con los elementos de la lista
list2bst xs
   | l == [] && r == [] = N H H m
   | l == [] = N H (list2bst r) m
   | r == [] = N (list2bst l) H m
   | otherwise = N (list2bst l) (list2bst r) m
   where (l, m, r) = splitMid xs

splitMid :: [a] -> ([a], a, [a])
-- Separar una lista en un trio de tres elementos, donde el elemento central es el centro de
-- la lista, y a sus laterales, los trozos de la lista que corresponderian siguiendo el orden
-- original.
-- Entradas: xs
--      xs ([a]): Lista de elementos
-- Salidas: (ys, x, zs)
--      ys ([a]): Lista de elementos a la izquierda de x en xs
--      x (a): Elemento central de xs
--      ys ([a]): Lista de elementos a la derecha de x en xs
splitMid xs = (take indiceMid xs, xs!!indiceMid, drop (indiceMid+1) xs)
   where indiceMid = div (length xs) 2

-- RECORRIDOS
inorder :: BSTree a -> [a]
-- Lista con el recorrido "inorder" del arbol
-- Entradas: arbol
--      arbol (BSTree): Arbol a recorrer
-- Salidas: xs
--      xs ([a]): Recorrido "inorder" del arbol
inorder (N l r v) = (inorder l) ++ [v] ++ (inorder r)
inorder H = []

preorder :: BSTree a -> [a]
-- Lista con el recorrido "preorder" del arbol
-- Entradas: arbol
--      arbol (BSTree): Arbol a recorrer
-- Salidas: xs
--      xs ([a]): Recorrido "preorder" del arbol
preorder (N l r v) = [v] ++ (preorder l) ++ (preorder r)
preorder H = []

postorder :: BSTree a -> [a]
-- Lista con el recorrido "postorder" del arbol
-- Entradas: arbol
--      arbol (BSTree): Arbol a recorrer
-- Salidas: xs
--      xs ([a]): Recorrido "postorder" del arbol
postorder (N l r v) = (postorder l) ++ (postorder r) ++ [v]
postorder H = []

-- BST a lista
bst2list :: (Eq a, Ord a) => BSTree a -> [a]
-- Obtiene un BST y devuelve sus nodos en una lista (preorder)
-- Parámetros: BST
-- Devuelve:   Lista de a
bst2list t = preorder t


-- Ejemplos BST
ejbt1,ejbt2,ejbt3,ejbt4,ejbt5,ejbt6,ejbt7 :: BSTree Int
ejbt1 = foldr insert H [50,40,60,70,80]
ejbt2 = (N (N (H) (H) 5) (N (N (N (H) (H) 9) (N (H) (H) 13) 12) (N (H) (N (H) (H) 23) 19) 15) 8)
ejbt3 = (N (N (H) (H) 40) (N (N (H) (H) 60) (N (H) (H) 80) 70) 50)
-- Prueba para ver que no es SBT
ejbt4 = (N (N (H) (H) 5) (N (N (N (H) (H) 9) (N (H) (H) 13) 12) (N (H) (N (H) (H) 23) 19) 15) 2000) 
ejbt5 = (N (H) (H) 4)
ejbt6 = (H)
ejbt7 = (N (N (N (H) (H) 1) (N (N (N (H) (H) 122) (N (H) (H) 126) 125) (N (N (N (H) (H) 170) (N (H) (H) 174) 173) (N (H) (N (H) (H) 180) 176) 175) 150) 100) (N (H) (H) 400) 200)
