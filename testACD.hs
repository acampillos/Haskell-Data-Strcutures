--import Data.List as L
--import DataStructures.BinarySearchTree as BST


------------------------
-- TESTS
------------------------

-- BST
{--
test1, test2 :: BSTree Int
test1 = foldr BST.insert H [50,40,60,70,80]
test2 = undefined
ejbt1,ejbt2,ejbt3,ejbt4,ejbt5 :: BSTree Int
ejbt1 = (N (N (H) (H) 5) (N (N (N (H) (H) 9) (N (H) (H) 13) 12) (N (H) (N (H) (H) 23) 19) 15) 8)
-- Prueba para ver que no es SBT
ejbt2 = (N (N (H) (H) 5) (N (N (N (H) (H) 9) (N (H) (H) 13) 12) (N (H) (N (H) (H) 23) 19) 15) 2000) 
ejbt4 = (N (H) (H) 4)
ejbt3 = (H)
ejbt5 = (N (N (N (H) (H) 1) (N (N (N (H) (H) 122) (N (H) (H) 126) 125) (N (N (N (H) (H) 170) (N (H) (H) 174) 173) (N (H) (N (H) (H) 180) 176) 175) 150) 100) (N (H) (H) 400) 200)

test1, test2 :: BSTree Int
test1 = (N (N (H) (H) 40) (N (N (H) (H) 60) (N (H) (H) 80) 70) 50)
test2 = (N (N (H) (H) 40) (N (N (H) (H) 60) (N (H) (H) 80) 70) 50)--}


-- AVLTree

data AVLTree a = N a (AVLTree a) (AVLTree a)
              | H
              deriving (Show, Eq)

isLeaf :: (Eq a, Ord a) => AVLTree a -> Bool
isLeaf (H) = True
isLeaf _ = False

minAVL :: (Eq a, Ord a) => AVLTree a -> a
minAVL (N v (H) _) = v
minAVL (N _ l _) = minAVL l

avlMax :: AVLTree e -> Maybe e
avlMax H = Nothing
avlMax (N v _ H) = Just v
avlMax (N v _ r) = avlMax r

left :: AVLTree a -> AVLTree a
left H = H
left (N _ l _) = l

right :: AVLTree a -> AVLTree a
right H = H
right (N _ _ r) = r

depth :: AVLTree a -> Int
depth H = -1
depth (N _ l r) = 1 + max (depth l) (depth r)

balanced :: AVLTree a -> Bool
balanced H = True
balanced (N v l r)
    | not (balanced r) = False
    | not (balanced l) = False
    | bf > 1 = False
    | otherwise = True
    where bf = balanceFactor l r

balanceFactor :: AVLTree a -> AVLTree a -> Int
balanceFactor a b = abs((depth a)-(depth b))

value :: AVLTree a -> a
value H = error "ERROR"
value (N v l r) = v

insert :: (Ord a) => AVLTree a -> a -> AVLTree a
insert H a = N a H H
insert (N v l r) a
    | v > a = rotate(N v (insert l a) r)
    | otherwise =  rotate(N v l (insert r a))

rotate :: AVLTree a -> AVLTree a
rotate H = H
rotate (N v l r) | not (balanced l) = N v (rotate l) r
                 | not (balanced r) = N v l (rotate r)
                 -- left left
                 | (depth r) + 1 < (depth r) && (depth (right l)) < (depth (left l)) = N (value l) (left l) (N v (right l) r)
                 -- left right
                 | (depth r) + 1 < (depth l) && (depth (right l)) > (depth (left l)) = N (value (right l)) (N (value l) (left l) (left (right l))) (N v (right (right l)) r)
                 -- right right
                 | (depth l) + 1 < (depth r) && (depth (left r)) < (depth (right r)) = N (value r) (N v l (left r)) (right r)
                 -- right left
                 | (depth l) + 1 < (depth r) && (depth (left r)) > (depth (right r)) = N (value (left r)) (N v l (left (left r))) (N (value r) (right (left r)) (right r))
                 | otherwise = N v l r

delete :: (Eq a, Ord a) => AVLTree a -> a -> AVLTree a
delete H _ = H
delete (N v l r) a = rotate(del)
    where del = delete' (N v l r) a

delete' :: (Eq a, Ord a) => AVLTree a -> a -> AVLTree a
delete' H _ = H
delete' (N v l r) a
    | v==a && isLeaf l && isLeaf r = H
    | v==a && isLeaf l = r
    | v==a && isLeaf r = l
    | v==a = rotate(N minimo l (delete' r minimo))
    | v > a = rotate(N v (delete' l a) r)
    | v < a = rotate(N v l (delete' r a))
    where minimo = minAVL r

contains :: (Eq a, Ord a) => a -> AVLTree a -> Bool
contains _ H = False
contains x (N v l r) = v == x || contains v l || contains v r

depthOf :: (Eq a, Ord a) => a -> AVLTree a -> Int
depthOf _ H = -1
depthOf v (N n izq der)
    | n == v = 0
    | otherwise = if v < n then 1 + (depthOf v izq) else 1 + (depthOf v der)


inorder :: AVLTree a -> [a]
inorder (N v l r) = (inorder l) ++ [v] ++ (inorder r)
inorder H = []

preorder :: AVLTree a -> [a]
preorder (N v l r) = [v] ++ (preorder l) ++ (preorder r)
preorder H = []

postorder :: AVLTree a -> [a]
postorder (N v l r) = (postorder l) ++ (postorder r) ++ [v]
postorder H = []

equals :: (Eq a, Ord a) => AVLTree a -> AVLTree a -> Bool
equals (N a l1 r1) (N b l2 r2) = a==b && equals l1 l2 && equals r1 r2
equals (N _ _ _) H = False
equals H (N _ _ _) = False
equals H H = True

test1, test2, test3, test4 :: AVLTree Int
test1 = (N 3 (N 2 (N 1 H H) H) H)
test2 = (N 20 (N 10 (N 7 H H) (N 15 H H)) (N 25 H (N 32 H H)))
test3 = (N 44 (N 17 H (N 32 H H)) (N 62 (N 50 (N 48 H H) (N 54 H H)) (N 78 H (N 88 H H))))
test4 = (N 44 (N 17 H H) (N 62 (N 50 (N 48 H H) (N 54 H H)) (N 78 H (N 88 H H))))

------------------------
------------------------
-- SORTING
------------------------
------------------------

-- quicksort :: (Ord a)= > 