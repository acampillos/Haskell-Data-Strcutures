{--The left subtree of a node contains only nodes with keys lesser than the node’s key.
The right subtree of a node contains only nodes with keys greater than the node’s key.
The left and right subtree each must also be a binary search tree.--}

-- search
-- insertion
-- delete
-- merges
-- fromlist
-- minimum
-- isBST
-- equal
-- traversals: inorder, preorder, postorder
-- levelorder
-- inverted
-- DFS y BFS

module BinarySearchTree(
    BSTree,
    insert,
    delete
) where

data BSTree a = E | N (BSTree a) (BSTree a)
    deriving (Eq, Show)

insert :: Ord a => BSTree a -> a -> BSTree a
insert E a = N a E E
insert (N a l r) b
    | a==b = N l r
    | a < b = N (insert l b) r
    | a > b = N l (insert r b)

delete :: Ord a => BSTree a -> a -> BSTree
delete E a = E
delete (N a l r) b
    | a==b = twoChilds a b
    | a < b = N l (delete r b)
    | a > b = N (delete l b) r

twoChilds E _ = b
twoChilds _ E = a
twoChilds (N a l r) b = 
    -- Caso en el que tiene 2 hijos, hay que hacer que el nodo raiz (a) pase a tener el valor del elemento mas pequeño de su hijo derecho (r)
