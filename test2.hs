-- algo hace parece que es el bueno xd

data Stree a = Nil | Node (Stree a) a (Stree a)
    deriving(Show)

t :: Stree Int
t = Node (Node Nil 17 (Node Nil 32 Nil)) 44 (Node (Node (Node Nil 48 Nil) 50 (Node Nil 54 Nil)) 62 (Node Nil 78 (Node Nil 88 Nil)))

inserttree :: (Eq a, Ord a) => (Stree a) -> a -> (Stree a)
inserttree Nil x = Node Nil x Nil
inserttree (Node tleft y tright) x
    | x == y    = Node tleft y tright
    | x < y     = rebalance (Node (inserttree tleft x) y tright)
    | otherwise = rebalance (Node tleft y (inserttree tright x))

deletetree :: (Eq a, Ord a) => (Stree a) -> a -> (Stree a)
deletetree Nil x = Nil
deletetree (Node tleft y tright) x
    | x < y   = rebalance (Node (deletetree tleft x) y tright)
    | x > y   = rebalance (Node tleft y (deletetree tright x))

-- In all cases below, we must have x == y

deletetree (Node Nil y tright) x   = tright
deletetree (Node tleft y tright) x = rebalance (Node tz z tright)
    where (z,tz) = deletemax tleft

deletemax :: (Eq a, Ord a) => (Stree a) -> (a,Stree a)
deletemax (Node t1 y Nil) = (y,t1)
deletemax (Node t1 y t2) = (z, Node t1 y tz)
    where (z,tz) = deletemax t2

rebalance :: (Eq a, Ord a) => (Stree a) -> (Stree a)

rebalance (Node t1 y t2)
    | abs (sy) < 2         = Node t1 y t2
    | sy == 2 && st1 /= -1 = rotateright (Node t1 y t2)
    | sy == 2 && st1 == -1 = rotateright (Node (rotateleft t1) y t2)
    | sy == -2 && st2 /= 1 = rotateleft (Node t1 y t2)
    | sy == -2 && st2 == 1 = rotateleft (Node t1 y (rotateright t2))
    where
        sy  = slope (Node t1 y t2)
        st1 = slope t1
        st2 = slope t2

        rotateright (Node (Node t1 y t2) x t3) = Node t1 y (Node t2 x t3)
        rotateleft  (Node t1 x (Node t2 y t3)) = Node (Node t1 y t2) x t3

slope :: (Eq a, Ord a) => (Stree a) -> Int
slope Nil = 0
slope (Node t1 x t2) = (height t1) - (height t2)

height :: (Eq a, Ord a) => (Stree a) -> Int
height Nil = 0
height (Node t1 x t2) = 1 + (max (height t1) (height t2))

preorder :: Stree a -> [a]
preorder (Node l v r) = [v] ++ (preorder l) ++ (preorder r)
preorder Nil = []

--Node (Node (Node (Node Nil 0 Nil) 2 Nil) 1 (Node (Node Nil 3 Nil) 6 (Node Nil 7 Nil))) 10 (Node Nil 11 (Node Nil 12 Nil))

