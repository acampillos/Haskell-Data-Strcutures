{--- avl2list
-- list2avl
-- minAVL
-- maxAVL
-- delete
-- merge

import Control.Applicative ((<|>))

data Tree a
  = Leaf
  | Node
      Int
      (Tree a)
      a
      (Tree a)
  deriving (Show, Eq)
 
t2 :: Tree Int
t2 = Node 4 (Node 2 Leaf 17 (Node 1 Leaf 32 Leaf)) 44 (Node 3 (Node 2 (Node 1 Leaf 48 Leaf) 50 (Node 1 Leaf 54 Leaf)) 62 (Node 2 Leaf 78 (Node 1 Leaf 88 Leaf)))

t :: Tree Int
t = Node 4 (Node 3 (Node 2 (Node 1 Leaf 8 Leaf) 9 (Node 1 Leaf 11 Leaf)) 13 (Node 1 Leaf 21 Leaf)) 33 (Node 2 Leaf 53 (Node 1 Leaf 61 Leaf))

foldTree :: Ord a => [a] -> Tree a
foldTree = foldr insert Leaf
 
height :: Tree a -> Int
height Leaf = -1
height (Node h _ _ _) = h
 
depth :: Tree a -> Tree a -> Int
depth a b = succ (max (height a) (height b))
 
insert :: Ord a => a -> Tree a -> Tree a
insert v Leaf = Node 1 Leaf v Leaf
insert v t@(Node n left v_ right)
  | v_ < v = rotate $ Node n left v_ (insert v right)
  | v_ > v = rotate $ Node n (insert v left) v_ right
  | otherwise = t
 
max_ :: Ord a => Tree a -> Maybe a
max_ Leaf = Nothing
max_ (Node _ _ v right) =
  case right of
    Leaf -> Just v
    _ -> max_ right
 
delete :: Ord a => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete x (Node h left v right)
  | x == v =
    maybe left (rotate . (Node h left <*> (`delete` right))) (max_ right)
  | x > v = rotate $ Node h left v (delete x right)
  | x < v = rotate $ Node h (delete x left) v right
 
rotate :: Tree a -> Tree a
rotate Leaf = Leaf
rotate (Node h (Node lh ll lv lr) v r)
  -- Left Left.
  | lh - height r > 1 && height ll - height lr > 0 =
    Node lh ll lv (Node (depth r lr) lr v r)
rotate (Node h l v (Node rh rl rv rr))
  -- Right Right.
  | rh - height l > 1 && height rr - height rl > 0 =
    Node rh (Node (depth l rl) l v rl) rv rr
rotate (Node h (Node lh ll lv (Node rh rl rv rr)) v r)
  -- Left Right.
  | lh - height r > 1 =
    Node h (Node (rh + 1) (Node (lh - 1) ll lv rl) rv rr) v r
rotate (Node h l v (Node rh (Node lh ll lv lr) rv rr))
  -- Right Left.
  | rh - height l > 1 =
    Node h l v (Node (lh + 1) ll lv (Node (rh - 1) lr rv rr))
rotate (Node h l v r) =
  -- Re-weighting.
  let (l_, r_) = (rotate l, rotate r)
   in Node (depth l_ r_) l_ v r_
 
draw :: Show a => Tree a -> String
draw t = '\n' : draw_ t 0 <> "\n"
  where
    draw_ Leaf _ = []
    draw_ (Node h l v r) d = draw_ r (d + 1) <> node <> draw_ l (d + 1)
      where
        node = padding d <> show (v, h) <> "\n"
        padding n = replicate (n * 4) ' '-}



data AVL a = Leaf | Node Int a (AVL a) (AVL a)
  deriving (Show, Eq)

data Direction = LH | RH | ROOT
    deriving (Eq, Show)

path :: (Ord a) => a -> AVL a -> [(Direction, AVL a)] -> [(Direction, AVL a)]
path a Leaf ps = ps
path a (Node h x l r) ps
    | a < x = path a l ((LH, l):ps)     -- Tomamos la rama izquierda
    | a > x = path a r ((RH, r):ps)     -- Tomamos la rama derecha
    | otherwise = ps                    -- Hemos alcanzado el valor a

--path2tree :: (Ord a) => [(Direction, AVL a)] -> AVL a
--path2tree ((_,n):[]) = n
--path2tree ((LH,x):(_,p):[]) = zig x p
--path2tree ((RH,x):(_,p):[]) = zag x p
--path2tree ((LH,x):(LH,p):(z,g):ps) = path2tree $ (z, zigzig x p g):ps
--path2tree ((RH,x):(RH,p):(z,g):ps) = path2tree $ (z, zagzag x p g):ps
--path2tree ((LH,x):(RH,p):(z,g):ps) = path2tree $ (z, zigzag x p g):ps
--path2tree ((RH,x):(LH,p):(z,g):ps) = path2tree $ (z, zagzig x p g):ps

--zig (Node h1 x a b) (Node h2 p _ c) = Node (h1-1) x a (Node (h2+1) p b c)
--zag (Node h1 x a b) (Node h2 p c _) = Node (h2-1) x (Node (h1+1) p c a) b

-- Las alturas que cambian son las de los nodos que 
--zig (Node h1 x a b) (Node h2 p _ c) = Node h1 x a (Node (h2-2) p b c)
----zag (Node h1 x a b) (Node h2 p c _) = Node h2 x (Node (h1-2) p c a) b
--zag (Node h1 x a b) (Node h2 p c _) = Node h1 x (Node (h2-2) p c a) b

zig (Node h1 x a b) (Node h2 p _ c) = 
  if h1==0 
    then Node (h1+1) x a (Node ((height c)+1) p b c)
  else 
    Node h1 x a (Node ((height c)+1) p b c)
zag (Node h1 x a b) (Node h2 p c _) = 
  if h1==0
    then Node (h1+1) x (Node ((height c)+1) p c a) b
  else
    Node h1 x (Node ((height c)+1) p c a) b



rotate :: (Ord a) => a -> AVL a -> AVL a
rotate a t = path2tree' (path a t [(ROOT, t)])

path2tree' :: (Ord a) => [(Direction, AVL a)] -> AVL a
-- si dse dan rotaciones en la propia raiz hay que quitarle 1 a su altura
path2tree' ((LH, x):(ROOT, p):[])
  | getBalance merged > 1 = zig x p
  | otherwise = merged
  where merged = Node ((height x)+1) (value p) x (right p)
path2tree' ((RH, x):(ROOT, p):[])
  | getBalance merged > 1 = zag x p
  | otherwise = merged
  where merged = Node ((height x)+1) (value p) (left p) x

path2tree' ((_,p):[]) = setHeight p
path2tree' ((LH,x):(LH,p):ps)
  | abs(height x - height (right p)) > 1 = path2tree' ((LH, zig x p):ps)
  | otherwise = path2tree' ((LH,setHeight p):ps)
path2tree' ((RH,x):(RH,p):ps)
  | abs(height x - height (right p)) > 1 = path2tree' ((RH, zag x p):ps)
  | otherwise = path2tree' ((RH,setHeight p):ps)

{-path2tree' ((RH,x):(LH,p):(z,g):ps)
  | getBalance g > 1 = path2tree' ((z, zig (zag x p) g):ps)
  | otherwise = path2tree' ((z,setHeight g):ps)

path2tree' ((LH,x):(RH,p):(z,g):ps)
  | getBalance g > 1 = path2tree' ((z, zag (zig x p) g):ps)
  | otherwise = path2tree' ((z,setHeight g):ps)-}

-- z e y son iguales por el caso que es, en el 1o es LH y en el 2o RH
path2tree' ((RH,x):(LH,p):(z,g):(y,q):ps)
  | getBalance g > 1 = let rotated = zig (zag x p) g in path2tree' ((y, Node ((height rotated)+1) (value q) (setHeight rotated) (right q)):ps)
  | otherwise = path2tree' ((z,setHeight g):ps)

path2tree' ((LH,x):(RH,p):(z,g):(y,q):ps)
  | getBalance g > 1 = let rotated = zag (zig x p) g in path2tree' ((y, Node ((height rotated)+1) (value q) (left q) (setHeight rotated)):ps)
  | otherwise = path2tree' ((z,setHeight g):ps)

setHeight :: AVL a -> AVL a
setHeight Leaf = Leaf
setHeight (Node h x l r) = Node (1 + max hl hr) x l r
  where hl = height l
        hr = height r

isLeaf :: AVL a -> Bool
isLeaf Leaf = True
isLeaf (Node h x l r) = False

value :: AVL a -> a
value Leaf = error "E"
value (Node h x l r) = x

height :: AVL a -> Int
height Leaf = -1
height (Node h x l r) = h

left :: AVL a -> AVL a
left Leaf = error "E"
left (Node h x l r) = l

right :: AVL a -> AVL a
right Leaf = error "E"
right (Node h x l r) = r

getBalance :: AVL a -> Int
getBalance Leaf = 0
getBalance (Node h x l r) = abs(height l - height r)

insert :: (Ord a) => a -> AVL a -> AVL a
insert v Leaf = Node 0 v Leaf Leaf
insert v t@(Node h x l r) = rotate v inserted
  where inserted = insert' v t

insert' :: (Ord a) => a -> AVL a -> AVL a
insert' v Leaf = Node 0 v Leaf Leaf
insert' v (Node h x l r)
    | v < x && (isLeaf l) = Node (1 + max 0 (height r)) x (Node 0 v Leaf Leaf) r
    | v > x && (isLeaf r) = Node (1 + max 0 (height l)) x l (Node 0 v Leaf Leaf)
    | v < x = Node (h+1) x (insert' v l) r
    | v > x = Node (h+1) x l (insert' v r)
    | otherwise = Node h x l r


delete :: (Eq a, Ord a) => a -> AVL a -> AVL a
delete _ Leaf = error "E"
delete v t@(Node h x l r) = rotate m deleted
  where deleted = delete' v t
        Just m = minAVL t

delete' :: (Eq a, Ord a) => a -> AVL a -> AVL a
delete' _ Leaf = error "E"
delete' v (Node h x l r)
    | v == x && (isLeaf l) && (isLeaf r) = Leaf
    | v == x && (isLeaf l) = r
    | v == x && (isLeaf r) = l
    | v == x = setHeight (Node h minimo l (delete' minimo r))
    | v < x = setHeight (Node h x (delete' v l) r)
    | v > x = setHeight (Node h x l (delete' v r))
    where
        Just minimo = minAVL r

minAVL :: (Eq a, Ord a) => AVL a -> Maybe a
minAVL Leaf = Nothing
minAVL (Node _ n Leaf r) = Just n
minAVL (Node _ _ l _) = minAVL l

maxAVL :: (Eq a, Ord a) => AVL a -> Maybe a
maxAVL Leaf = Nothing
maxAVL (Node _ n _ Leaf) = Just n
maxAVL (Node _ _ _ r) = maxAVL r


ej1, ej2, ej2izq, ej2izq', ej3, ej3', ej4, ej4', ej5, ej5', ej6, ej7, ej8, ej9 :: AVL Int
-- INSERTS
-- left left
ej1 = Node 3 13 (Node 2 10 (Node 1 5 (Node 0 4 Leaf Leaf) (Node 0 8 Leaf Leaf)) (Node 0 11 Leaf Leaf)) (Node 1 15 Leaf (Node 0 16 Leaf Leaf))
ej2 = Node 4 13 (Node 3 10 (Node 2 5 (Node 1 4 (Node 0 3 Leaf Leaf) Leaf) (Node 0 8 Leaf Leaf)) (Node 0 11 Leaf Leaf)) (Node 1 15 Leaf (Node 0 16 Leaf Leaf))
ej2izq = Node 2 5 (Node 1 4 (Node 0 3 Leaf Leaf) Leaf) (Node 0 8 Leaf Leaf)
ej2izq' = Node 3 10 (Node 2 5 (Node 1 4 (Node 0 3 Leaf Leaf) Leaf) (Node 0 8 Leaf Leaf)) (Node 0 11 Leaf Leaf)

-- right right
ej3 = Node 2 30 (Node 0 5 Leaf Leaf) (Node 1 35 (Node 0 32 Leaf Leaf) (Node 0 40 Leaf Leaf))
ej3' = Node 3 30 (Node 0 5 Leaf Leaf) (Node 2 35 (Node 0 32 Leaf Leaf) (Node 1 40 Leaf (Node 0 45 Leaf Leaf)))

-- left right
ej4 = Node 3 13 (Node 2 10 (Node 1 5 (Node 0 4 Leaf Leaf) (Node 0 6 Leaf Leaf)) (Node 0 11 Leaf Leaf)) (Node 1 15 Leaf (Node 0 16 Leaf Leaf))
ej4' = Node 4 13 (Node 3 10 (Node 2 5 (Node 0 4 Leaf Leaf) (Node 1 6 Leaf (Node 0 7 Leaf Leaf))) (Node 0 11 Leaf Leaf)) (Node 1 15 Leaf (Node 0 16 Leaf Leaf))

-- right left
ej5 = Node 3 5 (Node 2 2 (Node 0 1 Leaf Leaf) (Node 1 4 (Node 0 3 Leaf Leaf) Leaf)) (Node 2 7 (Node 0 6 Leaf Leaf) (Node 1 9 Leaf (Node 0 16 Leaf Leaf)))
ej5' = Node 4 5 (Node 2 2 (Node 0 1 Leaf Leaf) (Node 1 4 (Node 0 3 Leaf Leaf) Leaf)) (Node 3 7 (Node 0 6 Leaf Leaf) (Node 2 9 Leaf (Node 1 16 (Node 0 15 Leaf Leaf) Leaf)))

-- DELETE
-- delete 32 - no va
ej6 = Node 3 44 (Node 1 17 Leaf (Node 0 32 Leaf Leaf)) (Node 2 62 (Node 1 50 (Node 0 48 Leaf Leaf) (Node 0 54 Leaf Leaf)) (Node 1 78 Leaf (Node 0 88 Leaf Leaf)))
-- delete 30 - si
ej7 = Node 2 20 (Node 1 10 (Node 0 5 Leaf Leaf) (Node 0 15 Leaf Leaf)) (Node 0 30 Leaf Leaf)
-- delete 55 - si
ej8 = Node 3 50 (Node 2 40 (Node 1 30 (Node 0 10 Leaf Leaf) Leaf) (Node 0 45 Leaf Leaf)) (Node 1 60 (Node 0 55 Leaf Leaf) Leaf)
-- delete 60 - no y ademas hace cosas raras
ej9 = Node 2 50 (Node 1 40 Leaf (Node 0 45 Leaf Leaf)) (Node 0 60 Leaf Leaf)
