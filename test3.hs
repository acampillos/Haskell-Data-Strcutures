data Tree a
  = Leaf
  | Node
      Int
      (Tree a)
      a
      (Tree a)
  deriving (Show, Eq)

isLeaf :: Ord a => Tree a -> Bool
isLeaf Leaf = True
isLeaf (Node _ _ _ _) = False

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

{--max_ :: Ord a => Tree a -> a
--max_ Leaf = 0
max_ (Node _ _ v right) =
  case right of
    Leaf -> v
    _ -> max_ right--}
 
-- NO FUNCIONA
delete :: Ord a => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete x (Node h left v right)
  | x == v =
    maybe left (rotate . (Node h left <*> (`delete` right))) (max_ right)
  | x > v = rotate $ Node h left v (delete x right)
  | x < v = rotate $ Node h (delete x left) v right

{--
{--delete :: Ord a => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete x (Node h left v right)--}

delete' :: Ord a => a -> Tree a -> Tree a
delete' _ Leaf = Leaf
delete' x (Node h l v r)
    | v == x && isLeaf l && isLeaf r = Leaf
    | v == x && isLeaf l = r
    | v == x && isLeaf r = l
    | v == x = let maximoL = max_ l 
               in let n = (Node h  (delete' maximoL l) maximoL r)
                  in Node (height n) (delete' maximoL l) maximoL r 
    | v < x = Node h (delete' x l) v r
    | v > x = Node h l v (delete' x r)
-- Corregir con el caso de que a la izquierda no tenga nodos si no hojas
-- habria que obtener el minimo de la derecha y si este tampoco existe
-- poner una hoja (la prueba esta hecha como si existiese el max de l)--}
 
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
        padding n = replicate (n * 4) ' '

t1, t2 :: Tree Int
t1 = Node 3 (Node 2 (Node 1 (Node 0 Leaf 10 Leaf) 30 Leaf) 40 (Node 0 Leaf 45 Leaf)) 50 (Node 1 (Node 0 Leaf 55 Leaf) 60 Leaf)
-- delete 55
t2 = Node 2 (Node 1 (Node 0 Leaf 5 Leaf) 10 (Node 0 Leaf 15 Leaf)) 20 (Node 0 Leaf 30 Leaf)
-- delete 30
-- HACE QUE PETE