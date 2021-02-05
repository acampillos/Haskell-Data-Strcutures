-- empty
-- isEmpty

data Deque a = Deque Int [a] Int [a]
    deriving (Show)

empty :: Deque a
empty = Deque 0 [] 0 []

isEmpty :: Deque a -> Bool
isEmpty (Deque sf f sr r) = sf + sr == 0

c :: Int
c = 2

-- FUNCIONES FRENTE
-- cons     insert
-- head     inspect
-- tail     remove

cons :: Deque a -> a -> Deque a
cons (Deque sf f sr r) x = queue (Deque (sf+1) (x:f) sr r)

head :: Deque a -> a
head (Deque 0 [] 0 []) = error "Empty deque"
-- no se si r es solo un elemento [x] o (x:r), igual en el resto de casos
head (Deque 0 [] sr r) = Prelude.head r
head (Deque sf (x:f) _ _) = x

tail :: Deque a -> Deque a
tail (Deque 0 [] 0 []) = error "Empty deque"
tail (Deque 0 [] sr [x]) = empty
tail (Deque sf f sr r) = queue (Deque (sf-1) (Prelude.tail f) sr r)

-- REAR ELEMENT
-- snoc     insert
-- last     inspect
-- init     remove

snoc :: Deque a -> a -> Deque a
snoc (Deque sf f sr r) x = queue (Deque sf f (sr+1) (x:r))

last :: Deque a -> a
last (Deque 0 [] 0 []) = error "Empty deque"
last (Deque sf [x] 0 []) = x
last (Deque _ _ sr (x:r)) = x

init :: Deque a -> Deque a
init (Deque 0 [] 0 []) = error "Empty queue"
init (Deque sf [x] 0 []) = empty
init (Deque sf f sr r) = queue (Deque sf f (sr-1) (Prelude.tail r))


queue :: Deque a -> Deque a
queue q@(Deque sf f sr r)
    | sf > c*sr + 1 = 
        let f' = take i f
            r' = r ++ reverse (drop i f)
        in Deque i f' j r'
    | sr > c*sf + 1 =
        let f' = f ++ reverse (drop i r)
            r' = take i r
        in Deque i f' j r'
    | otherwise = q
    where i = div (sf+sr) 2
          j = sf + sr - i

q1, q2, q3 :: Deque Int
q1 = Deque 3 [1,2,3] 2 [4,5]
q2 = Deque 7 [1,2,3,4,5,6,7] 1 [8]
q3 = undefined