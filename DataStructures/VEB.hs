module DataStructures.VEB 
    (   VEBTree
      ,   isqrt
      ,   upPowerOfTwo
      ,   empty
      ,   maybeOr
      ,   isMinOrMax
--    ,   member
--    ,   insert
--    ,   successor
--    ,   delete
--    ,   u
--    ,   fromList
    ) where

import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>), (<*>) )
import Control.Monad ((=<<))
import qualified Data.Vector as V (Vector, replicate, (!), (//)) 
import Test.QuickCheck

-- El caso base es que haya solo dos elementos, donde uno será el minimo y el otro el maximo,
-- En otro caso se está en un nodo 

--initially assume that u is a power of 2
-- num elementos, minimo, maximo, summary, cluster
-- el numero de elementos sera el siguiente cuadrado del numero
data VEBTree = VEBNode Int (Maybe Int) (Maybe Int) VEBTree (V.Vector VEBTree)
    -- for the VEB base, u=2
    | VEBBase (Maybe Int) (Maybe Int)
    deriving (Show, Eq)

{-
instance Arbitrary VEBTree where
    arbitrary = fromList <$> (arbitrary :: Gen [Int])
-}
-- redondeando la raiz cuadrada
isqrt :: Int -> Int
isqrt n = round $ sqrt $ fromIntegral n

-- Pasar al siguiente cuadrado
-- upPowerOfTwo 8 = 8
-- mayor que 8 y menor que 16 = 16
-- upPowerOfTwo 16 = 16
-- para mayor de 16 y <= 32 = 32
upPowerOfTwo :: Int -> Int
upPowerOfTwo x | x <= 0 = 0
upPowerOfTwo x = (^) 2  . ceiling . logBase 2 . fromIntegral $ x

-- dar un vEB vacio con el tamaño especificado, si es de tamaño 2 son dos min y max vacios
-- si es de mayor tamaño, se hace del tamaño del siguiente cuadrado, con un summary vacio de tamaño la raiz
-- cuadrada redondeada y un vector de vEB vacios del tamaño del summary (el vector y el vEB)
-- vector de vEB de
empty :: Int -> VEBTree
empty 2 =  VEBBase Nothing Nothing
empty a = VEBNode (upPowerOfTwo a) Nothing Nothing (empty a') (V.replicate a' (empty a'))
    where a' = isqrt (upPowerOfTwo a) 

getMin :: VEBTree -> Maybe Int
getMin (VEBBase min _) = min
getMin (VEBNode _ min _ _ _) = min

getMax :: VEBTree -> Maybe Int
getMax (VEBBase _ max) = max
getMax (VEBNode _ _ max _ _) = max

{-
-- Si es lista vacia, se hace un vEB de tamaño 4
-- Sino se insertan ...
fromList :: [Int] -> VEBTree
fromList [] = empty 4
fromList x = foldr insert (empty size) x
    where size = max 4 (1+ maximum x) 
-}

-- or para Maybe, es True si alguno de los dos es Just True
maybeOr :: Maybe Bool -> Maybe Bool -> Bool
maybeOr x y = (fromMaybe False x) || (fromMaybe False y)

-- esta el min o max a en el arbol tree,
isMinOrMax :: Int -> VEBTree -> Bool
isMinOrMax a tree
    | isNothing (getMin tree) && isNothing (getMax tree) = False
    | isNothing (getMin tree) = Nothing `maybeOr` (max == a)
    | isNothing (getMax tree) = (min == a) `maybeOr` Nothing
    | otherwise = (a == min) `maybeOr` (a == max) 
    where
        Just min = getMin tree
        Just max = getMax tree


high :: VEBTree -> Int -> Int
high tree x = x `quot` isqrt (u tree)  

{-
low :: VEBTree -> Int -> Int
low tree x = x `mod` isqrt (u tree)

index :: VEBTree -> Int -> Int -> Int
index tree x y = x * (isqrt (u tree)) + y

member :: Int -> VEBTree -> Bool
member a tree | a > maxNum tree || a < 0 = False
member a tree@(VEBBase _ _)= isMinOrMax a tree  
member a tree@(VEBNode _ _ _ _ _)= isMinOrMax a tree 
        || member (low tree a) ((clusters tree) V.! (high tree a))  

maxNum :: VEBTree -> Int
maxNum (VEBBase _ _) = 1
maxNum (VEBNode u _ _ _ _) = u-1

minEqualsMax :: VEBTree -> Bool
minEqualsMax tree = v_min tree == v_max tree

insert :: Int -> VEBTree -> VEBTree 
-- cannot insert elements outside of the key range [0,u)
insert a tree | a > maxNum tree || a < 0 = tree
-- Base case -- initialize the node
insert a (VEBNode u Nothing Nothing summary clusters) = (VEBNode u (Just a) (Just a)  summary clusters)
insert a (VEBBase Nothing Nothing) = VEBBase (Just a) (Just a)
-- The next two cases are about replacing min if a <min or max if a>max
insert a (VEBBase (Just val_min) (Just val_max)) 
    | a < val_max = VEBBase (Just a) (Just val_max)
    | a > val_max = VEBBase (Just val_min) (Just a) 
insert a (VEBNode u (Just val_min) (Just val_max) summary clusters)
    | a < val_min = insert val_min (VEBNode u (Just a) (Just val_max) summary clusters)
    | a > val_max = insert val_max (VEBNode u (Just val_min) (Just a) summary clusters)
-- insert the node deeper into the tree if minimum and maximum do not have to be replaced
insert a tree@(VEBNode u maybe_min maybe_max summary clusters) =  
    let new_node = insert (low tree a) (clusters V.! (high tree a)) 
        new_clusters = clusters V.// [(high tree a, new_node)] 
     in case v_min $ clusters V.! (high tree a) of
        Nothing -> VEBNode u maybe_min maybe_max (insert (high tree a) summary) new_clusters    
        _ -> VEBNode u maybe_min maybe_max summary new_clusters 

successor :: Int -> VEBTree -> Maybe Int
successor a (VEBBase Nothing _) = Nothing
successor a (VEBBase (Just 0) (Just 1)) = Just 1
successor a (VEBBase _ _ ) = Nothing
successor a (VEBNode  _ (Just val_min) _ _ _) | a < val_min = return val_min
successor a tree@(VEBNode _ _ _ summary clusters ) = 
    case v_max $ clusters V.! (high tree a) of
        Just max_low | low tree a < max_low ->
            let offset = successor (low tree a) (clusters V.! (high tree a))
             in index tree (high tree a) <$> offset
        _ ->
            case successor (high tree a) summary of
                Nothing -> Nothing
                Just succ_cluster ->
                    let offset = v_min $ clusters V.! succ_cluster
                    in index tree succ_cluster <$> offset

-- TODO: check if the element exists before trying to delete it. This code assumes the element already exists
delete :: Int -> VEBTree -> VEBTree
delete _ tree@(VEBNode u maybe_min maybe_max summary clusters) 
   | minEqualsMax tree = VEBNode u Nothing Nothing summary clusters
delete _ tree@(VEBBase maybe_min maybe_max) 
    | minEqualsMax tree = VEBBase Nothing Nothing
delete 0 (VEBBase _ (Just 1)) = VEBBase (Just 1) (Just 1)
delete 1 (VEBBase (Just 0) _ ) = VEBBase (Just 0) (Just 0)
delete a tree@(VEBNode u (Just val_min) maybe_max summary clusters)
    | a == val_min =
        let first_cluster = v_min summary
            next_min = (index tree) <$> first_cluster <*> ( v_min =<< (((V.!) clusters) <$> first_cluster))
            new_cluster = delete (low tree a) (clusters V.!  (high tree a))
            new_clusters = clusters V.// [(high tree a, new_cluster)]
         in delete' a (VEBNode u next_min maybe_max summary  new_clusters)
    | otherwise = 
        let new_cluster = delete (low tree a) (clusters V.!  (high tree a))
            new_clusters = clusters V.// [(high tree a, new_cluster)]
         in delete' a (VEBNode u (Just val_min) maybe_max summary new_clusters)

delete' a tree@(VEBNode u maybe_min maybe_max summary clusters)
    | (v_min $ clusters V.! (high tree a)) == Nothing = 
        let new_summary = delete (high tree a) summary
         in if (Just a) == maybe_max 
                then case v_max new_summary of
                    Nothing -> VEBNode u maybe_min maybe_min new_summary clusters
                    Just summary_max -> VEBNode u maybe_min (index tree summary_max <$> (v_min (clusters V.! summary_max))) new_summary clusters
                else VEBNode u maybe_min maybe_max new_summary clusters
    | Just a == maybe_max = VEBNode u maybe_min (index tree (high tree a)  <$> (v_max $ clusters V.! (high tree a))) summary clusters
    | otherwise = tree
-}