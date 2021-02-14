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

