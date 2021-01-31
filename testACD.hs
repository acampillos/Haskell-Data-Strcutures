import Data.List as L
import DataStructures.BinarySearchTree as BST


------------------------
-- TESTS
------------------------
test1, test2 :: BSTree Int
test1 = foldr BST.insert H [50,40,60,70,80]
test2 = undefined


{--
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