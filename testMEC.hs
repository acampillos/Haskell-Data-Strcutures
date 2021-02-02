import Data.List as L
--import DataStructures.BinarySearchTree as BST
import DataStructures.RedBlackTree as RBT
-- Zona de tests para marespecue1


ejbrt1,ejbrt2,ejbrt3,ejbrt4,ejbrt5,ejbrt6,ejbrt7 :: RBTree Int
ejbrt1 = (N B 7 (N B 3 (L) (L)) (N R 18 (N B 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (L) (L)))))
ejbrt6 = foldr insertRBT (L) [6,3,18,1,4,14,22,9,16,20,26,8,10,15,17]
ejbrt7 = foldr insertRBT (L) [7,3,18,10,8,11,22,26]
ejbrt8 = foldr insertRBT (L) [1,2,4,5,3]
ejbrt9 = foldr insertRBT (L) [8,9,10,13,11]
ejbrt10 = (N B 6 ejbrt8 ejbrt9)
-- Ejemplos de RBT no valido
-- No tiene todos los caminos hasta las hojas con misma cantidad de negros
ejbrt2 = (N B 7 (N B 3 (L) (L)) (N R 18 (N B 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (N B 25 (L) (L)) (L)))))
-- La raiz es roja
ejbrt3 = (N R 7 (N B 3 (L) (L)) (N R 18 (N B 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (N B 25 (L) (L)) (L)))))
-- Un rojo tiene un hijo rojo
ejbrt4 = (N B 7 (N B 3 (L) (L)) (N R 18 (N R 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (N B 25 (L) (L)) (L)))))
-- No es BST
ejbrt5 = (N B 7 (N B 42 (L) (L)) (N R 18 (N R 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (N B 25 (L) (L)) (L)))))

--fuse (N R 2 (N B 1 (L) (L)) (N B 3 (L) (L))) (N B 5 (L) (L))
ejbrt11 = (N B 4 (N B 1 L (N R 3 L L)) (N B 8 (N R 6 L L) L))
ejbrt12 = (N B 15 (N B 10 L (N R 14 L L)) (N R 20 (N B 17 (N R 16 L L) (N R 18 L L)) (N B 26 (N R 22 L L) L)))