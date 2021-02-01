import Data.List as L
--import DataStructures.BinarySearchTree as BST
import DataStructures.RedBlackTree as RBT
-- Zona de tests para marespecue1


ejbrt1,ejbrt2,ejbrt3,ejbrt4,ejbrt5 :: RBTree Int
ejbrt1 = (N B 7 (N B 3 (L) (L)) (N R 18 (N B 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (L) (L)))))
-- Ejemplos de RBT no valido
-- No tiene todos los caminos hasta las hojas con misma cantidad de negros
ejbrt2 = (N B 7 (N B 3 (L) (L)) (N R 18 (N B 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (N B 25 (L) (L)) (L)))))
-- La raiz es roja
ejbrt3 = (N R 7 (N B 3 (L) (L)) (N R 18 (N B 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (N B 25 (L) (L)) (L)))))
-- Un rojo tiene un hijo rojo
ejbrt4 = (N B 7 (N B 3 (L) (L)) (N R 18 (N R 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (N B 25 (L) (L)) (L)))))
-- No es BST
ejbrt5 = (N B 7 (N B 42 (L) (L)) (N R 18 (N R 10 (N R 8 (L) (L)) (N R 11 (L) (L))) (N B 22 (L) (N R 26 (N B 25 (L) (L)) (L)))))

