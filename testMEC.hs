import Data.List as L
import DataStructures.BinarySearchTree as BST
import DataStructures.RedBlackTree as RBT
-- Zona de tests para marespecue1


ejbrt1,ejbrt2,ejbrt3,ejbrt4,ejbrt5 :: RBTree Int
ejbrt1 = (B 7 (B 3 (L) (L)) (R 18 (B 10 (R 8 (L) (L)) (R 11 (L) (L))) (B 22 (L) (R 26 (L) (L)))))
-- Ejemplos de RBT no valido
-- No tiene todos los caminos hasta las hojas con misma cantidad de negros
ejbrt2 = (B 7 (B 3 (L) (L)) (R 18 (B 10 (R 8 (L) (L)) (R 11 (L) (L))) (B 22 (L) (R 26 (B 25 (L) (L)) (L)))))
-- La raiz es roja
ejbrt3 = (R 7 (B 3 (L) (L)) (R 18 (B 10 (R 8 (L) (L)) (R 11 (L) (L))) (B 22 (L) (R 26 (B 25 (L) (L)) (L)))))
-- Un rojo tiene un hijo rojo
ejbrt4 = (B 7 (B 3 (L) (L)) (R 18 (R 10 (R 8 (L) (L)) (R 11 (L) (L))) (B 22 (L) (R 26 (B 25 (L) (L)) (L)))))
-- No es BST
ejbrt5 = (B 7 (B 42 (L) (L)) (R 18 (R 10 (R 8 (L) (L)) (R 11 (L) (L))) (B 22 (L) (R 26 (B 25 (L) (L)) (L)))))

