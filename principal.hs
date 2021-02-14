import Data.Char
import Text.Printf
import System.IO
import System.Directory

import Data.List as L
import DataStructures.BinarySearchTree as BST
import Data.Set as Set
import DataStructures.Graph as G
import DataStructures.DirectedGraph as DG
import DataStructures.RedBlackTree as RBT
import DataStructures.Deque as D
import DataStructures.HashTable.HashTableSChaining as HTS
import DataStructures.HashTable.HashTableLProbing as HTL
import DataStructures.HashTable.HashTableQProbing as HTQ
import DataStructures.MinHeap as MinHeap
import DataStructures.MaxHeap as MaxHeap
import DataStructures.SplayTree as SplayTree

{-import System.Console.ANSI

limpiar = clearScreen-}
import System.Process

{-
returnMain = system ":m"
enterInfo = system ":m + System.Info"
lookingForOs = system "let actualOs = os"
cleanerForOs = system "if actualOs == \"mingw32\" then let limpiar = system \"cls\" else if actualOs == \"linux\" then let limpiar = system \"clear\" else let limpiar = putStr \"\ESC[2J\""

--if actualOs == "mingw32" then let limpiar = system "cls" else if actualOs == "linux" then let limpiar = system "clear" else let limpiar = putStr "\ESC[2J"


--limpiar = system "cls"
limpiar = system "limpiar"
-}

limpiar = system "cls"

--limpiar = system "clear"

-- EJEMPLOS

-- EJEMPLOS GRAFOS ######################################################################################
-- Carreteras minimas
carreteras :: Graph String
carreteras = G.fromTupleL ([(G.V "Sevilla"),(G.V "Huelva"),(G.V "Cadiz"),(G.V "Malaga"),(G.V "Granada"),(G.V "Almeria"),(G.V "Cordoba"),(G.V "Jaen")],
    [(G.P 92.8 (G.V "Sevilla") (G.V "Huelva")), (G.P 121.0 (G.V "Sevilla") (G.V "Cadiz")), (G.P 214.0 (G.V "Sevilla") (G.V "Malaga")),
        (G.P 141.0 (G.V "Sevilla") (G.V "Cordoba")), (G.P 234.0 (G.V "Cadiz") (G.V "Malaga")),(G.P 160.0 (G.V "Cordoba") (G.V "Malaga")),
            (G.P 108.0  (G.V "Cordoba") (G.V "Jaen")), (G.P 127.0 (G.V "Malaga") (G.V "Granada")),(G.P 93.8 (G.V "Jaen") (G.V "Granada")),
                (G.P 167.0 (G.V "Granada") (G.V "Almeria"))])
carreterasStringFH :: String
carreterasStringFH = "carreteras :: Graph String\ncarreteras = fromTuple ([(V \"Sevilla\"),(V \"Huelva\"),(V \"Cadiz\")\n,(V \"Malaga\"),(V \"Granada\"),(V \"Almeria\"),(V \"Cordoba\"),(V \"Jaen\")],\n[(P 92.8 (V \"Sevilla\") (V \"Huelva\")), (P 121.0 (V \"Sevilla\") (V \"Cadiz\")), (P 214.0 (V \"Sevilla\") (V \"Malaga\")),\n        (P 141.0 (V \"Sevilla\") (V \"Cordoba\")), (P 234.0 (V \"Cadiz\") (V \"Malaga\")),(P 160.0 (V \"Cordoba\") (V \"Malaga\")),\n            (P 108.0  (V \"Cordoba\") (V \"Jaen\")), (P 127.0 (V \"Malaga\") (V \"Granada\")),(P 93.8 (V \"Jaen\") (V \"Granada\")),\n                (P 167.0 (V \"Granada\") (V \"Almeria\"))])"

{-
        92.8                  141.0                 108
    +-------------- Sevilla ---------- Cordoba ----------- Jaen
    |                   | \               |                 |
 Huelva             121 |  \  214         | 160             | 93.8
                        |   -----\        |                 |
                        |         \____   |         127     |           167
                      Cadiz ---------- Malaga ----------- Granada ---------------- Almeria
                                234
-}
carreterasString :: IO ()
carreterasString = putStrLn "         92.8                  141.0                 108\n    +-------------- Sevilla ---------- Cordoba ----------- Jaen\n    |                   | \\               |                 |\n Huelva             121 |  \\  214         | 160             | 93.8\n                        |   -----\\        |                 |\n                        |         \\____   |         127     |           167\n                      Cadiz ---------- Malaga ----------- Granada ---------------- Almeria\n                                234"

-- Tras aplicar kruskal
{-
        92.8                  141.0                 108
    +-------------- Sevilla ---------- Cordoba ----------- Jaen
    |                   |                                   |
 Huelva             121 |                                   | 93.8
                        |                                   |
                        |                           127     |           167
                      Cadiz            Malaga ----------- Granada ---------------- Almeria
                                
-}
carreterasKruskalString :: IO ()
carreterasKruskalString = putStrLn "        92.8                  141.0                 108\n    +-------------- Sevilla ---------- Cordoba ----------- Jaen\n    |                   |                                   |\n Huelva             121 |                                   | 93.8\n                        |                                   |\n                        |                           127     |           167\n                      Cadiz            Malaga ----------- Granada ---------------- Almeria"

-- Laberinto
grafo :: Graph Int
grafo = G.fromTupleL ([(G.V 1),(G.V 2),(G.V 3),(G.V 4),(G.V 5),(G.V 6),(G.V 7),(G.V 8),(G.V 9)],
    [(G.P 1 (G.V 1) (G.V 2)), (G.P 1 (G.V 1) (G.V 4)), (G.P 1 (G.V 2) (G.V 3)), (G.P 1 (G.V 2) (G.V 8)),
        (G.P 1 (G.V 4) (G.V 3)), (G.P 1 (G.V 4) (G.V 5)), (G.P 1 (G.V 5) (G.V 6)), (G.P 1 (G.V 3) (G.V 7)),
            (G.P 1 (G.V 7) (G.V 9))])

grafoStringFH :: String 
grafoStringFH = "grafo = fromTuple ([(V 1),(V 2),(V 3),(V 4),(V 5),(V 6),(V 7),(V 8),(V 9)],\n    [(P 1 (V 1) (V 2)), (P 1 (V 1) (V 4)), (P 1 (V 2) (V 3)), (P 1 (V 2) (V 8)),\n        (P 1 (V 4) (V 3)), (P 1 (V 4) (V 5)), (P 1 (V 5) (V 6)), (P 1 (V 3) (V 7)),\n            (P 1 (V 7) (V 9))])"
grafoString :: IO ()
grafoString = putStrLn "    1 --------- 2 --------- 8\n    |           |\n    |           |\n    4 --------- 3 --------- 7\n    |                       |\n    |                       |\n    5 --------- 6           9"

{-
Empieza en 1 y acaba en 9
    1 --------- 2 --------- 8
    |           |           
    |           |           
    4 --------- 3 --------- 7
    |                       |
    |                       |
    5 --------- 6           9

-}
solBFSGrafo,solDFSGrafo :: [G.Vertex Int]
solDFSGrafo = G.dfs grafo (G.V 1)
solBFSGrafo = G.bfs grafo (G.V 1)

-- EJEMPLOS BST #############################################################################################
ejbst1 :: BSTree Int
ejbst1 = (BST.N (BST.N (H) (H) 5) (BST.N (BST.N (BST.N (H) (H) 9) (BST.N (H) (H) 13) 12) (BST.N (H) (BST.N (H) (H) 23) 19) 15) 8)

ejbst1StrF :: IO ()
ejbst1StrF = putStrLn "ejbst1 = (BST.N (BST.N (H) (H) 5) (BST.N (BST.N (BST.N (H) (H) 9) (BST.N (H) (H) 13) 12) (BST.N (H) (BST.N (H) (H) 23) 19) 15) 8)"
ejbst1Str :: IO ()
ejbst1Str = putStrLn "            8\n          /   \\\n         5     15\n              /   \\\n             12     19\n           /   \\   /   \\\n          9    13  H     23\n        /   \\\n       H     H"
{-
            8
          /   \
         5     15
              /   \
             12     19
           /   \   /   \
          9    13  H     23
        /   \
       H     H
-}
ejbst1InsertStr :: IO ()
ejbst1InsertStr = putStrLn "            8\n          /   \\\n         5     15\n              /   \\\n             12     19\n           /   \\   /   \\\n          9    13  H     23\n        /   \\           /   \\\n       H     H         H    2000\n                            /   \\\n                           H     H"
{-
            8
          /   \
         5     15
              /   \
             12     19
           /   \   /   \
          9    13  H     23
        /   \           /   \
       H     H         H    2000
                            /   \
                           H     H
-}
ejbst1DeleteStr :: IO ()
ejbst1DeleteStr = putStrLn "            8\n          /   \\\n         5     15\n              /   \\\n             12     23\n           /   \\   /   \\\n          9    13  H    H\n        /   \\\n       H     H"
{-
            8
          /   \
         5     15
              /   \
             12     23
           /   \   /   \
          9    13  H    H
        /   \
       H     H
-}

-- EJEMPLO RBT #######################################################################################
ejrbt1 :: RBTree Int
ejrbt1 = (RBT.N B 7 (RBT.N B 3 (L) (L)) (RBT.N R 18 (RBT.N B 10 (RBT.N R 8 (L) (L)) (RBT.N R 11 (L) (L))) (RBT.N B 22 (L) (RBT.N R 26 (L) (L)))))
ejrbt1StrF :: IO ()
ejrbt1StrF = putStrLn "ejrbt1 = (RBT.N B 7 (RBT.N B 3 (L) (L)) (RBT.N R 18 (RBT.N B 10 (RBT.N R 8 (L) (L)) (RBT.N R 11 (L) (L))) (RBT.N B 22 (L) (RBT.N R 26 (L) (L)))))"
ejrbt1Str :: IO ()
ejrbt1Str = putStrLn "             B7\n           /    \\\n        B3       R18 \n       / \\      /    \\\n      L   L   B10     B22\n             /  \\     /   \\\n           R8   R11  L    R26\n           / \\            / \\\n          L   L           L  L"
{-
             B7
           /    \
        B3       R18 
       / \      /    \
      L   L   B10     B22
             /  \     /   \
           R8   R11  L    R26
           / \            / \
          L   L           L  L
-}
ejrbt1InsertStr :: IO ()
ejrbt1InsertStr = putStrLn "             B18\n           /     \\\n        B7        R26 \n       /  \\       /  \\\n      B3  B10   B22   B200\n          /  \\\n         R8  R11"
{-
             B18
           /     \
        B7        R26 
       /  \       /  \
      B3  B10   B22   B200
          /  \
         R8  R11
-}
ejrbt1DeleteStr :: IO ()
ejrbt1DeleteStr = putStrLn "             B18\n           /     \\\n        R8        B22 \n       /  \\       /  \\\n      B3  B10     L   R26\n          /  \\\n         L   R11\n            /  \\\n            L   L"
{-
             B18
           /     \
        R8        B22 
       /  \       /  \
      B3  B10     L   R26
          /  \
         L   R11
            /  \
            L   L
-}

-- MENU DE ARBOL DE BUSQUEDA BINARIA -----------------------------------------------------
{-
data BSTree a = N (BSTree a) (BSTree a) a
              | H
              deriving (Show, Eq)
-}
bstStr :: IO ()
bstStr = putStrLn "data BSTree a = N (BSTree a) (BSTree a) a\n              | H\n              deriving (Show, Eq)"

menuBST :: IO ()
menuBST = do
    limpiar
    putStrLn "Menú Arbol de Búsqueda Binaria:\n"
    putStrLn ""
    putStrLn "Su estructura es la siguiente:"
    bstStr
    ejbst1StrF
    putStrLn ""
    putStrLn "Y su representación gráfica sería:"
    ejbst1Str
    putStrLn ""
    putStrLn "Un arbol de busqueda binaria es un arbol binario cuyos elementos"
    putStrLn "se encuentran ordenados con el siguiente criterio, si un"
    putStrLn "elemento es menor que el nodo actual, este elemento se encontrará"
    putStrLn "en la rama izquierda del nodo actual, en caso de ser mayor"
    putStrLn "se encontrará a la derecha."
    putStrLn ""
    putStrLn "Algunas pruebas que podemos hacer con las funciones definidas para"
    putStrLn "los BST son:"
    putStrLn ""
    putStrLn "1. Funciones de Insert, Delete y Search."
    putStrLn ""
    putStrLn "2. Recorridos del arbol."
    putStrLn ""
    putStrLn "Escribe el número de la prueba a la que quieres entrar."
    
    o <- getLine

    if o == "1" then do
        menuFuncionesBST
    else if o == "2" then do
        menuRecorridoBST
    else do
        putChar '\n'
        putStrLn "No se ha seleccionado ninguna opción válida."
    return ()

-- Submenus de BST -------------------
menuFuncionesBST :: IO ()
menuFuncionesBST = do
    limpiar
    putStrLn "Ejemplo:"
    putStrLn ""
    ejbst1Str
    putStrLn ""
    putStrLn "Tenemos 3 funciones principales en un BST, de búsqueda,"
    putStrLn "de inserción y de borrado, en esta implementación se"
    putStrLn "encuentran dos funciones de busqueda, una de ellas"
    putStrLn "con el nombre de \"depthOf\" que dado un elemento y BST"
    putStrLn "se nos retorna la profundidad en la que se encuentra el"
    putStrLn "elemento, y en caso de no encontrarse en el BST, se"
    putStrLn "retorna -1. La otra función es \"contains\" que tiene"
    putStrLn "las mismas entradas y retorna un Bool indicando si el"
    putStrLn "BST contiene el elemento dado."
    putStrLn ""
    putStrLn "Dado el BST de ejemplo se quiere buscar el elemento \"19\","
    putStrLn "para ejecutar las dos funciones que hemos nombrado antes"
    putStrLn "escribe un carácter."

    o <- getLine

    putStrLn ""
    putStrLn "Salida de (depthOf 19 ejbst1):"
    putStrLn $ show $ depthOf 19 ejbst1
    putStrLn ""

    putStrLn ""
    putStrLn "Salida de (contains 19 ejbst1):"
    putStrLn $ show $ BST.contains 19 ejbst1
    putStrLn ""

    putStrLn "Escribe un carácter para continuar."

    o <- getLine

    limpiar
    putStrLn "Ahora podemos probar la función de insercion, está definida"
    putStrLn "como \"insert\", toma como parámetros de entrada un elemento"
    putStrLn "y un BST, añade al BST el elemento dado."
    putStrLn ""
    putStrLn "Para ejecutar la función \"insert 2000 ejbst1\", que deberia"
    putStrLn "insertar en nustro BST de prueba el elemento 2000."

    o <- getLine

    putStrLn "Salida de (BST.insert 2000 ejbst1)"
    putStrLn $ show $ BST.insert 2000 ejbst1
    putStrLn ""
    ejbst1InsertStr
    putStrLn ""
    putStrLn "Como se ve, se ha insertado el elemento 2000 en el extremo mas"
    putStrLn "derecho del arbol, ya que este es el elemento mayor del BST"
    putStrLn ""
    putStrLn "Escribe un carácter para continuar."

    o <- getLine

    limpiar
    putStrLn "Por último probamos la función de borrado a la que llamamos"
    putStrLn "\"delete\", también toma como valores de entrada el elemento"
    putStrLn "que queremos eliminar y el arbol del que lo queremos eliminar."
    putStrLn ""
    putStrLn "Escriber un carácter para ejecutar la siguiente función:"
    putStrLn "\"delete 19 ejbst1\""

    o <- getLine

    putStrLn "Salida de (BST.delete 19 ejbst1)"
    putStrLn $ show $ BST.delete 19 ejbst1
    putStrLn ""
    ejbst1DeleteStr
    putStrLn ""
    putStrLn "Como podemos ver el 19 ha sido eliminado y sustituido por el 23,"
    putStrLn "el procedimiento de borrado es:"
    putStrLn ""
    putStrLn "\t1) si tiene un hijo es sustituido por ese hijo"
    putStrLn "\t2) si tiene 0 hijos se sustituye por H"
    putStrLn "\t3) si tiene 2 hijos se sustituye por el menor hijo de la rama"
    putStrLn "\tderecha"

    o <- getLine

    limpiar
    putStrLn "Para volver al menú de BST escribe 1"
    putStrLn ""
    putStrLn "Para volver al menú principal escribe 2"
    putStrLn ""
    putStrLn "Para salir escribe q"

    o <- getLine

    if o == "1" then do
        menuBST
    else if o == "2" then do
        nuevoMenu
    else do
        putChar '\n'
    return ()

menuRecorridoBST :: IO ()
menuRecorridoBST = do
    limpiar
    putStrLn "Todos los árboles se pueden recorrer de las siguientes"
    putStrLn "tres formas, \"inorder\", \"postorder\" y \"preorder\"."
    putStrLn "En el caso de \"inorder\", el arbol se recorre de forma"
    putStrLn "que se toma primero los nodos de la izquierda del arbol"
    putStrLn "y se sigue progresivamente hacia la derecha."
    putStrLn ""
    putStrLn "En el caso de \"preorder\" se toma cada nodoque se"
    putStrLn "encuentra como el siguiente, de este modo el nodo raiz"
    putStrLn "queda el primero de todos y le siguen su nodo izquierdo"
    putStrLn "y su nodo derecho."
    putStrLn ""
    putStrLn "En el caso de \"postorder\" se hacer el recorrido inverso"
    putStrLn "del preorder de forma que el nodo raiz queda el último."
    putStrLn ""
    putStrLn ""
    putStrLn "Escribe un carácter para ver el ejemplo sobre el que se"
    putStrLn "realizarán los recorridos:"

    o <- getLine
    
    limpiar
    putStrLn ""
    ejbst1Str
    putStrLn ""
    putStrLn "Escribe un carácter para ver el recorrido inorder del arbol."
    putStrLn ""

    o <- getLine

    putStrLn "inorder ejbst1"
    putStrLn $ show $ inorder ejbst1
    putStrLn ""
    putStrLn "Escribe un carácter para ver el recorrido preorder del arbol."
    putStrLn ""

    o <- getLine

    putStrLn "preorder ejbst1"
    putStrLn $ show $ preorder ejbst1
    putStrLn ""
    putStrLn "Escribe un carácter para ver el recorrido postorder del arbol."
    putStrLn ""

    o <- getLine

    putStrLn "postorder ejbst1"
    putStrLn $ show $ postorder ejbst1
    putStrLn ""

    putStrLn "Para volver al menú de BST escribe 1"
    putStrLn ""
    putStrLn "Para volver al menú principal escribe 2"
    putStrLn ""
    putStrLn "Para salir escribe q"

    o <- getLine

    if o == "1" then do
        menuBST
    else if o == "2" then do
        nuevoMenu
    else do
        putChar '\n'
    return ()

st1,st2 :: SplayTree Int
st1 = SplayTree.Node 50 (SplayTree.Node 30 (SplayTree.Node 10 SplayTree.Leaf (SplayTree.Node 20 (SplayTree.Node 15 SplayTree.Leaf SplayTree.Leaf) SplayTree.Leaf)) (SplayTree.Node 40 SplayTree.Leaf SplayTree.Leaf)) (SplayTree.Node 60 SplayTree.Leaf (SplayTree.Node 90 (SplayTree.Node 70 SplayTree.Leaf SplayTree.Leaf) (SplayTree.Node 100 SplayTree.Leaf SplayTree.Leaf)))
st2 = SplayTree.Node 80 (SplayTree.Node 60 (SplayTree.Node 50 (SplayTree.Node 30 (SplayTree.Node 10 SplayTree.Leaf (SplayTree.Node 20 (SplayTree.Node 15 SplayTree.Leaf SplayTree.Leaf) SplayTree.Leaf)) (SplayTree.Node 40 SplayTree.Leaf SplayTree.Leaf)) SplayTree.Leaf) (SplayTree.Node 70 SplayTree.Leaf SplayTree.Leaf)) (SplayTree.Node 90 SplayTree.Leaf (SplayTree.Node 100 SplayTree.Leaf SplayTree.Leaf))

menuSplayTree :: IO ()
menuSplayTree = do 
    limpiar
    putStrLn "Menú Splay Tree:\n"
    putStrLn "Árbol binario de búsqueda que permite acceder de forma más eficiente a nodos recién utilizados."
    putStrLn "Hace que sus operaciones en un complejidad amortizada de O(logn)."
    putStrLn "Se consigue mediante la operación de splay que reordena el árbol mediante rotaciones de forma que"
    putStrLn "el elemento sobre el que se hace el splay se sitúa en la raíz del árbol."

    putStrLn "Cuando se realiza una operación en el árbol biselado se aplica la operación de splay sobre el nodo"
    putStrLn "afectado, de forma que en las operacioens de búsqueda, inserción o eliminación el nodo x buscado,"
    putStrLn "añadido o su padre en el caso de la eliminación pasarán a ser la raíz del nuevo árbol. Además, "
    putStrLn "debe mantener las propiedades de un BST ya que se trata de una variación de este que se auto-balancea.\n"

    putStrLn "Sea nuestro árbol de prueba: "
    print st1
    putStrLn " \n        50\n        /  \\\n      30    60\n     /   \\    \\\n   10    40    90\n     \\        /  \\\n     20      70  100\n    /\n   15\n"
{-
         50
        /  \
      30    60
     /   \    \
   10    40    90
     \        /  \
     20      70  100
    /
   15
-}
    o <- getLine
    limpiar

    putStrLn "\nProcedemos a aplicar distintas operaciones sobre el mismo:\n"

    putStrLn "search 80 st1"
    print (search 80 st1)
    putStrLn "\n         70\n        /  \\\n      50    90\n     /   \\    \\\n   30    60    100\n  /  \\\n10   40\n  \\\n  20\n  /\n15\n"
{-
         70
        /  \
      50    90
     /   \    \
   30    60    100
  /  \
10   40
  \
  20
  /
15
-}

    putStrLn "Al no encontrar el nodo en el árbol, se realiza la operación de splay sobre el que sería su padre.\n"

    o <- getLine
    limpiar

    putStrLn "insert 80 st1"
    print (SplayTree.insert 80 st1)
    putStrLn "\n             80\n            /  \\\n          60    90\n         /   \\    \\\n       50    70    100\n      /\n    30\n   /  \\\n  10   40\n   \\\n    20\n   /\n  15\n"
{-
             80
            /  \
          60    90
         /   \    \
       50    70    100
      /
    30
   /  \
  10   40
   \
    20
   /
  15
-}

    putStrLn "Se hace la inserción como si de un BST se tratase y posteriormente se aplica splay sobre el nodo añadido."

    putStr "Llamemos a este árbol st2\nst2 = "
    print st2

    o <- getLine
    limpiar

    putStrLn "\ndelete 30 st2"
    print (SplayTree.delete 30 st2)
    putStrLn "\n         50\n        /  \\\n      20    60\n     /   \\    \\\n   10    40    80\n     \\        /  \\\n      15     70   90\n                    \\\n                    100\n"

    putStrLn "Podemos apreciar como se produce la eliminación del nodo 30, pasando a tomar su lugar"
    putStrLn "el nodo 20 (decidimos tomar el mayor del hijo izquierdo para seguir el recorrido inorder)"
    putStrLn "y se realiza la operación de splay sobre el nodo 50 (padre del nodo a eliminar)."
{-
         50
        /  \
      20    60
     /   \    \
   10    40    80
     \        /  \
      15     70   90
                    \
                    100
-}

    putStr "\nEscriba 'y' si desea volver al menú principal:"
    o <- getLine

   -- NO FUNCIONA NO SE PORQUE
    if o=="y" then
        nuevoMenu
    else do
        putStrLn "\nNo se ha seleccionado ninguna opción válida."
    return ()

--------------------------------------
menuMaxMinHeap :: IO ()
menuMaxMinHeap = do
    limpiar
    putStrLn "Menú Max/Min heap:\n\n"
    putStrLn "Son árboles binarios completos que cumplen una propiedad en función"
    putStrLn "del tipo de montículo que sean: que para cada padre en el árbol sus hijos"
    putStrLn "tienen un valor mayor que el de este o menor en el caso de ser uno mínimo.\n"
    putStrLn "Las aplicaciones de ambas estructuras son similares aunque la que se utiliza"
    putStrLn "es la min heap con ya que implementa el ADT de una cola de prioridad.\n"

    putStrLn "Podemos utilizar como ejemplo la implementación del algoritmo de Dijkstra de"
    putStrLn "una forma más eficiente por la obtención del camino con menor peso para cada nodo."
    putStrLn "Así, probamos en el conjunto de aristas que conforman el grafo formado por las provincias"
    putStrLn "de Andalucía. Pongamos que partimos de Sevilla, almacenamos todas las aristas salientes.\n"

    let caminosSevilla = G.edgesFrom carreteras (G.V "Sevilla")
    let minHeap = L.foldr (\p ac -> MinHeap.insert ac p) MinHeap.empty caminosSevilla
    print (MinHeap.elements minHeap)

    putStrLn "\nA fin de cuentas permite la ordenación de un conjunto de datos mediante operaciones"
    putStrLn "más eficientemente que otras estructuras más simples.\n"

    putStrLn "Otro ejemplo sería la obtención de la tarea más urgente de un conjunto de tareas a realizar."
    putStrLn "Sea el conjunto de tareas:\n"

    putStrLn "\t{(225, Fix bug 2), (150, Deploy model), (90, Refactor Controller.py), (110, Render index.html)}"

    let maxHeap = L.foldr (\x ac -> MaxHeap.insert ac x) MaxHeap.empty [(225, "Fix bug 2"), (150, "Deploy model"), (90, "Refactor Controller.py"), (110, "Render index.html")]

    putStrLn "\nEn un sistema de tickets de trabajo se podrían repartir dichas tareas en función de su urgencia,"
    putStr "siendo la primera de ellas en este caso: "

    let max_ = MaxHeap.findMax maxHeap
    print max_

    putStr "\nEscriba 'y' si desea volver al menú principal:"
    o <- getLine

    if o=="y" then
        nuevoMenu
    else do
        putStrLn "\nNo se ha seleccionado ninguna opción válida."
    return ()


-- MENU DE GRAFO ----------------------------------------------------------------------------
strDefGrafo :: IO ()
strDefGrafo = putStrLn "type Graph a = (Set (Vertex a), Set (Edge a))\n\ndata Vertex a = V a\n    deriving (Show, Eq, Ord)\n\n--                w        src       dst\ndata Edge a = P Float (Vertex a) (Vertex a)\n    deriving (Show, Eq,Ord)"

menuGrafo :: IO ()
menuGrafo = do
    limpiar
    putStrLn "Menú de grafos:\n"
    putStrLn ""
    putStrLn "Los grafos tienen la siguiente estructura:"
    strDefGrafo
    putStrLn ""
    putStrLn "Para inicializar un grafo se aconseja el uso de las funciones"
    putStrLn "\"fromTupleL\" (que crea un grafo a partir de la tupla"
    putStrLn "[Vertex a], [Edge a]) y \"fromTupleS\" (que crea un grafo a"
    putStrLn "partir de la tupla (Set (Vertex a), Set (Edge a))). Esto es"
    putStrLn "de este modo para evitar posibles errores al añadir aristas"
    putStrLn "que estan conectadas a vertices que no pertenecen al grafo."
    putStrLn ""
    putStrLn "Algunas pruebas que podemos realizar con las funciones creadas: "
    putStrLn ""
    putStrLn "1. Tenemos un grafo que representa la distribución de carreteras"
    putStrLn "en un mapa, los pesos de las aristas son los km de carretera que"
    putStrLn "separan los vertices que son ciudades"
    putStrLn ""
    putStrLn "2. Tenemos un grafo, vemos como se recorre el grafo por dfs y bfs"
    putStrLn ""
    putStrLn "3. Podemos hacer pruebas con la conectividad."
    putStrLn ""
    putStrLn "4. Hay grafos dirigidos."
    putStrLn ""
    putStrLn "Escribe el número de la prueba a la que se quiere entrar."

    o <- getLine

    if o == "1" then do
        menuCosteMinimo
    else if o == "2" then do
        menuRecorrido
    else if o == "3" then do
        menuConectividad
    else if o == "4" then do
        menuGrafoDirigido
    else do
        putChar '\n'
        putStrLn "No se ha seleccionado ninguna opción válida."
    return ()

----Submenus grafos ---------
menuGrafoDirigido :: IO ()
menuGrafoDirigido = do
    limpiar

    putStrLn "La diferencia entre los grafos dirigidos y los no dirigidos"
    putStrLn "puede resumirse en la definición de adyacencia."
    putStrLn ""
    putStrLn "Los grafos NO dirigidos tienen por vertices adyacente todos"
    putStrLn "aquellos que comparten arista. En cambio, en los grafos"
    putStrLn "dirigidos, un vértice \'a\' tiene a otro \'b\' como adyacente,"
    putStrLn "cuando existe una arista que tiene como vértice fuente a \'a\'"
    putStrLn "y como vértice destino a \'b\'."
    putStrLn ""
    putStrLn "Para volver al menu de grafos escribe 1"
    putStrLn ""
    putStrLn "Para volver al menu de inicio escribe 2"
    putStrLn ""
    putStrLn "Para salir escribe q, escribe otra cosa para salir."
    putStrLn ""

    o <- getLine

    if o == "q" then do
        limpiar
        putStrLn "Saliendo..."
    else if o == "1" then do
        menuGrafo
    else if o == "2" then do
        nuevoMenu
    else do
        putChar '\n'
        putStrLn "No se ha seleccionado ninguna opción válida."

    return ()


menuCosteMinimo :: IO ()
menuCosteMinimo = do
    limpiar
    carreterasString
    putStrLn "Representado como: "
    putStrLn carreterasStringFH
    putStrLn ""
    putStrLn "Tenemos el mapa de Andalucía, un terremoto ha dejado inutilizables"
    putStrLn "las carreteras que unen las capitales, el coste de las reparaciones"
    putStrLn "es proporcional a los km de longitud. Para poder recuperar la"
    putStrLn "conectividad entre todas las ciudades se quieren reparar las"
    putStrLn "carreteras necesarias gastando lo minimo posible."
    putStrLn ""
    putStrLn "Para solucionar este problema usamos un arbol de expansión mínima,"
    putStrLn "consiste en un subrgrafo del grafo principal (en nuestro caso el"
    putStrLn "mapa de Andalucía), que contiene todos los vértices y solo las"
    putStrLn "aristas cuya suma de los pesos es mínima. (Tampoco hay ciclos ya"
    putStrLn "que es un arbol)."
    putStrLn ""
    putStrLn "Se ha implementado una funcion a la que llamamos \"kruskal\" que"
    putStrLn "encuentra ese arbol de expansión mínima, toma como parámetro de"
    putStrLn "entrada el grafo (nuestro grafo del mapa de Andalucía) y retorna"
    putStrLn "el arbol de expansión mínima, otro grafo."
    putStrLn ""
    putStrLn "Escribe 1 para ejecutar la siguiente instrucción:"
    putStrLn "kruskal carreteras"
    putStrLn ""
    putStrLn "Escribe 2 para volver al menú anterior."

    o <- getLine

    if o == "1" then do
        limpiar
        putStrLn "Salida aplicando kruskal"
        putStrLn (show (kruskal carreteras))
        putStrLn ""
        putStrLn "Como se ve en el conjunto de aristas, las carreteras"
        putStrLn "Sevilla - Malaga, Cadiz - Malaga y Cordoba - Malaga"
        putStrLn "ya no son parte del nuevo grafo."
        putStrLn ""
        putStrLn "El algoritmo de kruskal (comenzando con un grafo vacio" 
        putStrLn "que es el grafo de salida y el grafo de entrada)," 
        putStrLn "consiste en ordenar las aristas de menor a mayor"
        putStrLn "por peso, se añade la primera arista, se comprueba"
        putStrLn "si el número de componentes conexas despues de"
        putStrLn "añadir la nueva arista es menor que antes de añadirla,"
        putStrLn "en caso de no ser menor no se añade al grafo de salida."
        putStrLn ""
        putStrLn "Salida grafica:"
        carreterasKruskalString

        o <- getLine

        menuGrafo

    else if o == "2" then do
        menuGrafo
    else do
        menuGrafo
    return ()

menuRecorrido :: IO ()
menuRecorrido = do
    limpiar 
    grafoString
    putStrLn ""
    putStrLn "Grafo: "
    putStrLn ""
    putStrLn grafoStringFH
    putStrLn ""
    putStrLn "Se han implementado dos formas de recorrer el grafo,"
    putStrLn "recorrido en profundidad (dfs) y recorrido en anchura"
    putStrLn "(bfs), dfs trata de recorrer el grafo de modo que por"
    putStrLn "cada nodo, se trata de llegar a mayor profundidad" 
    putStrLn "(usando una pila), en cambio bfs recorre el grafo" 
    putStrLn "abarcando primero cada nodo adyacente al nodo en el" 
    putStrLn "que se encuentra actualmente (usando una cola)."
    putStrLn ""
    putStrLn "bfs y dfs toman como entrada el grafo y el vertice por"
    putStrLn "el que se empieza el recorrido, tomamos como ejemplo de"
    putStrLn "vertice de inicio el 1."
    putStrLn ""
    putStrLn "Para ver el ejemplo siguiente escribe 1:"
    putStrLn "solDFSGrafo = dfs grafo (V 1)"
    putStrLn "solBFSGrafo = bfs grafo (V 1)"
    putStrLn ""
    putStrLn "Para volver al menú anterior escribe 2:"

    o <- getLine

    if o == "1" then do
        menuSalidaBFSDFS

        -- Hacer que pueda probar con sus propios grafos

    else if o == "2" then do
        menuGrafo
    else do
        menuGrafo
    return ()

menuSalidaBFSDFS :: IO ()
menuSalidaBFSDFS = do
    limpiar
    grafoString
    putStrLn ""
    putStrLn "Salida dfs"
    putStrLn (show (G.dfs grafo (G.V 1)))
    putStrLn "Salida bfs"
    putStrLn (show (G.bfs grafo (G.V 1)))
    putStrLn ""
    putStrLn "Ambas listas representan de forma secuencial el vertice que ha"
    putStrLn "sido visitado en cada iteración (llamada recursiva) del algoritmo"
    putStrLn ""
    putStrLn "Para volver al menu de grafos escribe 1"
    putStrLn ""
    putStrLn "Para volver al menu de inicio escribe 2"
    putStrLn ""
    putStrLn "Para salir escribe q, escribe otra cosa para salir."
    putStrLn ""

    o <- getLine

    if o == "q" then do
        limpiar
        putStrLn "Saliendo..."
    else if o == "1" then do
        menuGrafo
    else if o == "2" then do
        nuevoMenu
    else do
        putChar '\n'
        putStrLn "No se ha seleccionado ninguna opción válida."
    return ()

menuConectividad :: IO ()
menuConectividad = do
    limpiar
    putStrLn "Se han implementado funciones que tratan la conectividad de los"
    putStrLn "grafos. Funciones como \"conexo\" y \"conectividad\", la primera"
    putStrLn "función que teiene como entrada el grafo, nos retorna un Bool que"
    putStrLn "indica si el grafo es conexo (si tiene contectividad 1), la segunda"
    putStrLn "función toma por entrada un grafo y retorna el número de componentes"
    putStrLn "conexas del grafo."
    putStrLn ""
    putStrLn "Para ver algunos ejemplos con las funciones de conectividad escribe 1"
    putStrLn ""
    putStrLn "Para vovler al menú anterior escribe 2."
    o <- getLine

    if o == "1" then do
        menuEjemplosConectividad
    else do
        menuGrafo

    return ()

--menuEjemplosConectividad = limpiar
menuEjemplosConectividad :: IO ()
menuEjemplosConectividad = do
    limpiar
    putStrLn "Podemos comprobar que el siguiente grafo de ejemplo es conexo"
    putStrLn "usando la función \"conexo\" si nos retorna True y usando la"
    putStrLn "función \"conectividad\" si nos retorna 1 (una componente"
    putStrLn "conexa:"
    putStrLn ""
    grafoString
    putStrLn ""
    putStrLn "Escribe un caracter para ejecutar \"G.conexo grafo\"."
    
    o <- getLine

    putStrLn "Salida:"
    putStrLn (show (G.conexo grafo))

    putStrLn ""
    putStrLn "Escribe un caracter para ejecutar \"conectividad grafo\""
    
    o <- getLine

    putStrLn ""
    putStrLn "Salida:"
    putStrLn (show (G.conectividad grafo))
    putStrLn ""
    putStrLn "Como vemos, el grafo tiene una componente conexa, lo cual nos"
    putStrLn "indica que en efecto el grafo es conexo."
    putStrLn ""
    putStrLn "Si añadimos un nuevo vértice sin conectarlo con ninguna arista"
    putStrLn "al resto del grafo, debemos tener dos componentes conexas."
    putStrLn ""
    putStrLn "Escribe un caracter para ejecutar las siguientes funciones"
    putStrLn "que añaden un nuevo vertice y que muestra el nuevo conjunto"
    putStrLn "de vértices del grafo, tras esto se mostrara la neuva salida"
    putStrLn "de llamar a las funciones \"conectividad\" y \"conexo\":"
    putStrLn "" 
    putStrLn "let grafo' = G.addVertex grafo (G.V 100)"
    putStrLn "vertexSet grafo'"

    o <- getLine

    putStrLn ""
    let grafo' = G.addVertex grafo (G.V 100)
    putStrLn (show (G.vertexSet grafo'))
    putStrLn ""
    putStrLn "G.conexo grafo'"
    putStrLn (show (G.conexo grafo'))
    putStrLn ""
    putStrLn "G.conectividad grafo'"
    putStrLn (show (G.conectividad grafo'))

    o <- getLine

    putStrLn ""
    putStrLn "Podemos ver también los conjuntos de adyacencias de un vertice."
    putStrLn "Por ejemplo si mostramos el conjunto de adyacencias del nuevo "
    putStrLn "vértice debe mostrarnos un conjunto vacio ya que no está conectado"
    putStrLn "a otros vértices mediante aristas."
    putStrLn ""
    putStrLn "Escribe un carácter para ejecutar las siguientes dos funciones y"
    putStrLn "comprobarlo."
    putStrLn ""

    putStrLn "G.adjacents grafo' (G.V 100)"
    putStrLn ""
    putStrLn "G.adjacents grafo' (G.V 1)"
    putStrLn ""

    o <- getLine

    putStrLn "Salida (adjacents grafo' (V 100)):"
    putStrLn (show (G.adjacents grafo' (G.V 100)))
    putStrLn ""
    putStrLn "Salida (adjacents grafo' (V 1)):"
    putStrLn (show (G.adjacents grafo' (G.V 1)))
    putStrLn ""
    putStrLn "Para volver al menu de grafos escribe 1."
    putStrLn "Para volver al menu principal escribe 2."
    putStrLn "Para salir escribe q"

    t <- getLine

    if t == "1" then do
        menuGrafo
    else if t == "2" then do
        nuevoMenu
    else do
        nuevoMenu   -- AQUI HAY QUE CAMBIAR ESTO

    return ()

------------------------------
-- MENU HASH TABLE -----------------------------------------------------------
hashTableF :: IO ()
hashTableF = putStrLn "HashTable = |pares contenidos| [indice bucket, [pares en el bucket]]\n\n\ndata HashTable a b = HashTable Int (Array Int [(a, b)])\n  deriving (Show, Eq)"

menuHashTable :: IO ()
menuHashTable = do
    limpiar
    putStrLn "Menú de HashTable"
    putStrLn ""
    -- Introducción
    putStrLn "Una tabla hash es una estructura de datos que"
    putStrLn "implementa el ADT de un array asociativo o diccionario."
    putStrLn "Utiliza una función de hash para calcular un índice o"
    putStrLn "código hash que identifica posición de la casilla o cubeta"
    putStrLn "(bucket o slot) donde se localiza el valor."
    putStrLn ""
    putStrLn "La estructura es la siguiente:"
    hashTableF
    putStrLn ""
    -- Tipos de hash table implementadas según el tratamiento de colisiones
    putStrLn "Idealmente, la función de hash asignará cada clave a un"
    putStrLn "único bucket pero la mayoría de hash tables emplean una"
    putStrLn "función de hash imperfecta que puede causar colisiones."
    putStrLn ""
    putStrLn "A continuación se haran pruebas de insert, delete y get"
    putStrLn "para los siguientes tipos de Hashing, ya que la única"
    putStrLn "diferencia está en el insert, debido a que al hacer el"
    putStrLn "hashing de cada elemento, los pares caen en indices"
    putStrLn "distintos."
    putStrLn ""
    putStrLn "(Sabiendo que la diferencia principal se enceuntra en el"
    putStrLn "tratamiento de colisiones durante el insert, solo la"
    putStrLn "versión de Separate Chaining contiene ejemplos de delete"
    putStrLn "y get)"
    putStrLn ""
    -- Ajuste del tamaño de la tabla (load factor)   
    putStrLn "\t0. Explicación de técnica para reajustar tamaño." 
    putStrLn "\t1. Separate chaining."
    putStrLn "\t2. Linear probing."
    putStrLn "\t3. Quadratic probing.\n"
    putStr "Elija una opción: "
    
    o <- getLine

    if o == "0" then do
        menuExplicacionReajuste
    else if o == "1" then do
        menuHTSeparateChaining
    else if o == "2" then do
        menuHTLinearProbing
    else if o == "3" then do
        menuHTQuadraticProbing
    else do
        putChar '\n'
        putStrLn "No se ha seleccionado ninguna opción válida."
    return ()

-- Submenus HT --------------------------------

menuExplicacionReajuste :: IO ()
menuExplicacionReajuste = do
    limpiar 
    putStrLn "Para todas las implementaciones de los distintos"
    putStrLn "métodos de tratamiento de colisiones utilizamos"
    putStrLn "una estrategia de reajuste del tamaño de las"
    putStrLn "tablas con el fin de optimizar el espacio ocupado"
    putStrLn "en la misma y disminuir las coincidencias de bucket."
    putStrLn ""
    putStrLn "Cuando realizamos la operación de put en la tabla, si"
    putStrLn "está ocupada a un 50% antes de la inserción reajustamos"
    putStrLn "la tabla para que duplique su tamaño. Tras ello," 
    putStrLn "insertamos el par correspondiente."
    putStrLn ""
    putStrLn "Cuando realizamos al operación de delete en la tabla,"
    putStrLn "tras la eliminación del par asociado a la clave que" 
    putStrLn "recibe reducimos el tamaño de la tabla a la mitad si"
    putStrLn "está llena menos de un 12'5%."
    putStrLn ""
    putStrLn "Los reajustes los realizamos a partir del factor de"
    putStrLn "carga de la tabla, determinado por el número de "
    putStrLn "entradas en la tabla partido de el número de buckets"
    putStrLn "(tamaño) de la misma."
    putStrLn ""
    putStrLn "Los valores que hemos dado para la reducción o aumento"
    putStrLn "son arbitrarios en nuestro caso y se pueden variar en"
    putStrLn "el código según se quiera."
    putStrLn ""
    putStrLn "Escribe un carácter para continuar."
    putStrLn ""

    o <- getLine

    limpiar
    putStrLn ""
    putStrLn "Tomamos como ejemplo la siguiente tabla:"
    putStrLn "let htsExperimento = L.foldr (HTS.put) (HTS.empty 7) [(50,\"M\"),(700,\"C\"),(76,\"D\"),(85,\"E\")]"
    putStrLn ""
    putStrLn "Que tiene el siguiente aspecto"
    putStrLn ""
    putStrLn $ HTS.printHT $ HTS.put (85,"E") htsExperimento
    putStrLn ""
    putStrLn "Si añadimos un elemento más vamos a poder comporbar"
    putStrLn "directamente el reescalado de la tabla."
    putStrLn ""
    putStrLn "Escribe un carácter para añadir un elemento."

    o <- getLine

    putStrLn ""
    putStrLn "Salida (put (20,\"G\") htsExperimento):"
    putStrLn $ show $ HTS.put (20,"G") (HTS.put (85,"E") htsExperimento)
    putStrLn ""
    putStrLn "Y tiene el siguiente aspecto:"
    putStrLn ""
    putStrLn $ HTS.printHT $ HTS.put (20,"G") (HTS.put (85,"E") htsExperimento)
    putStrLn ""
    putStrLn "Como se ve, la tabla ha sido reescalada."
    putStrLn ""
    putStrLn "Para volver al menu de HashTable escribe 1."
    putStrLn "Para volver al menu principal escribe 2."
    putStrLn "Para salir escribe q"

    t <- getLine

    if t == "1" then do
        menuHashTable
    else if t == "2" then do
        nuevoMenu
    else do
        putChar '\n'  
        putStrLn "Saliendo..."
    return ()

htsExperimento :: HashTableSC Int String
htsExperimento = L.foldr (HTS.put) (HTS.empty 7) [(50,"M"),(700,"C"),(76,"D")]

menuHTSeparateChaining :: IO ()
menuHTSeparateChaining = do
    limpiar
    putStrLn "En este método, cada cubo es independiente y en ellos se"
    putStrLn "almacenan un conjunto de pares clave-valor en una lista."
    putStrLn "En la mayoría de implementaciones cada bucket tendrá pocas"
    putStrLn "entradas si la función de hash es adecuada."
    putStrLn ""
    putStrLn "La complejidad de las operaciones es O(1) para la búsqueda"
    putStrLn "del bucket correspondiente y O(n) para la búsqueda en la"
    putStrLn "lista (en nuestro caso listas enlazadas)."
    putStrLn ""
    putStrLn "Para hacer las pruebas vamos a usar la siguiente HashTable."
    putStrLn "Escribe un carácter para continuar."

    o <- getLine

    putStrLn ""
    putStrLn "Salida (L.foldr (HTS.put) (HTS.empty 7) [(50,\"M\"),(700,\"C\"),(76,\"D\")]):"
    putStrLn $ show $ htsExperimento
    putStrLn ""
    putStrLn "Que representamos como:"
    putStrLn ""
    putStrLn (HTS.printHT (htsExperimento))
    putStrLn ""
    putStrLn "La primera prueba va a ser con la función de insert a la"
    putStrLn "que llamamos \"put\" y toma como valores de entrada un par"
    putStrLn "de clave y valor, y una tabla hash."
    putStrLn ""
    putStrLn "Escribe un carácter para continuar."

    o <- getLine

    limpiar
    putStrLn ""
    putStrLn "Vamos a insertar en la tabla anterior el par (85,\"E\") y"
    putStrLn "para ello:"
    putStrLn ""
    putStrLn "Escribe un carácter para ejecutar \"put (85,\"E\") htsExperimento\":"
    
    o <- getLine
    
    putStrLn ""
    putStrLn "Salida (put (85,\"E\") htsExperimento):"
    putStrLn $ show $ HTS.put (85,"E") htsExperimento
    putStrLn ""
    putStrLn "Salida (printHT $ put (85,\"E\") htsExperimento):"
    putStrLn $ HTS.printHT $ HTS.put (85,"E") htsExperimento
    putStrLn ""
    putStrLn "Como podemos ver la tabla, el nuevo valor ha coincidido al hacer el"
    putStrLn "hash con otro valor, por tanto en ese valor de hash, ahora tendremos"
    putStrLn "dos elementos."
    putStrLn ""
    putStrLn "Para continuar al siguiente ejemplo escribe un carácter."

    o <- getLine

    limpiar
    putStrLn ""
    putStrLn "También es posible eliminar valores de la tabla con la función "
    putStrLn "\"removeKey\" pasandole la clave a eliminar y la tabla de la que"
    putStrLn "se pretende eliminar."
    putStrLn ""
    putStrLn "Para ejecutar \"removeKey 85 htsExperimento\" escribe un"
    putStrLn "carácter."
    putStrLn ""
    putStrLn "Salida (removeKey 85 htsExperimento):"
    putStrLn $ show $ HTS.removeKey 85 $ HTS.put (85,"E") htsExperimento
    putStrLn ""
    putStrLn "Y este sería el aspecto de la tabla:"
    putStrLn ""
    putStrLn $ HTS.printHT $ HTS.removeKey 85 $ HTS.put (85,"E") htsExperimento
    putStrLn ""
    putStrLn "Escribe un carácter para continuar."

    o <- getLine

    limpiar
    putStrLn "Como última prueba podemos testear la función \"getValue\""
    putStrLn "que toma la clave del valor que se busca y la tabla en la"
    putStrLn "cual se busca y se retorna un Maybe valor."
    putStrLn ""
    putStrLn "Escribe un carácter para ejecutar \"getValue 700 htsExperimento\""
    putStrLn ""
    putStrLn "Salida (getValue 700 htsExperimento):"
    putStrLn $ show $ HTS.getValue 700 htsExperimento
    putStrLn ""
    putStrLn "Para volver al menu de HashTable escribe 1."
    putStrLn "Para volver al menu principal escribe 2."
    putStrLn "Para salir escribe q"

    t <- getLine

    if t == "1" then do
        menuHashTable
    else if t == "2" then do
        nuevoMenu
    else do
        putChar '\n'  
        putStrLn "Saliendo..."
    return ()
    
htlExperimento :: HashTableLP Int String
htlExperimento = L.foldr (HTL.put) (HTL.empty 7) [(50,"M"),(700,"C"),(76,"D")]

menuHTLinearProbing :: IO ()
menuHTLinearProbing = do
    limpiar
    putStrLn "Trata de evitar las colisiones entre elementos que se insertan a la tabla" 
    putStrLn "con posibles valores para la función de hash iguales probando primeramente la posición"
    putStrLn "determinada por la función de hash y si esta está ocupada vamos probando en"
    putStrLn "los siguientes buckets.\n"

    putStrLn "hash(x) % |tabla|, (hash(x)+1) % |tabla|, (hash(x)+2) % |tabla|, ...\n"

    putStrLn "Como hemos dicho en el menú anterior, en este apartado"
    putStrLn "solo se hara una prueba con el metodo de insert para"
    putStrLn "ver el tratamiento de la colisión."
    putStrLn ""
    putStrLn "El ejemplo sobre el que vamos a probar es el siguiente:"
    putStrLn ""
    putStrLn $ show $ htlExperimento
    putStrLn ""
    putStrLn "Con el siguiente aspecto de tabla:"
    putStrLn ""
    putStrLn (HTL.printHT (htlExperimento))
    putStrLn ""
    putStrLn "Ahora procedemos a insertar el elemento (85,\"E\") en"
    putStrLn "la tabla."
    putStrLn "Escribe un carácter para continuar:"

    o <- getLine

    putStrLn ""
    putStrLn "Salida (HTL.put (85,\"E\") htLExperimento):"
    putStrLn $ HTL.printHT $ HTL.put (85,"E") htlExperimento
    putStrLn ""

    putStrLn "Se puede observar que el elemento insertado toma la siguiente posición "
    putStrLn "disponible a partir de su hash original.\n"

    putStrLn "Para volver al menu de HashTable escribe 1."
    putStrLn "Para volver al menu principal escribe 2."
    putStrLn "Para salir escribe q"

    t <- getLine

    if t == "1" then do
        menuHashTable
    else if t == "2" then do
        nuevoMenu
    else do
        putChar '\n'  
        putStrLn "Saliendo..."
    return ()

htqExperimento :: HashTableQP Int String
htqExperimento = L.foldr (HTQ.put) (HTQ.empty 7) [(50,"M"),(76,"D"),(85,"E")]

menuHTQuadraticProbing :: IO ()
menuHTQuadraticProbing = do
    limpiar
    putStrLn "Trata de evitar las colisiones entre elementos que se insertan a la tabla con"
    putStrLn "posibles valores para la función de hash iguales probando primeramente la posición"
    putStrLn "determinada por la función de hash y si esta está ocupada vamos probando en"
    putStrLn "los siguientes (i^2)-ésimos buckets.\n"

    putStrLn "hash(x) % |tabla|, (hash(x)+1*1) % |tabla|, (hash(x)+2*2) % |tabla|, ...\n"

    putStrLn "El ejemplo sobre el que vamos a probar es el siguiente:\n"

    putStrLn $ show $ htqExperimento

    putStrLn "\nCon el siguiente aspecto de tabla:\n"

    putStrLn (HTQ.printHT (htqExperimento))

    putStrLn "\nAhora procedemos a insertar el elemento (92,\"Z\") en"
    putStrLn "la tabla."
    putStrLn "Escribe un carácter para continuar:"

    o <- getLine

    putStrLn ""
    putStrLn "Salida (HTQ.put (92,\"Z\") htqExperimento):"
    putStrLn $ HTQ.printHT $ HTQ.put (92,"Z") htqExperimento
    putStrLn ""

    putStrLn "Se puede observar que el elemento insertado toma la siguiente posición "
    putStrLn "disponible a partir de su hash original utilizando un incremento en el"
    putStrLn "en el calculo de la siguiente posición igual al cuadrado del índice.\n"

    putStrLn "Para volver al menu de HashTable escribe 1."
    putStrLn "Para volver al menu principal escribe 2."
    putStrLn "Para salir escribe q"

    t <- getLine

    if t == "1" then do
        menuHashTable
    else if t == "2" then do
        nuevoMenu
    else do
        putChar '\n'  
        putStrLn "Saliendo..."

    return ()
-------------------------------------------------

-- MENU DE DEQUE ---------------------------------------------------------------
dequeStr :: IO ()
dequeStr = putStrLn "data Deque a = Deque Int [a] Int [a]\n    deriving (Show)"

menuDeque :: IO ()
menuDeque = do
    limpiar
    putStrLn "Menu de Deque\n"
    putStrLn ""
    putStrLn "ADT que generaliza una cola, permitiendo la inserción y"
    putStrLn "eliminación de elementos del principio y final de la cola."
    putStrLn ""
    putStrLn "La estructura implementada es la siguiente:"
    putStrLn ""
    dequeStr
    putStrLn ""
    putStrLn "El balance perfecto se da cuando el conjunto de elementos"
    putStrLn "está dividido en partes iguales para las dos listas que "
    putStrLn "componen la deque."
    putStrLn ""
    putStrLn "Para mejorar la eficiencia, restauramos este balance mediante"
    putStrLn "un invariante que nos permite no tener que restaurarlo con"
    putStrLn "cada operación:"
    putStrLn ""
    putStrLn "          |F| <= c|R|+1 y |R| <= c|F|+1"
    putStrLn ""
    putStrLn "Podemos hacer las siguientes pruebas:"
    putStrLn ""
    putStrLn "1. Inicialización de una Deque a partir de una lista."
    putStrLn ""
    putStrLn "2. Vemos como se añaden y eliminan elementos en el inicio"
    putStrLn "   y el fin de la Deque."
    putStrLn ""
    putStrLn "3. Comprobando el rebalanceo."
    putStrLn ""
    putStrLn "Escribe el número de la prueba a la que se quiere entrar."

    o <- getLine

    if o == "1" then do
        menuInicializaDeque
    else if o == "2" then do
        menuEliminacionesEInserciones
    else if o == "3" then do
        menuRebalanceo
    else do
        putChar '\n'
        putStrLn "No se ha seleccionado ninguna opción válida."

    return ()

-- Submenus Deque ------------------
menuInicializaDeque :: IO ()
menuInicializaDeque = do
    limpiar
    putStrLn "Podemos inicializar una Deque, con el constructor del tipo"
    putStrLn "de dato, pero la forma mas segura es usando la función"
    putStrLn "\"list2Deque\" transformando una lista dada en una Deque."
    putStrLn ""
    putStrLn "Escribe un caracter para ejecutar \"list2Deque [1,2,3,4,5,6,7]\""
    
    o <- getLine

    putStrLn "Salida (list2Deque [1,2,3,4,5,6,7]):"
    putStrLn $ show $ list2Deque [1,2,3,4,5,6,7]
    putStrLn ""
    putStrLn "Para volver al menú de Deque escribe 1"
    putStrLn "Para volver al menú principal escribe 2"
    putStrLn "Para salir escribe q"

    o <- getLine

    if o == "1" then do
        menuDeque
    else if o == "2" then do
        nuevoMenu
    else if o == "q" then do
        limpiar
        putChar '\n'
    else do
        putChar '\n'
        putStrLn "No se ha seleccionado ninguna opción válida."
    
    return ()

menuEliminacionesEInserciones :: IO ()
menuEliminacionesEInserciones = do
    limpiar
    putStrLn "La ventaja de la Deque frente a la lista normal, es que la"
    putStrLn "complejidad de borrado e inserción de elementos al final"
    putStrLn "es 1 al igual que al principio. Esto debido a la naturaleza"
    putStrLn "de las linked-list."
    putStrLn ""
    putStrLn "Vamos a hacer las pruebas con la siguiente Deque:"
    putStrLn ""
    putStrLn $ show $ list2Deque [1,2,3,4,5]
    let dequeTest = list2Deque [1,2,3,4,5]
    putStrLn ""
    putStrLn "Vamos a probar las dos funciones de inserción, la de insertar"
    putStrLn "por delante \"cons\", y por detras \"snoc\"."
    putStrLn ""
    putStrLn "Escribe un carácter para ejecutar las funciones."

    o <- getLine

    putStrLn ""
    putStrLn "Salida (cons 10 dequeTest):"
    putStrLn $ show $ cons dequeTest 10
    putStrLn ""
    putStrLn "Salida (snoc 10 dequeTest):"
    putStrLn $ show $ snoc dequeTest 10
    putStrLn ""
    putStrLn "Nos queda por probar la eliminación de elementos, la función"
    putStrLn "de eliminar el elemento del inicio es \"tailDeque\", y la de"
    putStrLn "eliminar por detras es \"initDeque\"."
    putStrLn ""
    putStrLn "Escribe un carácter para ejecutar la prueba de las funciones."
    
    o <- getLine

    putStrLn ""
    putStrLn "Salida (tailDeque dequeTest)"
    putStrLn $ show $ tailDeque dequeTest
    putStrLn ""
    putStrLn "Salida (initDeque dequeTest)"
    putStrLn $ show $ initDeque dequeTest
    putStrLn ""
    putStrLn "Estas son las funciones principales de las Deque."
    putStrLn ""
    putStrLn "Para volver al menú de Deque escribe 1."
    putStrLn ""
    putStrLn "Para volver al menú principal escribe 2."
    putStrLn ""
    putStrLn "Para salir escribe q"
    
    o <- getLine

    if o == "1" then do
        menuDeque
    else if o == "2" then do
        nuevoMenu
    else if o == "q" then do
        limpiar
        putChar '\n'
    else do
        putChar '\n'
        putStrLn "No se ha seleccionado ninguna opción válida."

    return ()

menuRebalanceo :: IO ()
menuRebalanceo = do
    limpiar
    putStrLn "Para que la Deque siga siendo eficiente en cualquier caso"
    putStrLn "tiene que haber una función que rebalancee las dos listas."
    putStrLn "En este caso la función se llama \"queue\", se le pasa"
    putStrLn "como parámetro una Deque y retorna otra que ya está"
    putStrLn "balanceada en función de la constante c definida (en"
    putStrLn "este caso su valor es 4) y la fórmula que se usa para decidir"
    putStrLn "si debe ser realizado el rebalanceo es la siguiente:"
    putStrLn ""
    putStrLn "          |F| <= c|R|+1 y |R| <= c|F|+1"
    putStrLn ""
    putStrLn "El ejemplo que vamos a usar para hacer la prueba es el"
    putStrLn "siguiente, (teniendo en cuenta que es incorrecto ya que"
    putStrLn "no está balanceado):"
    putStrLn ""
    putStrLn $ show $ Deque 7 [1,2,3,4,5,6,7] 1 [8]
    let dequeTest = Deque 7 [1,2,3,4,5,6,7] 1 [8]
    putStrLn ""
    putStrLn "Escribe un carácter para ejecutar \"queue dequeTest\"."
    
    o <- getLine
    
    putStrLn ""
    putStrLn "Salida (queue dequeTest):"
    putStrLn $ show $ queue dequeTest
    putStrLn ""
    putStrLn "Como podemos ver, ahora si está balanceado."
    putStrLn ""
    putStrLn "Para volver al menú de Deque escribe 1."
    putStrLn ""
    putStrLn "Para volver al menú principal escribe 2."
    putStrLn ""
    putStrLn "Para salir escribe q"
    
    o <- getLine

    if o == "1" then do
        menuDeque
    else if o == "2" then do
        nuevoMenu
    else if o == "q" then do
        limpiar
        putChar '\n'
    else do
        putChar '\n'
        putStrLn "No se ha seleccionado ninguna opción válida."


    return ()
---------------------------------------------------------------------------------

menuAVL = do
    limpiar
    return ()


-- MENU DE RED BLACK TREE -----------------------------------------------------
rbtStrF :: IO ()
rbtStrF = putStrLn "data Color = R | B deriving (Show, Eq)\ndata RBTree a = N Color a (RBTree a) (RBTree a)\n              | L\n              deriving (Show, Eq)"

menuRBT :: IO ()
menuRBT = do
    limpiar
    putStrLn "Menú de Red-Black Tree:\n"
    putStrLn ""
    putStrLn "La estructura del arbol es la siguiente"
    putStrLn ""
    rbtStrF
    putStrLn ""
    putStrLn "Un Red-Black Tree es una estructura de datos basada en un"
    putStrLn "arbol binario en el cual sus nodos son pintados de rojo o"
    putStrLn "negro, esta información es representada con un bit extra"
    putStrLn "de información en cada nodo."
    putStrLn ""
    putStrLn "Lo bueno de esta estructura de datos es que su complejidad"
    putStrLn "en busqueda, insercion y borrado es log(n), muy bueno en"
    putStrLn "comparación con otras estructuras (aunque van Embde Boas"
    putStrLn "es aun mejor ya que llega a la complejidad de log(log(n)))."
    putStrLn ""
    putStrLn "Todo Red-Black Tree cumple estas reglas:"
    putStrLn ""
    putStrLn "\t1) El nodo raiz es negro."
    putStrLn "\t2) Las hojas son negras."
    putStrLn "\t3) Un nodo rojo no tiene ningún hijo negro."
    putStrLn "\t4) El arbol debe tener la misma altura negra desde el nodo"
    putStrLn "\t   raiz a cada hoja, es decir, la suma de los nodos negros"
    putStrLn "\t   encontrados en el recorrido desde el nodo raiz a cada"
    putStrLn "\t   hoja, debe ser siempre el mismo."
    putStrLn ""
    putStrLn "Para hacer tests podemos probar con el siguiente ejemplo."
    putStrLn "Escribe un carácter para continuar:"

    o <- getLine

    limpiar
    ejrbt1StrF
    putStrLn ""
    ejrbt1Str
    putStrLn ""
    putStrLn "Las pruebas que realizaremos son sobre las funciones de insert"
    putStrLn ", delete y search."
    putStrLn "Comenzando con la función de búsqueda, se ha definido como"
    putStrLn "\"depthRBTOf\" que toma como entrada el elemento buscado y"
    putStrLn "el RBT en el que se busca, se retorna la profundidad en la que"
    putStrLn "se encuentra el elemento, en caso de no estar en el arbol se"
    putStrLn "retorna -1."
    putStrLn ""
    putStrLn "Es muy similar a la función de busqueda de un arbol de búsqueda"
    putStrLn "binaria."
    putStrLn "Escribe un caracter para ejecutar \"depthRBTOf 18 ejrbt1\""

    o <- getLine

    putStrLn ""
    putStrLn "Salida (depthRBTOf 18 ejrbt1)"
    putStrLn $ show $ depthRBTOf 18 ejrbt1
    putStrLn ""
    putStrLn "Como se ve la profundidad del elemento 18 es 1 y en efecto se"
    putStrLn "encuentra en el arbol."
    putStrLn ""
    putStrLn "La función de inserción está definida como \"insertRBT\" y toma"
    putStrLn "como valores de entrada un elemento que se quiere introducir y"
    putStrLn "el RBT en el que se quiere introducir."
    putStrLn ""
    putStrLn "Escribe un carácter para ejecutar \"insertRBT 200 ejrbt1\""

    o <- getLine

    putStrLn "Salida (insertRBT 200 ejrbt1):"
    putStrLn $ show $ insertRBT 200 ejrbt1
    putStrLn ""
    putStrLn "(En la representación gráfica no se muestran las hojas por falta"
    putStrLn "de espacio."
    ejrbt1InsertStr
    putStrLn ""
    putStrLn "Como se ve, el arbol ha tenido que ser rebalanceado ya que al"
    putStrLn "añadir directamente el 200 en el mismo, se incumplen algunas"
    putStrLn "restricciones, también hay que recolorear algunos nodos."
    putStrLn ""
    putStrLn "Por último tenemos la función de eliminar a la que llamamos"
    putStrLn "\"deleteRBT\", con los mismos parámetros de entrada que la "
    putStrLn "función de insert, y su salida es un RBT sin el elemento que"
    putStrLn "se desea eliminar."
    putStrLn ""
    putStrLn "Escribe un carácter para ejecutar \"deleteRBT 7 ejrbt1\""

    o <- getLine

    putStrLn ""
    putStrLn "Salida (deleteRBT 7 ejrbt1):"
    putStrLn $ show $ deleteRBT 7 ejrbt1
    putStrLn ""
    ejrbt1DeleteStr
    putStrLn ""
    putStrLn "En este caso también hay que hacer un rebalanceo del arbol y"
    putStrLn "recolorear algunos nodos."
    putStrLn ""
    putStrLn "Para volver al menu principal escribe 1."
    putStrLn "Para salir escribe q"

    t <- getLine

    if t == "1" then do
        nuevoMenu
    else if t == "q" then do
        limpiar
        putStrLn "Saliendo..."
    else do
        nuevoMenu 

    o <- getLine

    putStrLn "\n"
    return ()

-------------------------------------------------------------------------------
nuevoMenu :: IO ()
nuevoMenu = do
    limpiar
    putStrLn "El objetivo de este programa interactivo es tratar de aprender"
    putStrLn "distintas estructuras de datos, mediante algunas pruebas de las"
    putStrLn "funciones basicas implementadas, se busca facilitar la"
    putStrLn "implementación de futuros programas ya que se ha tratado de"
    putStrLn "preparar el código para poder ser reutilizado en multiples"
    putStrLn "ocasiones."
    putStrLn ""
    putStrLn "Antes de continuar se recuerda la existencia de la función help"
    putStrLn "en caso de estar ejecutando desde una consola de haskell."
    putStrLn "A continuación se presentan una serie de estructuras de datos \nde los cuales se pueden ver ejemplos de su uso."
    putStrLn ""
    putStrLn "1. Deque"
    putStrLn "2. Grafos"
    putStrLn "3. Hash table"
    putStrLn "4. Max/Min Heap"
    putStrLn "5. Arbol de búsqueda binaria"
    putStrLn "6. Splay tree"
    putStrLn "7. Red-Black Tree"
    putStrLn "\n"
    putStr "Elija una opción: "
    o <- getLine
    
    if o == "1" then do
        menuDeque               ----- > Hecho
    else if o == "2" then do
        menuGrafo               ----- > Hecho
    else if o == "3" then do
        menuHashTable           ----- > Hecho
    else if o == "4" then do
        menuMaxMinHeap             ----- > Hecho (revisar)
    else if o == "5" then do
        menuBST                 ----- > Hecho
    else if o == "6" then do
        menuSplayTree
    else if o == "7" then do
        menuRBT                 ----- > Hecho 
    else do
        putChar '\n'
        putStrLn "No se ha seleccionado ninguna opción válida."
    
    return ()

main = do
    hSetBuffering stdout NoBuffering
    nuevoMenu

help :: IO ()
help = do
    putStrLn "A continuación se muestran las funciones de ayuda que sirven para"
    putStrLn "consultar las funciones disponibles con cada estructura de datos."
    putStrLn ""
    putStrLn "helpBST"
    putStrLn "helpDeque"
    putStrLn "helpMaxHeap"
    putStrLn "helpMinHeaps"
    putStrLn "helpRBT"
    putStrLn "helpGraph"
    putStrLn "helpSplayTree"
    putStrLn "helpDGraph"
    putStrLn "helpHTS"
    putStrLn "helpHTL"
    putStrLn "helpHTQ"