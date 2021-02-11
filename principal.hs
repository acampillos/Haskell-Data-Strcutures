import Data.Char
import Text.Printf
import System.IO
import System.Directory

import Data.List as L
import DataStructures.BinarySearchTree as BST
import Data.Set as Set
import DataStructures.Graph as G

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
carreteras = fromTuple ([(V "Sevilla"),(V "Huelva"),(V "Cadiz"),(V "Malaga"),(V "Granada"),(V "Almeria"),(V "Cordoba"),(V "Jaen")],
    [(P 92.8 (V "Sevilla") (V "Huelva")), (P 121.0 (V "Sevilla") (V "Cadiz")), (P 214.0 (V "Sevilla") (V "Malaga")),
        (P 141.0 (V "Sevilla") (V "Cordoba")), (P 234.0 (V "Cadiz") (V "Malaga")),(P 160.0 (V "Cordoba") (V "Malaga")),
            (P 108.0  (V "Cordoba") (V "Jaen")), (P 127.0 (V "Malaga") (V "Granada")),(P 93.8 (V "Jaen") (V "Granada")),
                (P 167.0 (V "Granada") (V "Almeria"))])

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
carreterasKruskalString = putStrLn "        92.8                  141.0                 108\n    +-------------- Sevilla ---------- Cordoba ----------- Jaen\n    |                   |                                   |\n Huelva             121 |                                   | 93.8\n                        |                                   |\n                        |                           127     |           167\n                      Cadiz            Malaga ----------- Granada ---------------- Almeria"

-- Laberinto
grafo :: Graph Int
grafo = fromTuple ([(V 1),(V 2),(V 3),(V 4),(V 5),(V 6),(V 7),(V 8),(V 9)],
    [(P 1 (V 1) (V 2)), (P 1 (V 1) (V 4)), (P 1 (V 2) (V 3)), (P 1 (V 2) (V 8)),
        (P 1 (V 4) (V 3)), (P 1 (V 4) (V 5)), (P 1 (V 5) (V 6)), (P 1 (V 3) (V 7)),
            (P 1 (V 7) (V 9))])

grafoStringFH = "grafo = fromTuple ([(V 1),(V 2),(V 3),(V 4),(V 5),(V 6),(V 7),(V 8),(V 9)],\n    [(P 1 (V 1) (V 2)), (P 1 (V 1) (V 4)), (P 1 (V 2) (V 3)), (P 1 (V 2) (V 8)),\n        (P 1 (V 4) (V 3)), (P 1 (V 4) (V 5)), (P 1 (V 5) (V 6)), (P 1 (V 3) (V 7)),\n            (P 1 (V 7) (V 9))])"

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

solDFSGrafo = dfs grafo (V 1)
solBFSGrafo = bfs grafo (V 1)

-- EJEMPLOS BST #############################################################################################
ejbst1 :: BSTree Int
ejbst1 = (N (N (H) (H) 5) (N (N (N (H) (H) 9) (N (H) (H) 13) 12) (N (H) (N (H) (H) 23) 19) 15) 8)

ejbst1StrF = putStrLn "ejbst1 = (N (N (H) (H) 5) (N (N (N (H) (H) 9) (N (H) (H) 13) 12) (N (H) (N (H) (H) 23) 19) 15) 8)"
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


-- MENU DE ARBOL DE BUSQUEDA BINARIA -----------------------------------------------------
{-
data BSTree a = N (BSTree a) (BSTree a) a
              | H
              deriving (Show, Eq)
-}
bstStr = putStrLn "data BSTree a = N (BSTree a) (BSTree a) a\n              | H\n              deriving (Show, Eq)"

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
    o <- getChar
    if o == '1' then do
        menuFuncionesBST
    else if o == '2' then do
        menuRecorridoBST
    else do
        putChar '\n'
        putStrLn "No se ha seleccionado ninguna opción válida."
    return ()

-- Submenus de BST -------------------

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

    o <- getChar
    o <- getChar

    putStrLn ""
    putStrLn "Salida de (depthOf 19 ejbst1):"
    putStrLn $ show $ depthOf 19 ejbst1
    putStrLn ""

    putStrLn ""
    putStrLn "Salida de (contains 19 ejbst1):"
    putStrLn $ show $ contains 19 ejbst1
    putStrLn ""

    putStrLn "Escribe un carácter para continuar."

    o <- getChar
    o <- getChar

    limpiar
    putStrLn "Ahora podemos probar la función de insercion, está definida"
    putStrLn "como \"insert\", toma como parámetros de entrada un elemento"
    putStrLn "y un BST, añade al BST el elemento dado."
    putStrLn ""
    putStrLn "Para ejecutar la función \"insert 2000 ejbst1\", que deberia"
    putStrLn "insertar en nustro BST de prueba el elemento 2000."

    o <- getChar
    o <- getChar

    putStrLn "Salida de (BST.insert 2000 ejbst1)"
    putStrLn $ show $ BST.insert 2000 ejbst1
    putStrLn ""
    ejbst1InsertStr
    putStrLn ""
    putStrLn "Como se ve, se ha insertado el elemento 2000 en el extremo mas"
    putStrLn "derecho del arbol, ya que este es el elemento mayor del BST"
    putStrLn ""
    putStrLn "Escribe un carácter para continuar."

    o <- getChar
    o <- getChar

    limpiar
    putStrLn "Por último probamos la función de borrado a la que llamamos"
    putStrLn "\"delete\", también toma como valores de entrada el elemento"
    putStrLn "que queremos eliminar y el arbol del que lo queremos eliminar."
    putStrLn ""
    putStrLn "Escriber un carácter para ejecutar la siguiente función:"
    putStrLn "\"delete 19 ejbst1\""

    o <- getChar
    o <- getChar

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

    o <- getChar
    o <- getChar

    limpiar
    putStrLn "Para volver al menú de BST escribe 1"
    putStrLn ""
    putStrLn "Para volver al menú principal escribe 2"
    putStrLn ""
    putStrLn "Para salir escribe q"

    o <- getChar
    o <- getChar

    if o == '1' then do
        menuBST
    else if o == '2' then do
        nuevoMenu
    else do
        putChar '\n'
    return ()


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

    o <- getChar
    o <- getChar
    
    limpiar
    putStrLn ""
    ejbst1Str
    putStrLn ""
    putStrLn "Escribe un carácter para ver el recorrido inorder del arbol."
    putStrLn ""

    o <- getChar
    o <- getChar

    putStrLn "inorder ejbst1"
    putStrLn $ show $ inorder ejbst1
    putStrLn ""
    putStrLn "Escribe un carácter para ver el recorrido preorder del arbol."
    putStrLn ""

    o <- getChar
    o <- getChar

    putStrLn "preorder ejbst1"
    putStrLn $ show $ preorder ejbst1
    putStrLn ""
    putStrLn "Escribe un carácter para ver el recorrido postorder del arbol."
    putStrLn ""

    o <- getChar
    o <- getChar

    putStrLn "postorder ejbst1"
    putStrLn $ show $ postorder ejbst1
    putStrLn ""

    putStrLn "Para volver al menú de BST escribe 1"
    putStrLn ""
    putStrLn "Para volver al menú principal escribe 2"
    putStrLn ""
    putStrLn "Para salir escribe q"

    o <- getChar
    o <- getChar

    if o == '1' then do
        menuBST
    else if o == '2' then do
        nuevoMenu
    else do
        putChar '\n'
    return ()

--------------------------------------

menuMinHeap = do
    limpiar
    return ()

menuMaxHeap = do
    limpiar
    return ()

-- MENU DE GRAFO ----------------------------------------------------------------------------

strDefGrafo = putStrLn "type Graph a = (Set (Vertex a), Set (Path a))\n\ndata Vertex a = V a\n    deriving (Show, Eq, Ord)\n\n--                w        src       dst\ndata Path a = P Float (Vertex a) (Vertex a)\n    deriving (Show, Eq,Ord)"

menuGrafo = do
    limpiar
    putStrLn "Menú de grafos:\n"
    putStrLn ""
    putStrLn "Los grafos tienen la siguiente estructura:"
    strDefGrafo
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
    putStrLn "Escribe el número de la prueba a la que se quiere entrar."
    o <- getChar
    if o == '1' then do
        menuCosteMinimo
    else if o == '2' then do
        menuRecorrido
    else if o == '3' then do
        menuConectividad
    else do
        putChar '\n'
        putStrLn "No se ha seleccionado ninguna opción válida."
    return ()

----Submenus grafos ---------
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
    putStrLn "Escribe 2 para volver al menú anterior."

    o <- getChar
    getChar
    if o == '1' then do
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
        
        -- Hacer que pueda probar con sus propios grafos

    else if o == '2' then do
        menuGrafo
    else do
        menuGrafo
    return ()

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

    o <- getChar
    getChar

    if o == '1' then do
        menuSalidaBFSDFS

        -- Hacer que pueda probar con sus propios grafos

    else if o == '2' then do
        menuGrafo
    else do
        menuGrafo
    return ()

menuSalidaBFSDFS = do
    limpiar
    grafoString
    putStrLn ""
    putStrLn "Salida dfs"
    putStrLn (show (dfs grafo (V 1)))
    putStrLn "Salida bfs"
    putStrLn (show (bfs grafo (V 1)))
    putStrLn ""
    putStrLn "Ambas listas representan de forma secuencial el vertice que ha"
    putStrLn "sido visitado en cada iteración (llamada recursiva) del algoritmo"
    putStrLn ""
    putStrLn "Para salir escribe q, escribe otra cosa para volver al inicio."
    putStrLn ""

    o <- getChar
    getChar

    if o == 'q' then do
        limpiar

        -- Hacer que pueda probar con sus propios grafos

    else if o == '2' then do
        limpiar
    else do
        limpiar
    return ()

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
    o <- getChar
    getChar

    if o == '1' then do
        menuEjemplosConectividad
    else do
        menuGrafo

    return ()

--menuEjemplosConectividad = limpiar

menuEjemplosConectividad = do
    limpiar
    putStrLn "Podemos comprobar que el siguiente grafo de ejemplo es conexo"
    putStrLn "usando la función \"conexo\" si nos retorna True y usando la"
    putStrLn "función \"conectividad\" si nos retorna 1 (una componente"
    putStrLn "conexa:"
    putStrLn ""
    grafoString
    putStrLn ""
    putStrLn "Escribe un caracter para ejecutar \"conexo grafo\"."
    
    o <- getChar
    o <- getChar

    putStrLn "Salida:"
    putStrLn (show (conexo grafo))

    putStrLn ""
    putStrLn "Escribe un caracter para ejecutar \"conectividad grafo\""
    
    o <- getChar
    o <- getChar

    putStrLn ""
    putStrLn "Salida:"
    putStrLn (show (conectividad grafo))
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
    putStrLn "let grafo' = addVertex grafo (V 100)"
    putStrLn "vertexSet grafo'"

    o <- getChar
    o <- getChar

    putStrLn ""
    let grafo' = addVertex grafo (V 100)
    putStrLn (show (vertexSet grafo'))
    putStrLn ""
    putStrLn "conexo grafo'"
    putStrLn (show (conexo grafo'))
    putStrLn ""
    putStrLn "conectividad grafo'"
    putStrLn (show (conectividad grafo'))

    o <- getChar
    o <- getChar

    putStrLn ""
    putStrLn "Podemos ver también los conjuntos de adyacencias de un vertice."
    putStrLn "Por ejemplo si mostramos el conjunto de adyacencias del nuevo "
    putStrLn "vértice debe mostrarnos un conjunto vacio ya que no está conectado"
    putStrLn "a otros vértices mediante aristas."
    putStrLn ""
    putStrLn "Escribe un carácter para ejecutar las siguientes dos funciones y"
    putStrLn "comprobarlo."
    putStrLn ""

    putStrLn "adjacents grafo' (V 100)"
    putStrLn ""
    putStrLn "adjacents grafo' (V 1)"
    putStrLn ""

    o <- getChar
    o <- getChar

    putStrLn "Salida (adjacents grafo' (V 100)):"
    putStrLn (show (adjacents grafo' (V 100)))
    putStrLn ""
    putStrLn "Salida (adjacents grafo' (V 1)):"
    putStrLn (show (adjacents grafo' (V 1)))
    putStrLn ""
    putStrLn "Para volver al menu de grafos escribe 1."
    putStrLn "Para volver al menu principal escribe 2."
    putStrLn "Para salir escribe q"

    t <- getChar
    t <- getChar

    if t == '1' then do
        menuGrafo
    else if t == '2' then do
        nuevoMenu
    else do
        nuevoMenu   -- AQUI HAY QUE CAMBIAR ESTO

    return ()

------------------------------


menuHashTable = do
    limpiar
    -- Introducción
    putStrLn "Una tabla hash es una estructura de datos que implementa el ADT de un array asociativo o diccionario."
    putStrLn "Utiliza una función de hash para calcular un índice o código hash que identifica posición de la casilla o cubeta (bucket o slot) donde se localiza el valor."
    -- Tipos de hash table implementadas según el tratamiento de colisiones
    putStrLn "Idealmente, la función de hash asignará cada clave a un único bucket pero la mayoría de hash tables emplean una función de hash imperfecta que puede causar colisiones."
    -- Ajuste del tamaño de la tabla (load factor)    
    putStrLn "\n\ta. Separate chaining."
    putStrLn "\tb. Linear probing."
    putStrLn "\tc. Quadratic probing.\n"
    putStr "Elija una opción: "
    o <- getChar
    getChar
    if o == 'a' then do
        menuHTSeparateChaining
    else if o == 'b' then do
        menuHTLinearProbing
    else if o == 'c' then do
        menuHTQuadraticProbing
    else do
        putChar '\n'
        putStrLn "No se ha seleccionado ninguna opción válida."
    return ()

menuHTSeparateChaining = do
    limpiar
    putStrLn "En este método, cada cubo es independiente y en ellos se almacenan un conjunto de pares clave-valor en una lista."
    putStrLn "En la mayoría de implementaciones cada bucket tendrá pocas entradas si la función de hash es adecuada."
    putStrLn "La complejidad de las operaciones es O(1) para la búsqueda del bucket correspondiente y O(n) para la búsqueda en la lista (en nuestro caso listas enlazadas)."
    
    return ()

menuHTLinearProbing = do
    limpiar
    return ()

menuHTQuadraticProbing = do
    limpiar
    return ()

menuDeque = do
    limpiar
    return ()

menuAVL = do
    limpiar
    return ()


-- MENU DE RED BLACK TREE -----------------------------------------------------
menuRBT = do
    limpiar
    
    return ()

-------------------------------------------------------------------------------

nuevoMenu = do
    limpiar
    putStrLn "A continuación se presentan una serie de estructuras de datos \nde los cuales se pueden ver ejemplos de su uso."
    putStrLn "1. Deque"
    putStrLn "2. Grafos"
    putStrLn "3. Hash table"
    putStrLn "4. Max/Min Heap"
    putStrLn "5. Arbol de búsqueda binaria"
    putStrLn "6. AVL"
    putStrLn "7. Red-Black Tree"
    putStrLn "\n"
    putStr "Elija una opción: "
    o <- getChar
    getChar
    if o == '1' then do
        menuDeque
    else if o == '2' then do
        menuGrafo               ----- > Casi terminado
    else if o == '3' then do
        menuHashTable           ----- > En prograso
    else if o == '4' then do
        menuMaxHeap
    else if o == '5' then do
        menuBST                 ----- > Hecho
    else if o == '6' then do
        menuAVL
    else if o == '7' then do
        menuRBT                 ----- > En prograso
    else do
        putChar '\n'
        putStrLn "No se ha seleccionado ninguna opción válida."
    
    return ()

main = do
    hSetBuffering stdout NoBuffering
    nuevoMenu