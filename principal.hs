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

-- EJEMPLOS GRAFOS
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

menuBST = do
    limpiar
    putStrLn "Menú Arbol de Búsqueda Binaria:\n"
    putStrLn "Un arbol de busqueda binaria es..."
    putStrLn "Tenemos algunos de estos arboles para hacer pruebas"
    putStrLn ""
    return ()

menuMinHeap = do
    limpiar
    return ()

menuMaxHeap = do
    limpiar
    return ()

-- MENU DE GRAFO ----------------------------------------------------------------------------
menuGrafo = do
    limpiar
    putStrLn "Menú de grafos:\n"
    putStrLn "Aquí se pueden encontrar los siguientes grafos: "
    putStrLn "\n"
    putStrLn "Utilidades de grafos: "
    putStrLn "1. Tenemos un grafo que representa la distribución de carreteras"
    putStrLn "en un mapa, los pesos de las aristas son los km de carretera que"
    putStrLn "separan los vertices que son ciudades"
    putStrLn ""
    putStrLn "2. Tenemos un grafo, vemos como se recorre el grafo por dfs y bfs"
    putStrLn ""
    putStrLn "3. Podemos hacer pruebas con la conectividad."
    putStrLn ""
    o <- getChar
    getChar
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

    i <- getChar
    getChar

    if i == 'q' then do
        limpiar

        -- Hacer que pueda probar con sus propios grafos

    else if i == '2' then do
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
        limpiar

    return ()

menuEjemplosConectividad = limpiar
{-
menuEjemplosConectividad = do
    limpiar
    putStrLn "Podemos comprobar que el siguiente grafo de ejemplo es conexo"
    putStrLn "usando la función \"conexo\" si nos retorna True y usando la"
    putStrLn "función \"conectividad\" si nos retorna 1 (una componente"
    putStrLn "conexa:"
    putStrLn ""
    grafoString
    putStrLn ""
    putStrLn "conexo grafo"
    o <- getChar
    getChar
    putStrLn (show (conexo grafo))


    if o == '1' then do
        limpiar
    else if o == '2' then do
        limpiar
    else do
        limpiar

    return ()-}

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

menuRBT = do
    limpiar

    return ()

nuevoMenu = do
    limpiar
    putStrLn "A continuación se presentan una serie de estructuras de datos \nde los cuales se pueden ver ejemplos de su uso."
    putStrLn "1. Deque"
    putStrLn "2. Grafos"
    putStrLn "3. Hash table"
    putStrLn "4. MaxHeap"
    putStrLn "5. MinHeap"
    putStrLn "6. Arbol de búsqueda binaria"
    putStrLn "7. AVL"
    putStrLn "8. Red-Black Tree"
    putStrLn "\n"
    putStr "Elija una opción: "
    o <- getChar
    getChar
    if o == '1' then do
        menuDeque
    else if o == '2' then do
        menuGrafo               ----- > En progreso
    else if o == '3' then do
        menuHashTable           ----- > En prograso
    else if o == '4' then do
        menuMaxHeap
    else if o == '5' then do
        menuMinHeap
    else if o == '6' then do
        menuBST
    else if o == '7' then do
        menuAVL
    else if o == '8' then do
        menuRBT
    else do
        putChar '\n'
        putStrLn "No se ha seleccionado ninguna opción válida."
    
    return ()

main = do
    hSetBuffering stdout NoBuffering
    nuevoMenu