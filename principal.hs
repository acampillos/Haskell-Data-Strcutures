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
limpiar = system "cls"

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

menuGrafo = do
    limpiar
    putStrLn "Menú de grafos:\n"
    putStrLn "Aquí se pueden encontrar los siguientes grafos: "
    putStrLn "\n"
    putStrLn "Ejemplos"
    putStrLn "\n"
    putStrLn "Utilidades de grafos: "
    putStrLn "1. Tenemos un grafo que representa la distribución de carreteras"
    putStrLn "en un mapa, los pesos de las aristas son los km de carretera que"
    putStrLn "separan los vertices que son ciudades"
    putStrLn "\n"
    putStrLn "2. Tenemos un laberinto representado como un grafo, el laberito"
    putStrLn "es resuelto por bfs y dfs."
    putStrLn "\n"
    putStrLn "3. Podemos hacer pruebas con la conectividad."
    putStrLn "\n"
    o <- getChar
    getChar
    if o == '1' then do
        menuCosteMinimo
    else if o == '2' then do
        menuLaberinto
    else if o == '3' then do
        menuConectividad
    else do
        putChar '\n'
        putStrLn "No se ha seleccionado ninguna opción válida."
    return ()

----Submenus grafos ---------
menuCosteMinimo = do
    limpiar
    return ()

menuLaberinto = do
    limpiar
    return ()

menuConectividad = do
    limpiar
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
        menuGrafo
    else if o == '3' then do
        menuHashTable
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