import Data.Char
import Text.Printf
import System.IO
import System.Directory

import Data.List as L
import DataStructures.BinarySearchTree as BST
import Data.Set as Set
import DataStructures.Graph as G

menuBST = do

    return ()

menuMinHeap = do

    return ()

menuMaxHeap = do

    return ()

menuGrafo = do
    putStrLn "Menú de grafos:\n"
    putStrLn "Aquí se pueden encontrar los siguientes grafos: "
    putStrLn "\n"
    putStrLn "Ejemplos"
    putStrLn "\n"
    putStrLn "Utilidades de grafos: "
    putStrLn "1. Tenemos un grafo que representa la distribución de carreteras\n en un mapa, los pesos de las aristas son los km de carretera que\n separan los vertices que son ciudades"
    putStrLn "\n"
    putStrLn "2. Tenemos un laberinto representado como un grafo, el laberito\n es resuelto por bfs y dfs."
    return ()

menuDeque = do

    return ()

menuAVL = do

    return ()

menu RBT = do

    return ()

nuevoMenu = do
    putStrLn "A continuación se presentan una serie de estructuras de datos \nde los cuales se pueden ver ejemplos de su uso."
    putStrLn "1. Deque"
    putStrLn "2. Grafos"
    putStrLn "3. MaxHeap"
    putStrLn "4. MinHeap"
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
        menuGrafo
    else if o == '3' then do
        menuMaxHeap
    else if o == '4' then do
        menuMinHeap
    else if o == '5' then do
        menuBST
    else if o == '6' then do
        menuAVL
    else if o == '7' then do
        menuRBT
    else do
        putChar '\n'
        putStrLn "No se ha seleccionado ninguna opción válida."
    
    return ()

main = do
    hSetBuffering stdout NoBuffering
    nuevoMenu