import DataStructures.Graph as Graph
import Data.Set as S
import Data.PQueue.Min as PQueue

inf :: Float 
inf = 999999999.0

--dijkstra :: (Eq a, Ord a) => Graph a -> Vertex a -> [Path a]
dijkstra g src = dijkstra' g src heap notvisited S.empty 
    where notvisited = S.delete src (vertexSet g)
          heap = PQueue.fromList ((0,src): Prelude.map (\x -> (inf, x)) (S.toList notvisited))


dijkstra' g src heap notvisited path = do
    let paths = S.filter (\p -> S.member (getDest p) notvisited) (pathsFrom src) -- set
        heap = PQueue.fromList (S.toList paths)
        m = PQueue.getMin heap
        heap' = PQueue.deleteMin heap


{----                              path
dijkstra' g src heap notvisited res = do
    let m = findMin heap
    case m of
        Nothing -> return path
        Just (dist, node) ->
            do
                let heap' = deleteMin heap
                let paths = pathsFrom g node
                let --}



carreteras :: Graph String
carreteras = fromTuple ([(V "Sevilla"),(V "Huelva"),(V "Cadiz"),(V "Malaga"),(V "Granada"),(V "Almeria"),(V "Cordoba"),(V "Jaen")],
    [(P 92.8 (V "Sevilla") (V "Huelva")), (P 121.0 (V "Sevilla") (V "Cadiz")), (P 214.0 (V "Sevilla") (V "Malaga")),
        (P 141.0 (V "Sevilla") (V "Cordoba")), (P 234.0 (V "Cadiz") (V "Malaga")),(P 160.0 (V "Cordoba") (V "Malaga")),
            (P 108.0  (V "Cordoba") (V "Jaen")), (P 127.0 (V "Malaga") (V "Granada")),(P 93.8 (V "Jaen") (V "Granada")),
                (P 167.0 (V "Granada") (V "Almeria"))])