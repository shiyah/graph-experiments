module Graph (
  Vertex,
  Edge,
  Table,
  Graph,
  mkGraph
) where
import qualified Data.Vector as V

type Vertex a = a
type Edge a = (Vertex a, Vertex a)
type Table a = V.Vector a

type Graph a = (Table (Vertex a), Table (Edge a))

mkGraph :: (Eq a, Show a) => [a] -> [(a, a)] -> Graph a
mkGraph vertices edges = (vs, es)
  where vs = V.fromList vertices
        es = V.fromList (map (checkEdge vs) edges)

checkEdge :: (Show a, Eq a, Foldable t) => t a -> (a, a) -> (a, a)
checkEdge vlist (e1, e2) = if (e1 `elem` vlist) && (e2 `elem` vlist) then (e1, e2)
                                                                     else error $ "Invalid edge: " ++ show (e1, e2)
