-- Pre-defined Functions
import Data.List (nub,sort)

type Node  = Int
type Edge  = (Node,Node)
type Graph = [Edge]
type Path  = [Node]

norm :: Ord a => [a] -> [a]
norm = sort . nub

-- Global Variables
g :: Graph
g = [(1,2),(1,3),(2,3),(2,4),(3,4)]
h :: Graph
h = [(1,2),(1,3),(2,1),(3,2),(4,4)]

-- a) Define the function nodes :: Graph -> [Node] that computes the list of nodes contained in a given graph.
nodes :: Graph -> [Node]
nodes [] = []
nodes ((source,destination):tail) = norm([source,destination] ++ nodes tail)

-- b) Define the function suc :: Node -> Graph -> [Node] that computes the list of successors for a node in a given graph.
suc :: Node -> Graph -> [Node]
suc node [] = []
suc node ((source,destination):tail) =
    if node == source then do norm([destination] ++ suc node tail)
    else do suc node tail

-- c) Define the function detach :: Node -> Graph -> Graph that removes a node together with all of its incident edges from a graph.
detach :: Node -> Graph -> Graph
detach node [] = []
detach node ((source,destination):tail) =
    if node == source || node == destination then do detach node tail
    else do [(source,destination)] ++ detach node tail

-- d) Define the function cyc :: Int -> Graph that creates a cycle of any given number.
cyc :: Int -> Graph
cyc 0 = []
cyc 1 = [(1,1)]
cyc num = zip ([1..num] ++ [1]) (tail ([1..num] ++ [1]))
