module Game.Definition where

import Data.Tree

---------------------
-- Game Definition --
---------------------

-- Game Tree
type InfoGroup m v = [Turn m v]
data Turn m v = Turn Int (InfoGroup m v) [(m, Game m v)] deriving (Eq, Show)
data Game m v = Decision (Turn m v)
              | Chance [(Int, Game m v)]
              | Payoff [v]
              deriving (Eq)

-- Instance Declarations
instance (Show m, Show v) => Show (Game m v) where
  show t = drawTree $ s "" t
    where s p (Decision (Turn i _ ts)) = Node (p ++ "Player " ++ show i) [s (show m ++ " -> ") t | (m, t) <- ts]
          s p (Chance ts) = Node (p ++ "Chance") [s (show c ++ " -> ") t | (c, t) <- ts]
          s p (Payoff vs) = Node (p ++ show vs) []
          cs f ts = [s (f a ++ " -> ") t | (a, t) <- ts]

-- Combines two game trees.
(<+>) :: (Num v) => Game m v -> Game m v -> Game m v
Payoff as <+> Payoff bs = Payoff [a + b | (a,b) <- zip as bs]
Chance as <+> Chance bs = Chance (as ++ bs)
Decision (Turn t1 g1 ms1) <+> Decision (Turn t2 g2 ms2) | t1 == t2 =
    Decision (Turn t1 (g1 ++ g2) (ms1 ++ ms2))

-- Add a branch to a turn node.
(<|>) :: Turn m v -> (m, Game m v) -> Turn m v
Turn t g ms <|> m = Turn t g (m:ms)

----------------------------
-- Normal Form Definition --
----------------------------

-- Construct a game from a Normal-Form definition
normal :: (Eq m) => Int -> [m] -> [[v]] -> Game m v
normal np ms vs =
    let turns n = [Turn n (turns n) (zip ms ns) | ns <- chunk (length ms) (nodes (n+1))]
        nodes n | n > np = [Payoff v | v <- vs]
        nodes n | n <= np = map Decision (turns n)
    in head $ nodes 1

-- Construct a two-player Normal-Form game.
matrix :: (Eq m) => [m] -> [[v]] -> Game m v
matrix = normal 2

-- Construct a two-player Zero-Sum game.
zerosum :: (Eq m, Num v) => [m] -> [v] -> Game m v
zerosum ms vs = normal 2 ms (map (\v -> [v, -v]) vs)

-----------------------------
-- State-Driven Definition --
-----------------------------

data StateGame d m v = StateGame (d -> Int) (d -> [m]) (d -> m -> d) (d -> [v])

stateGame :: StateGame d m v -> d -> Game m v
stateGame spec@(StateGame who moves exec pay) d =
    let end = null . moves
        next = zip (moves d) (map (stateGame spec . (exec d)) (moves d))
        turn = Turn (who d) [turn] next
    in if end d then Payoff (pay d) else Decision turn

------------------------
-- Game Traversal --
------------------------

asTree :: Game m v -> Tree (Game m v)
asTree t@(Decision (Turn _ _ ts)) = Node t [asTree t' | t' <- snd (unzip ts)]
asTree t@(Chance ts) = Node t [asTree t' | t' <- snd (unzip ts)]
asTree t@(Payoff _) = Node t []

-- The immediate children of a node.
children :: Game m v -> [Game m v]
children = map rootLabel . subForest . asTree

-- Search nodes in BFS order.
bfs :: Game m v -> [Game m v]
bfs t = let b [] = []
            b ts = ts ++ b (concatMap children ts)
        in b [t]

-- Search nodes DFS order.
dfs :: Game m v -> [Game m v]
dfs t = t : concatMap dfs (children t)

---------------
-- Utilities --
---------------

-- Split a list into n-sized chunks.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = (take n l) : chunk n (drop n l)
