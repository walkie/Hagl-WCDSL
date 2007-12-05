{-# OPTIONS_GHC -fglasgow-exts #-}

module Game.Definition where

import Data.List
import Data.Tree

---------------------
-- Game Definition --
---------------------

-- Game Definition
data Game m v = Game {
    numPlayers :: Int,
    tree       :: GameTree m v,
    infoGroup  :: GameTree m v -> [GameTree m v]
}

-- Game Tree
data GameTree m v = Decision Int [(m, GameTree m v)]
                  | Chance [(Int, GameTree m v)]
                  | Payoff [v]
                  deriving (Eq)

-- Instance Declarations
instance (Show m, Show v) => Show (GameTree m v) where
  show t = drawTree $ s "" t
    where s p (Decision i ts) = Node (p ++ "Player " ++ show i) [s (show m ++ " -> ") t | (m, t) <- ts]
          s p (Chance ts) = Node (p ++ "Chance") [s (show c ++ " -> ") t | (c, t) <- ts]
          s p (Payoff vs) = Node (p ++ show vs) []
instance (Show m, Show v) => Show (Game m v) where
  show g = show (tree g)

----------------------------
-- Normal Form Definition --
----------------------------

-- Construct a game from a Normal-Form definition
normal :: (Eq m) => Int -> [m] -> [[v]] -> Game m v
normal np ms vs =
    let nodes n = if n > np 
          then [Payoff v | v <- vs]
          else [Decision n (zip ms ns) | ns <- chunk (length ms) (nodes (n+1))]
        group (Decision n _) = nodes n
        group t = [t]
    in Game np (head $ nodes 1) group

-- Construct a two-player Normal-Form game.
matrix :: (Eq m) => [m] -> [[v]] -> Game m v
matrix = normal 2

-- Construct a two-player Zero-Sum game.
zerosum :: (Eq m, Num v) => [m] -> [v] -> Game m v
zerosum ms vs = normal 2 ms (map (\v -> [v, -v]) vs)

-------------------------------
-- Extensive Form Definition --
-------------------------------

-- Build a game from a tree.
extensive :: GameTree m v -> Game m v
extensive t = let p (Decision i _) = i
                  p _ = 0
                  np = foldl1 max $ map p (bfs t)
              in Game np t (\a -> [a])

-----------------------------
-- State-Driven Definition --
-----------------------------

{- Args:
     * Number of players.
     * Whose turn is it?
     * What are the available moves? If empty, check payoff.
     * Execute a move and return the new state.
     * What is the payoff for this state? Undefined if there are available moves.
     * Initial state. -}
stateGame :: Int -> (d -> Int) -> (d -> [m]) -> (d -> m -> d) -> (d -> [v]) -> d -> Game m v
stateGame np who moves exec pay init = 
    let end = null . moves
        tree d = if end d 
          then Payoff (pay d)
          else Decision (who d) $ zip (moves d) $ map (tree . exec d) (moves d)
    in Game np (tree init) (\t -> [t])

----------------------------
-- Game Tree Construction --
----------------------------

-- Construct a decision node with only one option.
decision :: Int -> (m, GameTree m v) -> GameTree m v
decision i m = Decision i [m]

-- Construct a chance node with only one option.
chance :: (Int, GameTree m v) -> GameTree m v
chance c = Chance [c]

-- Combines two game trees.
(<+>) :: (Eq m, Num v) => GameTree m v -> GameTree m v -> GameTree m v
Payoff as <+> Payoff bs = Payoff [a + b | (a,b) <- zip as bs]
Chance as <+> Chance bs = Chance (as ++ bs)
Decision a as <+> Decision b bs | a == b = Decision a (as ++ bs)

-- Add a branch to a game tree.
class Branch t b where
    (<|>) :: t -> b -> t
instance Branch (GameTree m v) (m, GameTree m v) where
    Decision i ms <|> m = Decision i (m:ms)
instance Branch (GameTree m v) (Int, GameTree m v) where
    Chance cs <|> c = Chance (c:cs)

-------------------------
-- Game Tree Traversal --
-------------------------

-- Return the moves that are available from this node.
availMoves :: (GameTree m v) -> [m]
availMoves (Decision _ ms) = map fst ms
availMoves _ = []

-- Get the game tree as a Data.Tree structure.
asTree :: GameTree m v -> Tree (GameTree m v)
asTree t@(Decision _ ts) = Node t [asTree t' | t' <- snd (unzip ts)]
asTree t@(Chance ts) = Node t [asTree t' | t' <- snd (unzip ts)]
asTree t@(Payoff _) = Node t []

-- The immediate children of a node.
children :: GameTree m v -> [GameTree m v]
children = map rootLabel . subForest . asTree

-- Search nodes in BFS order.
bfs :: GameTree m v -> [GameTree m v]
bfs t = let b [] = []
            b ts = ts ++ b (concatMap children ts)
        in b [t]

-- Search nodes DFS order.
dfs :: GameTree m v -> [GameTree m v]
dfs t = t : concatMap dfs (children t)

---------------
-- Utilities --
---------------

-- Split a list into n-sized chunks.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = (take n l) : chunk n (drop n l)
