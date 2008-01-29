{-# OPTIONS_GHC -fglasgow-exts #-}

module Game.Definition where

import Data.List
import Data.Tree

---------------------
-- Game Definition --
---------------------

-- Game Definition
data Game m = Game {
    numPlayers :: Int,
    info       :: GameTree m -> InfoGroup m,
    tree       :: GameTree m
}

-- Game Tree
data GameTree m = Decision Int [(m, GameTree m)]
                | Chance [(Int, GameTree m)]
                | Payoff [Float]
                deriving (Eq)

data InfoGroup m = Perfect (GameTree m)
                 | Imperfect [GameTree m]
                 deriving (Eq)


-- Instance Declarations
instance (Show m) => Show (GameTree m) where
  show t = condense $ drawTree $ s "" t
    where s p (Decision i ts) = Node (p ++ "Player " ++ show i) [s (show m ++ " -> ") t | (m, t) <- ts]
          s p (Chance ts) = Node (p ++ "Chance") [s (show c ++ " -> ") t | (c, t) <- ts]
          s p (Payoff vs) = Node (p ++ show vs) []
          condense s = let empty = not . and . map (\c -> c == ' ' || c == '|')
                       in unlines $ filter empty $ lines s
instance (Show m) => Show (Game m) where
  show g = show (tree g)
instance (Show m) => Show (InfoGroup m) where
  show (Perfect t) = show t
  show (Imperfect ts) = unlines $ intersperse " ** or **" (map (init . show) ts)

----------------------------
-- Normal Form Definition --
----------------------------

-- Construct a game from a Normal-Form definition
normal :: Int -> [m] -> [[Float]] -> Game m
normal np ms vs =
    let chunk _ [] = []
        chunk n l = (take n l) : chunk n (drop n l)
        nodes n = if n > np
          then [Payoff v | v <- vs]
          else [Decision n (zip ms ns) | ns <- chunk (length ms) (nodes (n+1))]
        group (Decision n _) = nodes n
        group t = [t]
    in Game np (Imperfect . group) (head $ nodes 1)

-- Construct a two-player Normal-Form game.
matrix :: [m] -> [[Float]] -> Game m
matrix = normal 2

-- Construct a two-player Zero-Sum game.
zerosum :: [m] -> [Float] -> Game m
zerosum ms vs = matrix ms (map (\v -> [v, -v]) vs)

-------------------------------
-- Extensive Form Definition --
-------------------------------

-- Build a game from a tree.
extensive :: GameTree m -> Game m
extensive t = let p (Decision i _) = i
                  p _ = 0
                  np = foldl1 max $ map p (bfs t)
              in Game np Perfect t

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
stateGame :: Int -> (d -> Int) -> (d -> [m]) -> (d -> m -> d) -> (d -> [Float]) -> d -> Game m
stateGame np who moves exec pay init = 
    let end = null . moves
        tree d = if end d 
          then Payoff (pay d)
          else Decision (who d) $ zip (moves d) $ map (tree . exec d) (moves d)
    in Game np Perfect (tree init)

----------------------------
-- Game Tree Construction --
----------------------------

-- Construct a decision node with only one option.
decision :: Int -> (m, GameTree m) -> GameTree m
decision i m = Decision i [m]

-- Construct a chance node with only one option.
chance :: (Int, GameTree m) -> GameTree m
chance c = Chance [c]

-- Combines two game trees.
(<+>) :: (Eq m) => GameTree m -> GameTree m -> GameTree m
Payoff as <+> Payoff bs = Payoff [a + b | (a,b) <- zip as bs]
Chance as <+> Chance bs = Chance (as ++ bs)
Decision a as <+> Decision b bs | a == b = Decision a (as ++ bs)

-- Add a decision branch to a game tree.
(<|>) :: GameTree m -> (m, GameTree m) -> GameTree m
Decision i ms <|> m = Decision i (m:ms)

-------------------------
-- Game Tree Traversal --
-------------------------

-- Return the moves that are available from this node.
availMoves :: GameTree m -> [m]
availMoves (Decision _ ms) = map fst ms
availMoves _ = []

-- The immediate children of a node.
children :: GameTree m -> [GameTree m]
children (Decision _ ms) = map snd ms
children (Chance cs) = map snd cs
children _ = []

-- Search nodes in BFS order.
bfs :: GameTree m -> [GameTree m]
bfs t = let b [] = []
            b ts = ts ++ b (concatMap children ts)
        in b [t]

-- Search nodes DFS order.
dfs :: GameTree m -> [GameTree m]
dfs t = t : concatMap dfs (children t)

-- Get the game tree as a Data.Tree structure.
asTree :: GameTree m -> Tree (GameTree m)
asTree t = Node t $ map asTree (children t)
