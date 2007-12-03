module Game.Definition where

import Data.Tree

---------------------
-- Game Definition --
---------------------

-- Game Tree
type InfoGroup m v = [Turn m v]
data Turn m v = Turn Int (InfoGroup m v) [(m, GameTree m v)] deriving (Eq, Show)
data GameTree m v = Decision (Turn m v)
                  | Chance [(Int, GameTree m v)]
                  | Payoff [v]
                  deriving (Eq)

-- Instance Declarations
instance (Show m, Show v) => Show (GameTree m v) where
  show t = drawTree $ s "" t
    where s p (Decision (Turn i _ ts)) = Node (p ++ "Player " ++ show i) [s (show m ++ " -> ") t | (m, t) <- ts]
          s p (Chance ts) = Node (p ++ "Chance") [s (show c ++ " -> ") t | (c, t) <- ts]
          s p (Payoff vs) = Node (p ++ show vs) []
          cs f ts = [s (f a ++ " -> ") t | (a, t) <- ts]

-- On operator that combines two game trees.
(<+>) :: (Num v) => GameTree m v -> GameTree m v -> GameTree m v
Payoff as <+> Payoff bs = Payoff [a + b | (a,b) <- zip as bs]
Chance as <+> Chance bs = Chance (as ++ bs)
Decision (Turn t1 g1 ms1) <+> Decision (Turn t2 g2 ms2) | t1 == t2 =
    Decision (Turn t1 (g1 ++ g2) (ms1 ++ ms2))

-- Add a branch to a turn node.
(<|>) :: Turn m v -> (m, GameTree m v) -> Turn m v
Turn t g ms <|> m = Turn t g (m:ms)

------------------------
-- GameTree Traversal --
------------------------

asTree :: GameTree m v -> Tree (GameTree m v)
asTree t@(Decision (Turn _ _ ts)) = Node t [asTree t' | t' <- snd (unzip ts)]
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
