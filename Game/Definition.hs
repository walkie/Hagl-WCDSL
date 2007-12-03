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
                  | Payoff [v] (GameTree m v)
                  | End
                  deriving (Eq)

-- Instance Declarations
instance (Show m, Show v) => Show (GameTree m v) where
  show t = drawTree $ s "" t
    where s p (Decision (Turn i _ ts)) = Node (p ++ "Player " ++ show i) [s (show m ++ " -> ") t | (m, t) <- ts]
          s p (Chance ts) = Node (p ++ "Chance") [s (show c ++ " -> ") t | (c, t) <- ts]
          s p (Payoff vs End) = Node (p ++ show vs) []
          s p (Payoff vs t) = Node (p ++ show vs) [s "" t]
          s p End = Node "End" []
          cs f ts = [s (f a ++ " -> ") t | (a, t) <- ts]

asTree :: GameTree m v -> Tree (GameTree m v)
asTree t@(Decision (Turn _ _ ts)) = Node t [asTree t' | t' <- snd (unzip ts)]
asTree t@(Chance ts) = Node t [asTree t' | t' <- snd (unzip ts)]
asTree t@(Payoff _ t') = Node t [asTree t']
asTree t = Node t []

------------------------
-- GameTree Traversal --
------------------------

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
