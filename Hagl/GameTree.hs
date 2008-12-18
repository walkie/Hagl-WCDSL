{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances #-}

module Hagl.GameTree where

import Data.List
import Data.Maybe
import qualified Data.Tree as Tree

import Hagl.Core

----------------
-- Game Trees --
----------------

type Edge g = (Move g, GameTree g)

data GameTree g = Decision PlayerIx [Edge g] -- decision made by a player
                | Chance (Dist (Edge g))     -- random move from distribution
                | Payoff Payoff              -- terminating payoff

-- The moves available from a node.
movesFrom :: GameTree g -> [Move g]
movesFrom (Decision _ es) = [m | (m,_) <- es]
movesFrom (Chance d) = [m | (_,(m,_)) <- d]
movesFrom _ = []

-- The immediate children of a node.
children :: GameTree g -> [GameTree g]
children (Decision _ es) = [n | (_,n) <- es]
children (Chance d) = [n | (_,(_,n)) <- d]
children _ = []

doMove :: (Eq (Move g), Show (Move g)) => Move g -> GameTree g -> GameTree g
doMove m (Decision _ es) = fromMaybe (error ("move not found: " ++ show m)) 
                                     (lookup m es)
doMove m (Chance d) = fromMaybe (error ("move not found: " ++ show m))
                                (lookup m [e | (_,e) <- d])

-- Nodes in BFS order.
bfs :: GameTree g -> [GameTree g]
bfs t = let b [] = []
            b ns = ns ++ b (concatMap children ns)
        in b [t]

-- Nodes DFS order.
dfs :: GameTree g -> [GameTree g]
dfs t = t : concatMap dfs (children t)

-- The highest numbered player in this finite game tree.
maxPlayer :: GameTree g -> Int
maxPlayer t = foldl1 max $ map player (dfs t)
  where player (Decision p _) = p
        player _ = 0

---------------
-- Instances --
---------------

instance Eq (Move g) => Eq (GameTree g) where
  (Decision p es) == (Decision q fs) = p == q && es == fs
  (Chance d) == (Chance e) = d == e 
  (Payoff p) == (Payoff q) = p == q
  _ == _ = False

instance Show (Move g) => Show (GameTree g) where
  show t = condense (Tree.drawTree (tree "" t))
    where str (Decision p es) = "Player " ++ show p
          str (Chance d) = "Chance"
          str (Payoff (ByPlayer vs)) = show vs
          sub (Decision p es) = [tree (show m ++ " -> ") t | (m,t) <- es]
          sub (Chance d) = [tree (show i ++ " * " ++ show m ++ " -> ") t | (i,(m,t)) <- d]
          sub (Payoff _) = []
          tree pre t = Tree.Node (pre ++ str t) (sub t)
          condense s = let empty = not . and . map (\c -> c == ' ' || c == '|')
                       in unlines $ filter empty $ lines s
