{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances #-}

module Hagl.Extensive where

import Data.List

import Hagl.Core
import Hagl.Game
import Hagl.GameTree

type Tree mv = GameTree (Extensive mv)
type TreeEdge mv = Edge (Extensive mv)

data Extensive mv = Extensive Int (Tree mv -> Info (Extensive mv)) (Tree mv) 

-- Information Groups
data Info g = Perfect (GameTree g)
            | Imperfect [GameTree g]
            | Simultaneous

-- Construct a perfect information game from a finite GameTree.
extensive :: GameTree (Extensive mv) -> Extensive mv
extensive t = Extensive (maxPlayer t) Perfect t

-- Construct a decision node with only one option.
player :: PlayerIx -> TreeEdge mv -> Tree mv
player i e = Decision i [e]

-- Combines two game trees.
(<+>) :: Tree mv -> Tree mv -> Tree mv
Decision a as <+> Decision b bs | a == b = Decision a (as ++ bs)
Chance as <+> Chance bs = Chance (as ++ bs)
Payoff as <+> Payoff bs = Payoff (dzipWith (+) as bs)

-- Add a decision branch to a game tree.
(<|>) :: Tree mv -> TreeEdge mv -> Tree mv
Decision i ms <|> m = Decision i (m:ms)

---------------
-- Instances --
---------------

instance (Eq mv, Show mv) => Game (Extensive mv) where
  type Move (Extensive mv) = mv
  type State (Extensive mv) = Tree mv
  initState (Extensive _ _ t) = t
  runGame = runTree

instance (Eq mv, Show mv) => Searchable (Extensive mv) where
  gameTree _ s = s
  nextState _ s m = doMove m s

-- Eq
instance Eq mv => Eq (Extensive mv) where
  (Extensive n1 _ t1) == (Extensive n2 _ t2) = n1 == n2 && t1 == t2

instance (Eq (Move g), Eq (State g)) => Eq (Info g) where
  (Perfect t1) == (Perfect t2) = t1 == t2
  (Imperfect t1) == (Imperfect t2) = t1 == t2
  Simultaneous == Simultaneous = True
  _ == _ = False

-- Show
instance Show mv => Show (Extensive mv) where
  show (Extensive _ _ t) = show t

instance Show (Move g) => Show (Info g) where
  show (Perfect t) = show t
  show (Imperfect ts) = unlines $ intersperse "*** OR ***" (map show ts)
  show Simultaneous = "Cannot show this location in the game tree."
