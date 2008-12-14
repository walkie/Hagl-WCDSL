{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Game.Extensive where

import Hagl.Lists
import Hagl.Game

type Tree mv = GameTree (Extensive mv)

data Extensive mv = Extensive Int (Tree mv -> Info (Extensive mv)) (Tree mv) 

instance Eq mv => Game (Extensive mv) where

  type Move (Extensive mv) = mv
  type State (Extensive mv) = ()

  numPlayers (Extensive n _ _) = n
  info       (Extensive _ i _) = i
  gameTree   (Extensive _ _ t) = t

-- Construct a perfect information game from a finite GameTree.
extensive :: GameTree (Extensive mv) -> Extensive mv
extensive t = Extensive (maxPlayer t) Perfect t

-- Construct a decision node with only one option.
player :: PlayerIx -> (mv, Tree mv) -> Tree mv
player i e = Node () (Decision i [e])

-- Combines two game trees.
(<+>) :: Tree mv -> Tree mv -> Tree mv
(Node () t1) <+> (Node () t2) = Node () (t1 <++> t2)
  where Decision a as <++> Decision b bs | a == b = Decision a (as ++ bs)
        Chance as <++> Chance bs = Chance (as ++ bs)
        Payoff as <++> Payoff bs = Payoff 
          (ByPlayer (zipWith (+) (toList as) (toList bs)))

-- Add a decision branch to a game tree.
(<|>) :: Tree mv -> (mv, Tree mv) -> Tree mv
(Node () (Decision i ms)) <|> m = Node () (Decision i (m:ms))

instance Eq mv => Eq (Extensive mv) where
  (Extensive n1 _ t1) == (Extensive n2 _ t2) = n1 == n2 && t1 == t2

instance Show mv => Show (Extensive mv) where
  show (Extensive _ _ t) = show t
