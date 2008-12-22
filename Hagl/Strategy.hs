{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Strategy where

import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Maybe

import Hagl.Core
import Hagl.GameTree
import Hagl.Exec
import Hagl.Exec.Util
import Hagl.Strategy.Selector
import Hagl.Searchable

-----------------------
-- Common Strategies --
-----------------------

-- Play a move.
play :: Move g -> Strategy g s
play = return

-- Construct a pure strategy. Always play the same move.
pure :: Move g -> Strategy g s
pure = return

-- Pick a move from the list of available moves randomly.
randomly :: (Searchable g, Eq (Move g)) => Strategy g s
randomly = do t <- gameTreeM
              randomlyFrom (movesFrom t)

-- Construct a mixed strategy. Play moves based on a distribution.
mixed :: [(Int, Move g)] -> Strategy g s
mixed = randomlyFrom . expandDist

-- Perform some pattern of moves periodically.
periodic :: (Game g, Show (Move g)) => [Move g] -> Strategy g s
periodic ms = numMoves >>= \n -> return $ ms !! mod n (length ms)

-- Begin a list of strategies.
atFirst :: (Game g, Show (Move g)) => Strategy g s -> [Strategy g s] -> Strategy g s
atFirst s ss = numMoves >>= \n ->
    let l = length (s:ss)
    in (s:ss) !! (if n < l then n else l - 1)

-- Next in a list of strategies.
next :: Strategy g s -> [Strategy g s] -> [Strategy g s]
next = (:)

-- End a list of strategies.
finally :: Strategy g s -> [Strategy g s]
finally = (:[])

-- Play a strategy for the first move, then another strategy thereafter.
atFirstThen :: (Game g, Show (Move g)) => Strategy g s -> Strategy g s -> Strategy g s
atFirstThen a b = atFirst a (finally b)

-- Play an initial move, then another strategy thereafter.
initiallyThen :: (Game g, Show (Move g)) => Move g -> Strategy g s -> Strategy g s
initiallyThen a b = atFirst (return a) (finally b)

-- Minimax algorithm with alpha-beta pruning. Only defined for games with
-- perfect information and no Chance nodes.
minimax :: Searchable g => Strategy g s
minimax = myIx >>= \me -> gameTreeM >>= \t -> 
  let isMe = (me + 1 ==)
      val alpha beta n@(Decision p _)
         | alpha >= beta = if isMe p then alpha else beta
         | otherwise =
             let mm (a,b) n = let v = val a b n
                              in if isMe p then (max a v, b)
                                           else (a, min b v)
                 (alpha', beta') = foldl mm (alpha, beta) (children n)
             in if isMe p then alpha' else beta'
      val _ _ (Payoff vs) = vs `forPlayer` me
  in let vals = map (val (-infinity) infinity) (children t)
     in return $ movesFrom t !! maxIndex vals

infinity :: Float
infinity = 1/0

---------------
-- Utilities --
---------------

maxIndex :: (Ord a) => [a] -> Int
maxIndex as = fromJust $ elemIndex (maximum as) as

numMoves :: (Game g, GameM m g, Show (Move g)) => m Int
numMoves = liftM (length . concat) (my `each` every moves)
