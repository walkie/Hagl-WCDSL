{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Strategy where

import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Maybe

import Hagl.Game
import Hagl.Exec
import Hagl.Exec.Util
import Hagl.Lists
import Hagl.Strategy.Accessor
import Hagl.Strategy.Selector
import Hagl.Util

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
randomly :: (Game g, Eq (Move g)) => Strategy g s
randomly = randomlyFrom =<< availMoves

-- Pick a move randomly from a list.
randomlyFrom :: [Move g] -> Strategy g s
randomlyFrom as = liftM (as !!) (randomIndex as)

-- Construct a mixed strategy. Play moves based on a distribution.
mixed :: [(Int, Move g)] -> Strategy g s
mixed = randomlyFrom . expandDist

-- Perform some pattern of moves periodically.
periodic :: Game g => [Move g] -> Strategy g s
periodic ms = numMoves >>= \n -> return $ ms !! mod n (length ms)

-- Begin a list of strategies.
atFirst :: Game g => Strategy g s -> [Strategy g s] -> Strategy g s
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
atFirstThen :: Game g => Strategy g s -> Strategy g s -> Strategy g s
atFirstThen a b = atFirst a (finally b)

-- Play an initial move, then another strategy thereafter.
initiallyThen :: Game g => Move g -> Strategy g s -> Strategy g s
initiallyThen a b = atFirst (return a) (finally b)

-- Minimax algorithm with alpha-beta pruning. Only defined for games with
-- perfect information and no Chance nodes.
minimax :: Game g => Strategy g s
minimax = myIx >>= \me -> location >>= \loc ->
  let isMe = (me + 1 ==)
      val alpha beta n@(Node _ (Decision p _))
         | alpha >= beta = if isMe p then alpha else beta
         | otherwise =
             let mm (a,b) n = let v = val a b n
                              in if isMe p then (max a v, b)
                                           else (a, min b v)
                 (alpha', beta') = foldl mm (alpha, beta) (children n)
             in if isMe p then alpha' else beta'
      val _ _ (Node _ (Payoff vs)) = vs `forPlayer` me
  in case loc of
       Perfect n -> 
         let vals = map (val (-infinity) infinity) (children n)
         in return $ movesFrom n !! maxIndex vals

infinity :: Float
infinity = 1/0


---------------
-- Utilities --
---------------

maxIndex :: (Ord a) => [a] -> Int
maxIndex as = fromJust $ elemIndex (maximum as) as

numMoves :: (Game g, GameM m g) => m Int
numMoves = liftM (length . concat) (my `each` every moves)
