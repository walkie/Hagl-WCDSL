{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Exec.Tournament where

import Control.Monad.State
import Data.List

import Hagl.Game
import Hagl.Exec
import Hagl.Exec.Run
import Hagl.Exec.Util
import Hagl.Lists
import Hagl.Strategy
import Hagl.Strategy.Accessor
import Hagl.Strategy.Selector

-- Run a game with each successive collection of players. Aggregate the scores
-- of all Players (based on name) and print the final scores.
runGames :: (Game g, Show (Move g)) => g -> [[Player g]] -> ExecM g a -> IO ()
runGames g pss f = 
    let unique = nub $ concat pss
        run ps = evalGame g ps (f >> our score)
    in sequence (map run pss) >>= \vss ->
         let pis = map (flip elemIndices (concat pss)) unique
             vs =  map (sum . map ((concat vss) !!)) pis
             (vs', ps') = unzip $ reverse $ sortTogether vs unique
         in do putStrLn "Final Scores:"
               putStr $ scoreString ps' vs'

-- Run a tournament where all combinations of players are played
-- where player 1 comes from list 1, player 2 from list 2, etc.
tournament :: (Game g, Show (Move g)) => g -> [[Player g]] -> ExecM g a -> IO ()
tournament g pss = runGames g (cross pss)

-- Run a tournament where all orders of all players are played 
-- (including against selves).
fullRoundRobin :: (Game g, Show (Move g)) => g -> [Player g] -> ExecM g a -> IO ()
fullRoundRobin g ps = tournament g (replicate (numPlayers g) ps)

-- Run a tournament where all unique combinations of players are played 
-- (including against selves).
roundRobin :: (Game g, Show (Move g)) => g -> [Player g] -> ExecM g a -> IO ()
roundRobin g ps = runGames g (ucross (replicate (numPlayers g) ps))

-----------------------
-- Utility Functions --
-----------------------

sortTogether :: (Ord a) => [a] -> [b] -> [(a, b)]
sortTogether as bs = let f (a1,_) (a2,_) = compare a1 a2
                     in sortBy f $ zip as bs
