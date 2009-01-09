{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Exec where

import Control.Monad
import Control.Monad.State hiding (State)
import Data.Maybe
import Data.List
import System.Random

import Hagl.Core

--
-- Functions that use the IO monad.
--

randomIndex :: MonadIO m => [a] -> m Int
randomIndex as = liftIO $ randomRIO (0, length as - 1)

-- Pick a move randomly from a list.
randomlyFrom :: MonadIO m => [a] -> m a
randomlyFrom as = liftM (as !!) (randomIndex as)

fromDist :: MonadIO m => Dist a -> m a
fromDist d = randomlyFrom (expandDist d)

--------------------
-- Game Execution --
--------------------

once :: (Game g) => ExecM g ()
once = do p <- runGame
          conclude p

times :: (Game g) => Int -> ExecM g ()
times 0 = return ()
times n = once >> times (n-1)

conclude :: Game g => Payoff -> ExecM g ()
conclude p = 
  do e <- getExec
     t <- transcript
     ms <- movesSoFar
     put e { _gameState = initState (_game e),
             _transcript = [],
             _history = ByGame ((t,(ms,p)) : toList (_history e)) }

-----------------------
-- Utility functions --
-----------------------

-- Generate a string showing a set of players' scores.
scoreString :: [Player g] -> [Float] -> String 
scoreString ps vs = unlines ["  "++show p++": "++show v | (p,v) <- zip ps vs]
