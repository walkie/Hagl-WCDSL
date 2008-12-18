{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Exec.Run where

import Control.Monad.State
import Data.Maybe

import Hagl.Core
import Hagl.Exec

--------------------
-- Game Execution --
--------------------

conclude :: Game g => Payoff -> ExecM g ()
conclude p = 
  do e <- getExec
     t <- transcript
     ms <- movesSoFar
     put e { _gameState = initState (_game e),
             _transcript = [],
             _history = ByGame ((t,(ms,p)) : toList (_history e)) }

once :: (Game g) => ExecM g ()
once = do p <- runGame
          conclude p

times :: (Game g) => Int -> ExecM g ()
times 0 = return ()
times n = once >> times (n-1)
