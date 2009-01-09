module Hagl.Exec where

import Control.Monad.State

import Hagl.Core
import Hagl.Accessor
import Hagl.Selector

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
