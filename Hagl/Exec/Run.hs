{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Exec.Run where

import Control.Monad.State
import Data.Maybe

import Hagl.Exec
import Hagl.Exec.Util
import Hagl.Game
import Hagl.Lists

--------------------
-- Game Execution --
--------------------

evalGame :: Game g => g -> [Player g] -> ExecM g a -> IO a
evalGame g ps f = evalStateT (unE f) (initState g ps)

runGame :: Game g => g -> [Player g] -> ExecM g a -> IO (Exec g)
runGame g ps f = execStateT (unE f) (initState g ps)

runStrategy :: Player g -> ExecM g (Move g, Player g)
runStrategy (Player n s f) = do (m, s') <- runStateT (unS f) s
                                return (m, Player n s' f)

step :: (Game g, Eq (Move g), Show (Move g)) => ExecM g ()
step = get >>= \state ->
    let (Node _ t) = _current state in case t of
      Decision i es ->
        let (ph, (p:pt)) = splitAt (i-1) $ _players state 
        in do (m, p') <- runStrategy p
              put state { _current = fromMaybe (error ("No such move: " ++ show m)) 
                                               (lookup m es),
                          _players = ph ++ p' : pt,
                          _events = DecisionEvent i m : _events state }
      Chance d ->
        do (m, t) <- fromDist d
           put state { _current = t,
                       _events = ChanceEvent m : _events state }
      Payoff vs ->
        let events = PayoffEvent vs : _events state
            summary = summarize (_game state) events
            history = ByGame $ (events, summary) : toList (_history state)
        in put state { _current = gameTree (_game state),
                       _events = [],
                       _history = history }

once :: (Game g, Eq (Move g), Show (Move g)) => ExecM g ()
once = do (Node _ t) <- liftM _current get
          case t of
            Payoff _ -> step
            _ -> step >> once 
                       
times :: (Game g, Eq (Move g), Show (Move g)) => Int -> ExecM g ()
times 0 = return ()
times n = once >> times (n-1)

---------------
-- Utilities --
---------------

initState :: Game g => g -> [Player g] -> Exec g
initState g ps = Exec g (gameTree g) ps [] (ByGame [])
