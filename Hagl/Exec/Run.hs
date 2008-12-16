{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Exec.Run where

import Control.Monad.State
import Data.Maybe

import Hagl.Lists
import Hagl.Types
import Hagl.Strategy.Accessor

--------------------
-- Game Execution --
--------------------

conclude :: Game g => Payoff -> ExecM g ()
conclude p = 
  do e <- getExec
     n <- numPlayers
     t <- transcript
     let s = (ByPlayer (map (forp t) [1..n]), p)
      in put e { _gameState = initState (_game e),
                 _transcript = [],
                 _history = ByGame ((t,s) : toList (_history e)) }
  where forp t i = [mv | (mi, mv) <- t, mi == Just i]

once :: (Game g) => ExecM g ()
once = do p <- runGame
          conclude p

times :: (Game g) => Int -> ExecM g ()
times 0 = return ()
times n = once >> times (n-1)

{-
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
                          _transcript = DecisionEvent i m : _transcript state }
      Chance d ->
        do (m, t) <- fromDist d
           put state { _current = t,
                       _transcript = ChanceEvent m : _transcript state }
      Payoff vs ->
        let events = PayoffEvent vs : _transcript state
            summary = summarize (_game state) events
            history = ByGame $ (events, summary) : toList (_history state)
        in put state { _current = gameTree (_game state),
                       _transcript = [],
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
-}
