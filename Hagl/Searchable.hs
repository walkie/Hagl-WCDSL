{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Searchable where

import Control.Monad
import Control.Monad.State hiding (State)
import Data.List
import Data.Maybe

import Hagl.Core
import Hagl.Exec
import Hagl.Game
import Hagl.GameTree

----------------------
-- Searchable Class --
----------------------

class Game g => Searchable g where
  gameTree :: g -> State g -> GameTree g
  nextState :: g -> State g -> Move g -> State g
  --gameTree  :: ExecM g (GameTree g)
  --nextState :: Move g -> ExecM g (State g)

gameTreeM :: (Searchable g, GameM m g) => m (GameTree g)
gameTreeM = do g <- game
               s <- gameState
               return (gameTree g s)

nextStateM :: (Searchable g, GameM m g) => Move g -> m (State g)
nextStateM m = do g <- game
                  s <- gameState
                  return (nextState g s m)

treeStep :: (Searchable g, Eq (Move g), Show (Move g)) => ExecM g (Maybe Payoff)
treeStep = gameTreeM >>= \t -> case t of
  Decision i es ->
    do m <- decide i 
       s <- nextStateM m
       putGameState s
       return Nothing
  Chance d ->
    do (m, _) <- fromDist d
       chanceMoved m
       s <- nextStateM m 
       putGameState s
       return Nothing
  Payoff p -> return (Just p)

runTree :: (Searchable g, Eq (Move g), Show (Move g)) => ExecM g Payoff
runTree = treeStep >>= maybe runTree return
