{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Exec where

import Control.Monad
import Control.Monad.State hiding (State)
import Data.Maybe
import Data.List
import System.Random

import Hagl.Core

--------------------
-- Data Accessors --
--------------------

--
-- Core accessors
--

game :: (Game g, GameM m g) => m g
game = liftM _game getExec

players :: (Game g, GameM m g) => m [Player g]
players = liftM _players getExec

gameState :: (Game g, GameM m g) => m (State g)
gameState = liftM _gameState getExec

playerIx :: (Game g, GameM m g) => m (Maybe PlayerIx)
playerIx = liftM _playerIx getExec

transcript :: (Game g, GameM m g) => m (Transcript g)
transcript = liftM _transcript getExec

history :: (Game g, GameM m g) => m (History g)
history = liftM _history getExec

--
-- Accessors that do some processing
--

myIx :: (Game g, GameM m g) => m PlayerIx
myIx = liftM (fromMaybe (error "playerIx not set")) playerIx

numGames :: (Game g, GameM m g) => m Int
numGames = liftM (length . toList) history

numPlayers :: (Game g, GameM m g) => m Int
numPlayers = liftM length players

-- True if this is the first iteration in this execution instance.
isFirstGame :: (Game g, GameM m g) => m Bool
isFirstGame = liftM (null . toList) history

-- Transcript of each game.
transcripts :: (Game g, GameM m g) => m (ByGame (Transcript g))
transcripts = do h  <- history
                 t' <- transcript
                 return (ByGame (t' : [t | (t,_) <- toList h]))

-- Summary of each completed game.
summaries :: (Game g, GameM m g) => m (ByGame (Summary g))
summaries = do h <- history
               return (ByGame [s | (_,s) <- toList h])

-- All moves made by each player in each game (including the current one).
moves :: (Game g, GameM m g) => m (ByGame (ByPlayer [Move g]))
moves = do ss  <- summaries
           ms' <- movesSoFar
           let mss = [ms | (ms,_) <- toList ss]
            in return (ByGame (ms':mss))

-- The last move by each player in each game (including the current one, if applicable).
move :: (Game g, GameM m g) => m (ByGame (ByPlayer (Move g)))
move = liftM (ByGame . map (ByPlayer . map head) . toList2) moves

-- The payoff for each player for each game.
payoff :: (Game g, GameM m g) => m (ByGame Payoff)
payoff = liftM (ByGame . snd . unzip . toList) summaries

-- The current score of each player.
score :: (Game g, GameM m g) => m Payoff
score = liftM (ByPlayer . map sum . transpose . toList2) payoff

-- The moves so far this game, by player.
movesSoFar :: (Game g, GameM m g) => m (ByPlayer [Move g])
movesSoFar = do np <- numPlayers
                t <- transcript
                return (ByPlayer [forp t i | i <- [1..np]])
  where forp t i = [mv | (mi,mv) <- t, mi == Just i]

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

--
-- Utility functions.
--

-- Generate a string showing a set of players' scores.
scoreString :: [Player g] -> [Float] -> String 
scoreString ps vs = unlines ["  "++show p++": "++show v | (p,v) <- zip ps vs]
