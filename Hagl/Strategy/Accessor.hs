{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Strategy.Accessor where

import Control.Monad
import Data.List
import Data.Maybe

import Hagl.Lists
import Hagl.Types
import Hagl.Exec.Util

--------------------
-- Data Accessors --
--------------------

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

myIx :: (Game g, GameM m g) => m PlayerIx
myIx = liftM fromJust playerIx

numGames :: (Game g, GameM m g) => m Int
numGames = liftM (length . toList) history

numPlayers :: Game g => ExecM g Int
numPlayers = liftM length players

{-
availMoves :: (Game g, GameM m g) => m [Move g]
availMoves = do g <- game
                s <- gameState
                return (movesFrom g s)
-}

-- True if this is the first iteration in this execution instance.
isFirstGame :: (Game g, GameM m g) => m Bool
isFirstGame = liftM (null . toList) history

-- Transcript of each game.
transcripts :: (Game g, GameM m g) => m (ByGame (Transcript g))
transcripts = liftM (ByGame . fst . unzip . toList) history

-- Summary of each game.
summaries :: (Game g, GameM m g) => m (ByGame (Summary g))
summaries = liftM (ByGame . snd . unzip . toList) history

-- All moves made by each player in each game.
moves :: (Game g, GameM m g) => m (ByGame (ByPlayer [Move g]))
moves = liftM (ByGame . fst . unzip . toList) summaries

-- The last move by each player in each game.
move :: (Game g, GameM m g) => m (ByGame (ByPlayer (Move g)))
move = liftM (ByGame . map (ByPlayer . map head) . toList2) moves

-- The payoff for each player for each game.
payoff :: (Game g, GameM m g) => m (ByGame Payoff)
payoff = liftM (ByGame . snd . unzip . toList) summaries

-- The current score of each player.
score :: (Game g, GameM m g) => m Payoff
score = liftM (ByPlayer . map sum . transpose . toList2) payoff

