{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Strategy.Accessor where

import Control.Monad
import Data.List

import Hagl.Exec
import Hagl.Exec.Util
import Hagl.Lists
import Hagl.Game

--------------------
-- Data Accessors --
--------------------

game :: (Game g, GameM m g) => m g
game = liftM _game getExec

players :: (Game g, GameM m g) => m [Player g]
players = liftM _players getExec

location :: (Game g, GameM m g) => m (Info g)
location = getExec >>= \s -> return $ info (_game s) (_current s)

transcript :: (Game g, GameM m g) => m (Transcript g)
transcript = liftM _transcript getExec

history :: (Game g, GameM m g) => m (History g)
history = liftM _history getExec

numGames :: (Game g, GameM m g) => m Int
numGames = liftM (length . toList) history

availMoves :: (Game g, GameM m g, Eq (Move g)) => m [Move g]
availMoves = location >>= \l -> case l of
  Perfect t -> return $ movesFrom t
  Imperfect ts -> return $ foldl1 intersect (map movesFrom ts)
  Simultaneous -> liftM (movesFrom . _current) getExec

-- True if this is the first iteration in this execution instance.
isFirstGame :: (Game g, GameM m g) => m Bool
isFirstGame = liftM (null . toList) history

-- Transcript of each game.
transcripts :: (Game g, GameM m g) => m (ByGame (Transcript g))
transcripts = do t <- transcript
                 h <- history
                 return (ByGame (t : (fst . unzip . toList) h))

-- Summary of each game.
summaries :: (Game g, GameM m g) => m (ByGame (Summary g))
summaries = do g <- game
               t <- transcript
               h <- history
               return (ByGame (summarize g t : (snd . unzip . toList) h))

-- All moves made by each player in each game.
moves :: (Game g, GameM m g) => m (ByGame (ByPlayer [Move g]))
moves = liftM (ByGame . fst . unzip . toList) summaries

-- The last move by each player in each game.
move :: (Game g, GameM m g) => m (ByGame (ByPlayer (Move g)))
move = liftM (ByGame . map (ByPlayer . map head) . toList2) moves

-- The payoff for each player for each game.
payoff :: (Game g, GameM m g) => m (ByGame (ByPlayer Float))
payoff = liftM (ByGame . snd . unzip . toList) summaries

-- The current score of each player.
score :: (Game g, GameM m g) => m (ByPlayer Float)
score = liftM (ByPlayer . map sum . transpose . toList2) payoff

