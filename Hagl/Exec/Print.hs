{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Exec.Print where

import Control.Monad.State
import Data.List

import Hagl.Exec
import Hagl.Exec.Util
import Hagl.Game
import Hagl.Lists
import Hagl.Strategy
import Hagl.Strategy.Accessor
import Hagl.Strategy.Selector

------------------------
-- Printing Functions --
------------------------

print :: (MonadIO m, Show a) => m a -> m ()
print = (>>= liftIO . putStr . show)

printLn :: (MonadIO m, Show a) => m a -> m ()
printLn = (>>= liftIO . putStrLn . show)

printStr :: MonadIO m => String -> m ()
printStr = liftIO . putStr

printStrLn :: MonadIO m => String -> m ()
printStrLn = liftIO . putStrLn

printTranscript :: (Game g, GameM m g, MonadIO m, Show (Move g)) => m ()
printTranscript = do n <- numGames
                     sequence_ $ map printTranscriptOfGame [1..n]

printTranscriptOfGame :: (Game g, GameM m g, MonadIO m, Show (Move g)) => Int -> m ()
printTranscriptOfGame n =
    do ByGame ts <- transcripts
       ps <- players
       printStrLn $ "Game "++show n++":"
       let t = reverse $ ts !! (length ts - n)
           event (DecisionEvent i m) = "  " ++ show (ps !! (i-1)) ++ "'s move: " ++ show m
           event (ChanceEvent i) = "  Chance: " ++ show i
           event (PayoffEvent vs) = "  Payoff: " ++ show vs
        in printStr $ unlines $ map event t

printSummaries :: (Game g, GameM m g, MonadIO m, Show (Move g)) => m ()
printSummaries = do n <- numGames
                    sequence_ $ map printSummaryOfGame [1..n]

printSummaryOfGame :: (Game g, GameM m g, MonadIO m, Show (Move g)) => Int -> m ()
printSummaryOfGame n = 
    do ByGame ss <- summaries
       ps <- players
       printStrLn $ "Summary of Game "++show n++":"
       let (ByPlayer mss, ByPlayer vs) = ss !! (length ss - n)
        in do printStr $ unlines ["  "++show p++" moves: "++show (reverse ms) | (p,ms) <- zip ps mss]
              printStrLn $ "  Score: "++show vs

printScore :: (Game g, GameM m g, MonadIO m, Show (Move g)) => m ()
printScore = do s <- liftM2 scoreString players (our score)
                printStrLn "Score:"
                printStr =<< liftM2 scoreString players (our score)
