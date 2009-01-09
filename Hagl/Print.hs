{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Print where

import Control.Monad.State
import Data.List

import Hagl.Core
import Hagl.Accessor

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
  do t <- transcripts `forGameM` n
     p <- payoff `forGameM` n
     ps <- players
     printStrLn $ "Game "++show n++":"
     let str (Just i, m) = "  " ++ show (ps !! (i-1)) ++ "'s move: " ++ show m
         str (Nothing, m) = "  Chance: " ++ show m
      in printStr $ unlines $ map str (reverse t) ++ ["  Payoff: " ++ show (toList p)]

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
printScore = do s  <- score
                ps <- players
                printStrLn "Score:"
                printStr (scoreString ps (toList s))

-----------------------
-- Utility functions --
-----------------------

-- Generate a string showing a set of players' scores.
scoreString :: [Player g] -> [Float] -> String 
scoreString ps vs = unlines ["  "++show p++": "++show v | (p,v) <- zip ps vs]
