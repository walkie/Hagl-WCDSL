module Game.Execution.Print where

import Control.Monad.State
import Data.List
import Game.Definition
import Game.Execution
import Game.Execution.Util
import Game.Strategy

------------------------
-- Printing Functions --
------------------------

print :: (Show a) => GameExec m a -> GameExec m ()
print = (>>= liftIO . putStr . show)

printLn :: (Show a) => GameExec m a -> GameExec m ()
printLn = (>>= liftIO . putStrLn . show)

printStr :: String -> GameExec m ()
printStr = liftIO . putStr

printStrLn :: String -> GameExec m ()
printStrLn = liftIO . putStrLn

printTranscript :: (Show m) => GameExec m ()
printTranscript = do n <- numGames
                     sequence_ $ map printTranscriptOfGame [1..n]

printTranscriptOfGame :: (Show m) => Int -> GameExec m ()
printTranscriptOfGame n =
    do ByGame ts <- transcripts
       ps <- players
       printStrLn $ "Game "++show n++":"
       let t = reverse $ ts !! (length ts - n)
           event (DecisionEvent i m) = "  " ++ show (ps !! (i-1)) ++ "'s move: " ++ show m
           event (ChanceEvent i) = "  Chance: " ++ show i
           event (PayoffEvent vs) = "  Payoff: " ++ show vs
        in printStr $ unlines $ map event t

printSummaries :: (Show m) => GameExec m ()
printSummaries = do n <- numGames
                    sequence_ $ map printSummaryOfGame [1..n]

printSummaryOfGame :: (Show m) => Int -> GameExec m ()
printSummaryOfGame n = 
    do ByGame ss <- summaries
       ps <- players
       printStrLn $ "Summary of Game "++show n++":"
       let (ByPlayer mss, ByPlayer vs) = ss !! (length ss - n)
        in do printStr $ unlines ["  "++show p++" moves: "++show (reverse ms) | (p,ms) <- zip ps mss]
              printStrLn $ "  Score: "++show vs

printScore :: (Show m) => GameExec m ()
printScore = do s <- liftM2 scoreString players (our score)
                printStrLn "Score:"
                printStr =<< liftM2 scoreString players (our score)
