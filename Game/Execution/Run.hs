module Game.Execution.Run where

import Control.Monad.State
import Data.List
import Game.Definition
import Game.Execution
import Game.Execution.Strategy


------------------------
-- Printing Functions --
------------------------

printLocation :: (Show m, Show v) => Game d m v ()
printLocation =
    do loc <- location
       ms <- getAvailMoves
       liftIO $ putStr $ showTree ms loc

printTranscript :: (Show m, Num v) => Game d m v ()
printTranscript = 
    do n <- numGames
       let game i | i == n = printTranscriptOfGame i
                  | otherwise = printTranscriptOfGame i >> game (i+1)
        in game 1

printTranscriptOfGame :: (Show m, Num v) => Int -> Game d m v ()
printTranscriptOfGame n =
    do ByGame h <- history
       ps <- players
       liftIO $ putStrLn $ "Game "++show n++ ":"
       let g = reverse $ h !! (length h - n)
        in liftIO $ putStr $ unlines $ map (showEvent ps) g
  where showEvent ps (MoveEvent i m) = "  " ++ show (ps !! (i-1)) ++ "'s move: " ++ show m
        showEvent _ (PayoffEvent vs) = "  Payoff: " ++ show vs

printSummaries :: (Show m, Num v) => Game d m v ()
printSummaries =
    do n <- numGames
       let game i | i == n = printSummaryOfGame i
                  | otherwise = printSummaryOfGame i >> game (i+1)
        in game 1

printSummaryOfGame :: (Show m, Num v) => Int -> Game d m v ()
printSummaryOfGame n = 
    do ByGame ss <- summaries
       ps <- players
       liftIO $ putStrLn $ "Summary of Game "++show n++":"
       let (ByPlayer mss, ByPlayer vs) = ss !! (length ss - n)
        in do liftIO $ putStr $ unlines ["  "++show p++" moves: "++show ms | (p,ms) <- zip ps mss]
              liftIO $ putStrLn $ "  Score: "++show vs

printScore :: (Show m, Num v) => Game d m v ()
printScore = do s <- liftM2 scoreString players (our score)
                liftIO $ putStrLn "Score:"
                liftIO $ putStr s

