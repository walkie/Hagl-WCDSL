module Game.Execution.Tournament where

import Control.Monad.State
import Data.List
import Game.Definition
import Game.Execution
import Game.Execution.Run
import Game.Execution.Util
import Game.Strategy

runGames :: (Show m, Num v) => [[Player m v]] -> Game m v -> GameExec m v a -> IO ()
runGames pss g f = 
    let unique = nub $ concat pss
        run ps = runGame g ps f >>= \end -> evalStateT (our score) end
    in sequence (map run pss) >>= \vss ->
         let pis = map (flip elemIndices (concat pss)) unique
             vs =  map (sum . map ((concat vss) !!)) pis
         in do putStrLn "Final Scores:"
               putStr $ scoreString unique vs

tournament :: (Show m, Num v) => [Player m v] -> Game m v -> GameExec m v a -> IO ()
tournament as = runGames [[a] | a <- as]

tournament2 :: (Show m, Num v) => [Player m v] -> [Player m v] -> Game m v -> GameExec m v a -> IO ()
tournament2 as bs = runGames [[a,b] | a <- as, b <- bs]

tournament3 :: (Show m, Num v) => [Player m v] -> [Player m v] -> [Player m v] -> Game m v -> GameExec m v a -> IO ()
tournament3 as bs cs = runGames [[a,b,c] | a <- as, b <- bs, c <- cs]

tournament4 :: (Show m, Num v) => [Player m v] -> [Player m v] -> [Player m v] -> [Player m v] -> Game m v -> GameExec m v a -> IO ()
tournament4 as bs cs ds = runGames [[a,b,c,d] | a <- as, b <- bs, c <- cs, d <- ds]

-----------------
-- Round-Robin --
-----------------

-- Run every combination of players (including against self). 
-- For two-player games where the order of play is not significant.
roundRobin :: (Show m, Num v) => [Player m v] -> Game m v -> GameExec m v a -> IO ()
roundRobin ps = runGames $ nub $ map sort [[a,b] | a <- ps, b <- ps]

-- Run every combination of players (including games where multiple players are same. 
-- For three-player games where the order of play is not significant.
roundRobin3 :: (Show m, Num v) => [Player m v] -> Game m v -> GameExec m v a -> IO ()
roundRobin3 ps = runGames $ nub $ map sort [[a,b,c] | a <- ps, b <- ps, c <- ps]

-- Run every combination of players (including games where multiple players are same. 
-- For four-player games where the order of play is not significant.
roundRobin4 :: (Show m, Num v) => [Player m v] -> Game m v -> GameExec m v a -> IO ()
roundRobin4 ps = runGames $ nub $ map sort [[a,b,c,d] | a <- ps, b <- ps, c <- ps, d <- ps]

-- Run every combination of players in every order. For two-player games.
fullRoundRobin :: (Show m, Num v) => [Player m v] -> Game m v -> GameExec m v a -> IO ()
fullRoundRobin ps = runGames [[a,b] | a <- ps, b <- ps]

-- Run every combination of players in every order. For three-player games.
fullRoundRobin3 :: (Show m, Num v) => [Player m v] -> Game m v -> GameExec m v a -> IO ()
fullRoundRobin3 ps = runGames [[a,b,c] | a <- ps, b <- ps, c <- ps]

-- Run every combination of players in every order. For four-player games.
fullRoundRobin4 :: (Show m, Num v) => [Player m v] -> Game m v -> GameExec m v a -> IO ()
fullRoundRobin4 ps = runGames [[a,b,c,d] | a <- ps, b <- ps, c <- ps, d <- ps]
