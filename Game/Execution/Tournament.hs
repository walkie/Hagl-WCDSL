module Game.Execution.Tournament where

import Data.List
import Game.Definition
import Game.Execution
import Game.Execution.Run

--runTournament :: GameDef d m v -> 

-- Assumes two player game
roundRobin :: (Show m, Num v) => GameDef d m v -> [Player d m v] -> Game d m v a -> IO ()
roundRobin def ps f = 
    let ii = take (length ps) [0..]
        robin (is:[]) scores = 
          do s <- runGame def (map (ps !!) is) f
             return $ (zip is $ scoresFromState s) ++ scores
        robin (is:iss) scores = 
          do s <- runGame def (map (ps !!) is) f
             robin iss ((zip is $ scoresFromState s) ++ scores)
        score a (b, v) | a == b = v
                       | otherwise = 0
    in do scores <- robin [[a,b] | a <- ii, b <- ii, a /= b] []
          putStrLn "Total Payoffs:"
          putStr $ scoreString ps [sum (map (score i) scores) | i <- take (length ps) [0..]]

-- hacks for demo...
scoresFromState :: (Num v) => ExecState d m v -> [v]
scoresFromState state = map sum $ transpose $ map asList $ snd $ unzip $ asList $ _summaries state

