module Game.Execution.Util where

import Control.Monad.State
import Game.Execution
import System.Random

-- Generate a string showing a set of players' scores.
scoreString :: (Show m) => [Player m] -> [Float] -> String 
scoreString ps vs = unlines ["  "++show p++": "++show v | (p,v) <- zip ps vs]

randomIndex :: [a] -> GameExec m Int
randomIndex as = liftIO $ getStdRandom $ randomR (0, length as - 1)

expandDist :: [(Int, a)] -> [a]
expandDist dist = concatMap (\(i, a) -> take i (repeat a)) dist

branch :: [(Int, a)] -> Int -> Int
branch ((i, _):r) n | n < i = 1
                    | otherwise = 1 + branch r (n-i)
