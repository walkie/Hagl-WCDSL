module Hagl.Exec.Util where

import Control.Monad.State
import System.Random

import Hagl.Exec
import Hagl.Game
import Hagl.Lists

fromDist :: MonadIO m => Dist a -> m a
fromDist d = let l = expandDist d
             in liftM (l !!) (liftIO $ randomRIO (0, length l))

-- Generate a string showing a set of players' scores.
scoreString :: [Player g] -> [Float] -> String 
scoreString ps vs = unlines ["  "++show p++": "++show v | (p,v) <- zip ps vs]

randomIndex :: MonadIO m => [a] -> m Int
randomIndex as = liftIO $ getStdRandom $ randomR (0, length as - 1)

-- TODO this could be cleaned up
summarize :: Game g => g -> [Event g] -> Summary g
summarize g t = 
    let np = numPlayers g
        addmove i a as = take i as ++ ((a:(as!!i)) : drop i as)
        payoffs (PayoffEvent vs : es) = zipWith (+) (toList vs) (payoffs es)
        payoffs (e : es) = payoffs es
        payoffs [] = take np (repeat 0)
        moves (DecisionEvent i m : es) = addmove (i-1) m (moves es)
        moves (e : es) = moves es
        moves [] = take np (repeat [])
    in (ByPlayer (moves t), ByPlayer (payoffs t))
