module Hagl.Exec.Util where

import Control.Monad.State
import Data.Maybe
import System.Random

import Hagl.Lists
import Hagl.Types

fromDist :: MonadIO m => Dist a -> m a
fromDist d = let l = expandDist d
             in liftM (l !!) (liftIO $ randomRIO (0, length l))

-- Generate a string showing a set of players' scores.
scoreString :: [Player g] -> [Float] -> String 
scoreString ps vs = unlines ["  "++show p++": "++show v | (p,v) <- zip ps vs]

randomIndex :: MonadIO m => [a] -> m Int
randomIndex as = liftIO $ getStdRandom $ randomR (0, length as - 1)

-- TODO this could be cleaned up
