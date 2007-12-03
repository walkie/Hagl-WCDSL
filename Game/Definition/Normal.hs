module Game.Definition.Normal where

import Data.Maybe
import Game.Definition

-- Construct a game from a Normal-Form definition
normal :: (Eq m) => Int -> [m] -> [[v]] -> GameDef Int m v
normal np moves values =
    let nexts ns m = fromJust (lookup m (zip moves ns))
        nodes n | n > np = [Payoff v End | v <- values]
                | n <= np = [Turn n n (nexts ns) 
                              | ns <- chunk (length moves) (nodes (n+1))]
    in GameDef np (head (nodes 1)) nodes (\_ -> moves)

-- Construct a two-player Normal-Form game.
matrix :: (Eq m) => [m] -> [[v]] -> GameDef Int m v
matrix = normal 2

-- Construct a two-player Zero-Sum game.
zerosum :: (Eq m, Num v) => [m] -> [v] -> GameDef Int m v
zerosum ms vs = normal 2 ms (map (\v -> [v, -v]) vs)

-- Split a list into n sized chunks.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = (take n l) : chunk n (drop n l)
