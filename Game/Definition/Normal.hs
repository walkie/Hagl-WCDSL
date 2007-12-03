module Game.Definition.Normal where

import Data.Maybe
import Game.Definition

-- Construct a game from a Normal-Form definition
normal :: (Eq m) => Int -> [m] -> [[v]] -> GameTree m v
normal np ms vs =
    let turns n = [Turn n (turns n) (zip ms ns) | ns <- chunk (length ms) (nodes (n+1))]
        nodes n | n > np = [Payoff v | v <- vs]
        nodes n | n <= np = map Decision (turns n)
    in head $ nodes 1

-- Construct a two-player Normal-Form game.
matrix :: (Eq m) => [m] -> [[v]] -> GameTree m v
matrix = normal 2

-- Construct a two-player Zero-Sum game.
zerosum :: (Eq m, Num v) => [m] -> [v] -> GameTree m v
zerosum ms vs = normal 2 ms (map (\v -> [v, -v]) vs)

-- Split a list into n sized chunks.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = (take n l) : chunk n (drop n l)
