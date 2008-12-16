{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Game.Normal where

import Data.List
import Data.Maybe

import Hagl.Strategy.Accessor hiding (moves, numPlayers)
import Hagl.Lists
import Hagl.Types
import Hagl.Game

data Normal mv = Normal Int (ByPlayer [mv]) [Payoff] deriving (Eq, Show)

{-
instance Eq mv => Game (Normal mv) where

  type Move (Normal mv) = mv
  type State (Normal mv) = ()

  info = simultaneous
  numPlayers (Normal np _ _) = np
  
  gameTree g = ply (numPlayers g) []
    where ply :: Int -> [mv] -> GameTree (Normal mv) -- GHC "panics" without this type def...
          ply 0 ms = pay (g `pays` ByPlayer ms)
          ply p ms = decide p [(m, ply (p-1) (m:ms)) | m <- moves g p]
-}

instance Eq mv => Game (Normal mv) where
  type Move (Normal mv) = mv
  type State (Normal mv) = ()
  initState _ = ()
  runGame = do g <- game
               ms <- allPlayers decide
               return (g `pays` ms)

type Profile mv = ByPlayer mv -- pure strategy profile

numPlayers :: Normal mv -> Int
numPlayers (Normal np _ _) = np

pays :: Eq mv => Normal mv -> Profile mv -> Payoff
pays (Normal _ mss ps) ms = fromJust (lookup ms (zip (dcross mss) ps))

moves :: Normal mv -> PlayerIx -> [mv]
moves (Normal _ mss _) p = mss `forPlayer` p

-- A list of all pure strategy profiles.
profiles :: Normal mv -> [Profile mv]
profiles (Normal _ mss _) = dcross mss

------------------------
-- Smart Constructors --
------------------------

-- Smart constructor to build from bare lists.
normal :: Eq mv => Int -> [[mv]] -> [[Float]] -> Normal mv
normal np mss vs = Normal np (ByPlayer mss) (map ByPlayer vs)

-- Construct a two-player game.
bimatrix :: Eq mv => [[mv]] -> [[Float]] -> Normal mv
bimatrix = normal 2

-- Construct a two-player, zero-sum game.
matrix :: Eq mv => [[mv]] -> [Float] -> Normal mv
matrix mss vs = bimatrix mss (zerosum vs)

-- Construct a two-player, symmetric game.
symmetric :: Eq mv => [mv] -> [Float] -> Normal mv
symmetric ms vs = normal 2 [ms, ms] vs'
  where sym = concat (transpose (chunk (length ms) vs))
        vs' = zipWith (\a b -> [a,b]) vs sym

-- Construct a zero-sum payoff grid.
zerosum :: [Float] -> [[Float]]
zerosum vs = [[v, -v] | v <- vs]

---------------------------
-- Equilibrium Solutions --
---------------------------

-- Finds all pure nash equilibrium solutions..
nash :: Eq mv => Normal mv -> [Profile mv]
nash g = [s | s <- profiles g, stable s]
  where stable s = all (uni s) [1 .. numPlayers g]
        uni s p = and [g `pays` s `forPlayer` p >= 
                       g `pays` s' `forPlayer` p | s' <- change s p]
        change (ByPlayer s) p = let (h,_:t) = splitAt (p-1) s 
                                in [ByPlayer (h ++ e:t) | e <- moves g p]

-- Finds all strong Pareto optimal solutions.
pareto :: Eq mv => Normal mv -> [Profile mv]
pareto g = [s | s <- profiles g, opt s]
  where opt s = not (any (imp s) (profiles g))
        imp s s' = let p  = toList (g `pays` s)
                       p' = toList (g `pays` s')
                   in or (zipWith (>) p' p) && and (zipWith (>=) p' p)

-- Finds all pareto optimal equilibriums.
paretoNash :: Eq mv => Normal mv -> [Profile mv]
paretoNash g = pareto g `intersect` nash g

---------------------
-- Pretty Printing --
---------------------

-- TODO

-- Examples

data CD = C | D deriving (Eq, Show)

pd   = symmetric [C,D] [2, 0, 3, 1]
stag = symmetric [C,D] [3, 0, 2, 1]
