{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Game.Normal where

import Data.List
import Data.Maybe

import Hagl.Lists
import Hagl.Game

data Normal mv = Normal Int (ByPlayer [mv]) [Payoff] deriving (Eq, Show)

instance Eq mv => Game (Normal mv) where

  type Move (Normal mv) = mv
  type State (Normal mv) = ()

  info = simultaneous
  numPlayers (Normal np _ _) = np
  
  gameTree g = ply (numPlayers g) []
    where ply :: Int -> [mv] -> GameTree (Normal mv) -- GHC "panics" without this type def...
          ply 0 ms = payoff (g `pays` ByPlayer ms)
          ply p ms = decision p [(m, ply (p-1) (m:ms)) | m <- moves g p]


type Profile mv = ByPlayer mv -- pure strategy profile

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

-- Construct a two-player Normal-Form game, where each player has the same moves.
matrix :: Eq mv => [mv] -> [[Float]] -> Normal mv
matrix ms = normal 2 [ms,ms]

-- Construct a two-player Zero-Sum game, where each player has the same moves.
zerosum :: Eq mv => [mv] -> [Float] -> Normal mv
zerosum ms vs = matrix ms [[v, -v] | v <- vs]

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

-- Finds all payoff dominant solutions.
dominant :: Eq mv => Normal mv -> [Profile mv]
dominant g = nash g `intersect` pareto g

---------------------
-- Pretty Printing --
---------------------

-- TODO

-- Examples

data CD = C | D deriving (Eq, Show)

pd   = matrix [C,D] [[2,2],[0,3],[3,0],[1,1]]
stag = matrix [C,D] [[3,3],[0,2],[2,0],[1,1]]
