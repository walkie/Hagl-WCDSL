{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Game.Normal where

import Data.List
import Data.Maybe

import Hagl.Core
import Hagl.Game
import Hagl.Exec hiding (numPlayers, moves)

type Profile mv = ByPlayer mv -- pure strategy profile

-- A Normal form game (extends Game type class)
class Game g => Normal g where
  numPlayers :: g -> Int
  pays       :: g -> Profile (Move g) -> Payoff
  moves      :: g -> PlayerIx -> [Move g]
  -- it would be nice if we could define initState and runGame here, but we can't :(
  
-- Normal form game types.

-- A general normal form game.
data General mv = General Int (ByPlayer [mv]) [Payoff] deriving (Eq, Show)

-- A two-player, zero-sum game.
data Matrix mv = Matrix [mv] [mv] [Float] deriving (Eq, Show)

------------------------
-- Smart Constructors --
------------------------

-- Smart constructor to build from bare lists.
normal :: Eq mv => Int -> [[mv]] -> [[Float]] -> General mv
normal np mss vs = General np (ByPlayer mss) (map ByPlayer vs)

-- Construct a two-player game.
bimatrix :: Eq mv => [[mv]] -> [[Float]] -> General mv
bimatrix = normal 2

-- Construct a two-player, symmetric game.
symmetric :: Eq mv => [mv] -> [Float] -> General mv
symmetric ms vs = bimatrix [ms, ms] vs'
  where sym = concat (transpose (chunk (length ms) vs))
        vs' = zipWith (\a b -> [a,b]) vs sym

-- Construct a two-player, zero-sum game.
matrix :: [mv] -> [mv] -> [Float] -> Matrix mv
matrix = Matrix

square :: [mv] -> [Float] -> Matrix mv
square ms = Matrix ms ms

---------------------------
-- Equilibrium Solutions --
---------------------------

-- Finds all pure nash equilibrium solutions..
nash :: (Normal g, Eq (Move g)) => g -> [Profile (Move g)]
nash g = [s | s <- profiles g, stable s]
  where stable s = all (uni s) [1 .. numPlayers g]
        uni s p = and [g `pays` s `forPlayer` p >= 
                       g `pays` s' `forPlayer` p | s' <- change s p]
        change (ByPlayer s) p = let (h,_:t) = splitAt (p-1) s 
                                in [ByPlayer (h ++ e:t) | e <- moves g p]

-- Finds all strong Pareto optimal solutions.
pareto :: (Normal g, Eq (Move g)) => g -> [Profile (Move g)]
pareto g = [s | s <- profiles g, opt s]
  where opt s = not (any (imp s) (profiles g))
        imp s s' = let p  = toList (g `pays` s)
                       p' = toList (g `pays` s')
                   in or (zipWith (>) p' p) && and (zipWith (>=) p' p)

-- Finds all pareto optimal, pure equilibriums.
paretoNash :: (Normal g, Eq (Move g)) => g -> [Profile (Move g)]
paretoNash g = pareto g `intersect` nash g

-- Finds all saddle points of a matrix game.
saddle :: Eq mv => Matrix mv -> [Profile mv]
saddle g = [p | p <- profiles g, v p == minimum (r p), v p == maximum (c p)]
  where v p = pays g p `forPlayer` 1
        r (ByPlayer [m,_]) = row g (fromJust (elemIndex m (moves g 1)) + 1)
        c (ByPlayer [_,m]) = col g (fromJust (elemIndex m (moves g 2)) + 1)

-- Utility functions used in definitions.

-- Get a particular row of the payoff matrix.
row :: Matrix mv -> Int -> [Float]
row (Matrix _ ms ps) i = chunk (length ms) ps !! (i-1)

-- Get a particular col of the payoff matrix.
col :: Matrix mv -> Int -> [Float]
col (Matrix _ ms ps) i = transpose (chunk (length ms) ps) !! (i-1)

-- The dimensions of the payoff matrix.
dimensions :: Normal g => g -> [Int]
dimensions g = let np = numPlayers g
               in [length (moves g i) | i <- [1..np]]

-- A list of all pure strategy profiles.
profiles :: Normal g => g -> [Profile (Move g)]
profiles g = let np = numPlayers g
             in dcross (fromList [(moves g i) | i <- [1..np]])

runNormal :: Normal g => ExecM g Payoff
runNormal = do g <- game
               ms <- allPlayers decide
               return (g `pays` ms)

lookupPay :: Eq mv => ByPlayer [mv] -> [Payoff] -> Profile mv -> Payoff
lookupPay mss ps ms = fromJust (lookup ms (zip (dcross mss) ps))

-- Construct a zero-sum payoff grid.
zerosum :: [Float] -> [Payoff]
zerosum vs = [fromList [v, -v] | v <- vs]

---------------------------
-- Instance Declarations --
---------------------------

instance Eq mv => Game (General mv) where
  type Move (General mv) = mv
  type State (General mv) = ()
  initState _ = ()
  runGame = runNormal

instance Eq mv => Normal (General mv) where
  numPlayers (General np _ _) = np
  pays (General _ mss ps) = lookupPay mss ps
  moves (General _ mss _) p = mss `forPlayer` p

instance Eq mv => Game (Matrix mv) where
  type Move (Matrix mv) = mv
  type State (Matrix mv) = ()
  initState _ = ()
  runGame = runNormal

instance Eq mv => Normal (Matrix mv) where
  numPlayers _ = 2
  pays (Matrix ms ns ps) = lookupPay (fromList [ms,ns]) (zerosum ps)
  moves (Matrix ms _ _) 1 = ms
  moves (Matrix _ ms _) 2 = ms

---------------------
-- Pretty Printing --
---------------------

-- TODO

-- Examples

{-
data CD = C | D deriving (Eq, Show)

pd   = symmetric [C,D] [2, 0, 3, 1]
stag = symmetric [C,D] [3, 0, 2, 1]
-}
m1 = matrix [1..4] [1..4] [4,3,2,5,-10,2,0,-1,7,5,2,3,0,8,-4,-5]
m2 = matrix [1..3] [1..2] [2,-3,0,2,-5,10]

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

