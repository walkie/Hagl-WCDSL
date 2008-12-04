{-# OPTIONS_GHC -fglasgow-exts #-}

module Game.Definition.Normal where

import Data.List
import Data.Maybe

import Game.Util

--type Payoff = ByPlayer Float
type Payoff = ByPlayer Float
type Profile mv = ByPlayer mv -- pure strategy profile
type PlayerIx = Int

--data Normal mv = Normal Int (ByPlayer [mv]) ([mv] -> Payoff)
--data Normal mv = Normal Int (PlayerIx -> [mv]) ([mv] -> Payoff)
--data Normal mv = Normal Int [[mv]] ([mv] -> Payoff)
--data Normal mv = Normal Int (PlayerIx -> [mv]) ([mv] -> Payoff)
data Normal mv = Normal Int (ByPlayer [mv]) [Payoff]

numPlayers :: Normal mv -> Int
numPlayers (Normal np _ _) = np

payoff :: Eq mv => Normal mv -> Profile mv -> Payoff
payoff (Normal _ mss ps) ms = fromJust (lookup ms (zip (cross mss) ps))

moves :: Normal mv -> PlayerIx -> [mv]
moves (Normal _ mss _) p = mss `forPlayer` p

normal :: Eq mv => Int -> [[mv]] -> [[Float]] -> Normal mv
normal np mss vs = Normal np (ByPlayer mss) (map ByPlayer vs)

-- A list of all pure strategy profiles.
profiles :: Normal mv -> [Profile mv]
profiles (Normal _ mss _) = cross mss

{-
moves :: Normal mv -> PlayerIx -> [mv]
moves (Normal _ mss _) = (mss !!)

numPlayers :: Normal mv -> Int
numPlayers (Normal np _ _) = np

normal :: Eq mv => Int -> [[mv]] -> [[Float]] -> Normal mv
normal np mss vs = Normal np mss payoff
  where payoff ms = fromJust (lookup ms (zip (allCombs mss) vs))
-}

-- Construct a two-player Normal-Form game, where each player has the same moves.
matrix :: Eq mv => [mv] -> [[Float]] -> Normal mv
matrix ms = normal 2 [ms,ms]

-- Construct a two-player Zero-Sum game, where each player has the same moves.
zerosum :: Eq mv => [mv] -> [Float] -> Normal mv
zerosum ms vs = matrix ms [[v, -v] | v <- vs]

{-
-- Finds all pure nash equilibrium solutions..
nash :: Eq mv => Normal mv -> [[mv]]
nash (Normal np mss pay) = [s | s <- allCombs mss, stable s]
  where stable s = all (uni s) [0..(np-1)]
        uni s p = and [pay s !! p >= pay s' !! p | s' <- change s p]
        change s p = let (h,_:t) = splitAt p s 
                     in [h ++ e:t | e <- mss !! p]

-- Finds all strong Pareto optimal solutions.
pareto :: Eq mv => Normal mv -> [[mv]]
pareto (Normal np mss pay) = [s | s <- ss, opt s]
  where ss = allCombs mss
        opt ms = not (any (imp ms) ss)
        imp ms ms' = let (p, p') = (pay ms, pay ms')
                     in or (zipWith (>) p' p) &&
                        and (zipWith (>=) p' p)

-- Finds all payoff dominant solutions.
dominant :: Eq mv => Normal mv -> [[mv]]
dominant g = nash g `intersect` pareto g
-}

-- Finds all pure nash equilibrium solutions..
nash :: Eq mv => Normal mv -> [Profile mv]
nash g = [s | s <- profiles g, stable s]
  where stable s = all (uni s) [1 .. numPlayers g]
        uni s p = and [payoff g s `forPlayer` p >= 
                       payoff g s' `forPlayer` p | s' <- change s p]
        change (ByPlayer s) p = let (h,_:t) = splitAt (p-1) s 
                                in [ByPlayer (h ++ e:t) | e <- moves g p]

-- Finds all strong Pareto optimal solutions.
pareto :: Eq mv => Normal mv -> [Profile mv]
pareto g = [s | s <- profiles g, opt s]
  where opt s = not (any (imp s) (profiles g))
        imp s s' = let p  = toList (payoff g s)
                       p' = toList (payoff g s')
                   in or (zipWith (>) p' p) && and (zipWith (>=) p' p)

-- Finds all payoff dominant solutions.
dominant :: Eq mv => Normal mv -> [Profile mv]
dominant g = nash g `intersect` pareto g

-- Delete me when merged with other code...
data ByGame a = ByGame [a] deriving (Eq, Show)
data ByPlayer a = ByPlayer [a] deriving (Eq, Show)

forGame :: ByGame a -> Int -> a
forGame (ByGame as) p = as !! (p-1)

forPlayer :: ByPlayer a -> PlayerIx -> a
forPlayer (ByPlayer as) p = as !! (p-1)

class DList d where
  fromList :: [a] -> d a
  toList :: d a -> [a]
instance DList ByGame where
  fromList = ByGame
  toList (ByGame as) = as
instance DList ByPlayer where
  fromList = ByPlayer
  toList (ByPlayer as) = as

cross :: DList d => d [a] -> [d a]
cross xss = map fromList (c (toList xss))
  where c (xs:xss) = [y:ys | y <- xs, ys <- c xss]
        c [] = [[]]

toList2 :: (DList f, DList g) => f (g a) -> [[a]]
toList2 = map toList . toList

-- Examples

data CD = C | D deriving (Eq, Show)

pd   = matrix [C,D] [[2,2],[0,3],[3,0],[1,1]]
stag = matrix [C,D] [[3,3],[0,2],[2,0],[1,1]]
