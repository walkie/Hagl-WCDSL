module Hagl.Lists where

import Data.List

-----------------------
-- Dimensioned Lists --
-----------------------

data ByGame a = ByGame [a] deriving (Eq, Show)
data ByPlayer a = ByPlayer [a] deriving (Eq, Show)

forGame :: ByGame a -> Int -> a
forGame (ByGame as) p = as !! (p-1)

forPlayer :: ByPlayer a -> Int -> a
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

dcross :: DList d => d [a] -> [d a]
dcross xss = map fromList (cross (toList xss))

toList2 :: (DList f, DList g) => f (g a) -> [[a]]
toList2 = map toList . toList

-------------------
-- Distributions --
-------------------
-- Maybe replace with Martin's probability package?

type Dist a = [(Int, a)]

expandDist :: Dist a -> [a]
expandDist d = concat [replicate i a | (i, a) <- d]

----------------------------
-- List Utility Functions --
----------------------------

cross :: [[a]] -> [[a]]
cross (xs:xss) = [y:ys | y <- xs, ys <- cross xss]
cross [] = [[]]

ucross :: (Ord a) => [[a]] -> [[a]]
ucross = nub . map sort . cross

-- Break a list into n equal-sized chunks.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = (take n l) : chunk n (drop n l)
