module Game.Util where

import Data.List

expandDist :: [(Int, a)] -> [a]
expandDist d = concat [replicate i a | (i, a) <- d]

branch :: [(Int, a)] -> Int -> Int
branch ((i, _):r) n | n < i = 1
                    | otherwise = 1 + branch r (n-i)

-- An list indexing function that returns the element at n, or the last
-- element if n is greater than the length of the list.  Works with 
-- infinite lists.
(!!!) :: [a] -> Int -> a
l !!! n = if length (take n l) == n then l !! n else last l

-- Break a list into n equal-sized chunks.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = (take n l) : chunk n (drop n l)
