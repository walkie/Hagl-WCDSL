module Hagl.Selector where

import Control.Monad
import Data.List

import Hagl.Core
import Hagl.Accessor
import Hagl.Game

--------------------
-- List Selectors --
--------------------

-- Apply selection to each element of a list.
each :: Monad m => (m a -> m b) -> m [a] -> m [b]
each f xs = (sequence . map f . map return) =<< xs

-- ByPlayer Selection --

my :: (Game g, GameM m g) => m (ByPlayer a) -> m a
my x = myIx >>= forPlayerM x

-- Selects the next player's x.
his :: (Game g, GameM m g) => m (ByPlayer a) -> m a
his x = do as <- x
           i <- myIx
           np <- numPlayers
           return $ as `forPlayer` nextPlayer np i

her :: (Game g, GameM m g) => m (ByPlayer a) -> m a
her = his

our :: (Game g, GameM m g) => m (ByPlayer a) -> m [a]
our = liftM toList

their :: (Game g, GameM m g) => m (ByPlayer a) -> m [a]
their x = do ByPlayer as <- x
             i <- myIx
             return $ (take i as) ++ (drop (i+1) as)

-- ByGame Selection --

every :: (Game g, GameM m g) => m (ByGame a) -> m [a]
every = liftM (tail . toList) -- TODO use of tail here is a gross hack to avoid a bug in "move"

first :: (Game g, GameM m g) => m (ByGame a) -> m a
first x = x `forGameM` 1

firstn :: (Game g, GameM m g) => Int -> m (ByGame a) -> m [a]
firstn n x = sequence (map (forGameM x) [n, n-1 .. 1])

prev :: (Game g, GameM m g) => m (ByGame a) -> m a
prev x = do n <- numGames
            x `forGameM` n

this :: (Game g, GameM m g) => m (ByGame a) -> m a
this x = do n <- gameNumber
            x `forGameM` n

prevn :: (Game g, GameM m g) => Int -> m (ByGame a) -> m [a]
prevn i x = do n <- numGames
               sequence (map (forGameM x) [n, n-1 .. i])
