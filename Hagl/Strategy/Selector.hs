module Hagl.Strategy.Selector where

import Control.Monad
import Data.List

import Hagl.Core
import Hagl.Exec
import Hagl.Game

--------------------
-- List Selectors --
--------------------

-- Apply selection to each element of a list.
each :: Monad m => (m a -> m b) -> m [a] -> m [b]
each f xs = (sequence . map f . map return) =<< xs

-- ByPlayer Selection --

-- TODO replace with forPlayerM
my :: (Game g, GameM m g) => m (ByPlayer a) -> m a
my x = liftM2 (forPlayer) x myIx

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
every = liftM toList

first :: (Game g, GameM m g) => m (ByGame a) -> m a
first = liftM (last . toList)

firstn :: (Game g, GameM m g) => Int -> m (ByGame a) -> m [a]
firstn n = liftM (reverse . take n . reverse . toList)

prev :: (Game g, GameM m g) => m (ByGame a) -> m a
prev = liftM (head . toList)

prevn :: (Game g, GameM m g) => Int -> m (ByGame a) -> m [a]
prevn n = liftM (take n . toList)
