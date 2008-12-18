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

-- The index of the current player.
--myIx :: (Game g, GameM m g) => m PlayerIx
--myIx = do (Node _ (Decision p _)) <- _exactLoc
          --return (p-1)

my :: (Game g, GameM m g) => m (ByPlayer a) -> m a
my x = liftM2 (!!) (liftM toList x) myIx

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

-- This syntax should be changed to match forPlayer (name change would be nice too)
playern :: (Game g, GameM m g) => PlayerIx -> m (ByPlayer a) -> m a
playern i = liftM (`forPlayer` i)

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

gamen :: (Game g, GameM m g) => Int -> m (ByGame a) -> m a
gamen i = liftM (`forGame` i)

-----------------------
-- Utility Functions --
-----------------------

--_exactLoc :: (Game g, GameM m g) => m (GameTree g)
--_exactLoc = liftM _current getExec
