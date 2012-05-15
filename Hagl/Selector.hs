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
each f xs = mapM (f . return) =<< xs

-- ByPlayer Selection --

my :: (Game g, GameM m g) => m (ByPlayer a) -> m a
my x = myIx >>= forPlayerM x

-- Selects the next player's x.
his :: (Game g, GameM m g) => m (ByPlayer a) -> m a
his x = do as <- x
           i <- myIx
           np <- numPlayers
           if np /= 2 then error "his/her can only be used in two player games"
                      else return $ as `forPlayer` nextPlayer np i

her :: (Game g, GameM m g) => m (ByPlayer a) -> m a
her = his

our :: (Game g, GameM m g) => m (ByPlayer a) -> m [a]
our = liftM toList

their :: (Game g, GameM m g) => m (ByPlayer a) -> m [a]
their x = do ByPlayer as <- x
             i <- myIx
             return $ take i as ++ drop (i+1) as

-- ByGame Selection --

every :: (Game g, GameM m g) => m (ByGame a) -> m [a]
every = liftM (tail . toList) -- TODO use of tail here is a gross hack to avoid a bug in "move"

first :: (Game g, GameM m g) => m (ByGame a) -> m a
first x = x `forGameM` 1

firstn :: (Game g, GameM m g) => Int -> m (ByGame a) -> m [a]
firstn n x = mapM (forGameM x) [n, n-1 .. 1]

prev :: (Game g, GameM m g) => m (ByGame a) -> m a
prev x = numGames >>= forGameM x

this :: (Game g, GameM m g) => m (ByGame a) -> m a
this x = gameNumber >>= forGameM x

prevn :: (Game g, GameM m g) => Int -> m (ByGame a) -> m [a]
prevn i x = do n <- numGames
               mapM (forGameM x) [n, n-1 .. i]
