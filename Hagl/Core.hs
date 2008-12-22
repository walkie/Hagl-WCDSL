{-# OPTIONS_GHC -fglasgow-exts #-}

-- This module contains stuff that almost every Hagl file will need to import.
module Hagl.Core where

import Control.Monad.State hiding (State)
import Data.Maybe
import Data.List

---------------------
-- Game Definition --
---------------------

type PlayerIx = Int
type Payoff = ByPlayer Float

class Game g where
  type Move g
  type State g
  initState :: g -> State g
  runGame   :: ExecM g Payoff

---------------------
-- Execution State --
---------------------

-- Game Execution State
data Exec g = Exec {
  _game       :: g,              -- game definition
  _players    :: [Player g],     -- players active in game
  _gameState  :: State g,        -- the current state of the game
  _playerIx   :: Maybe PlayerIx, -- the index of the currently deciding player
  _transcript :: Transcript g,   -- moves so far this iteration (newest at head)
  _history    :: History g       -- a summary of each iteration
}

-- History
type Moved g   = (Maybe PlayerIx, Move g)
type Transcript g = [Moved g]

type History g = ByGame (Transcript g, Summary g)
type Summary g = (ByPlayer [Move g], Payoff)

initExec :: Game g => g -> [Player g] -> Exec g
initExec g ps = Exec g ps (initState g) Nothing [] (ByGame [])

-------------
-- Players --
-------------

type Name = String

data Player g = forall s.
  Player {
    name     :: Name,
    state    :: s,
    strategy :: Strategy g s
  }

plays :: Name -> Strategy g () -> Player g
plays n s = Player n () s

instance Show (Player g) where
  show = name
instance Eq (Player g) where
  a == b = name a == name b
instance Ord (Player g) where
  compare a b = compare (name a) (name b)

-----------------------------------
-- Execution and Strategy Monads --
-----------------------------------

data ExecM g a = ExecM { unE :: StateT (Exec g) IO a }
data StratM g s a = StratM { unS :: StateT s (ExecM g) a }
type Strategy g s   = StratM g s (Move g)

class (Monad m, MonadIO m) => GameM m g | m -> g where
  getExec :: m (Exec g)

update :: MonadState s m => (s -> s) -> m s
update f = modify f >> get

evalGame :: Game g => g -> [Player g] -> ExecM g a -> IO a
evalGame g ps f = evalStateT (unE f) (initExec g ps)

execGame :: Game g => g -> [Player g] -> ExecM g a -> IO (Exec g)
execGame g ps f = execStateT (unE f) (initExec g ps)

runStrategy :: Player g -> ExecM g (Move g, Player g)
runStrategy (Player n s f) = do (m, s') <- runStateT (unS f) s
                                return (m, Player n s' f)

-- ExecM instances
instance Monad (ExecM g) where
  return = ExecM . return
  (ExecM x) >>= f = ExecM (x >>= unE . f)

instance MonadState (Exec g) (ExecM g) where
  get = ExecM get
  put = ExecM . put

instance MonadIO (ExecM g) where
  liftIO = ExecM . liftIO

instance GameM (ExecM g) g where
  getExec = ExecM get

-- StratM instances
instance Monad (StratM g s) where
  return = StratM . return
  (StratM x) >>= f = StratM (x >>= unS . f)

instance MonadState s (StratM g s) where
  get = StratM get
  put = StratM . put

instance MonadIO (StratM g s) where
  liftIO = StratM . liftIO

instance GameM (StratM g s) g where
  getExec = StratM (lift getExec)

-----------------------
-- Dimensioned Lists --
-----------------------

data ByGame a = ByGame [a] deriving (Eq, Show)
data ByPlayer a = ByPlayer [a] deriving (Eq, Show)

forGame :: ByGame a -> Int -> a
forGame (ByGame as) i = as !! (length as - i)

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

dzipWith :: DList d => (a -> b -> c) -> d a -> d b -> d c
dzipWith f as bs = fromList (zipWith f (toList as) (toList bs))

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

debug :: MonadIO m => String -> m ()
debug = liftIO . putStrLn
