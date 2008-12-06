{-# OPTIONS_GHC -fglasgow-exts #-}

module Game.Types where -- maybe rename to Hagl.Core?

import Control.Monad.State

---------------------
-- Game Definition --
---------------------

type PlayerIx = Int
type Payoff = ByPlayer Float

data Game g 

class Game g mv | g -> mv where
  numPlayers :: g -> Int
  runGame    :: ExecM g mv (Summary mv)

-----------------------
-- Player Definition --
-----------------------

type Name = String

data Player g mv = forall s.
  Player {
    name     :: Name,
    state    :: s,
    strategy :: Strategy g mv s
  }

instance Show (Player g mv) where
  show = name
instance Eq (Player g mv) where
  a == b = name a == name b
instance Ord (Player g mv) where
  compare a b = compare (name a) (name b)

--------------------
-- Game Execution --
--------------------

data Exec g mv = Exec {
    --_game    :: g,
    _game    :: Game g mv,
    _players :: [Player g mv],
    _history :: History mv
}

type History mv = ByGame (Summary mv)
type Summary mv = ([mv], ByPlayer [mv], Payoff)

data ExecM    g mv a   = ExecM  { unG :: StateT (Exec g mv) IO a }
data StratM   g mv s a = StratM { unS :: StateT s (ExecM g mv) a }
type Strategy g mv s   = StratM g mv s mv

class Monad m => GameMonad m g mv | m -> g, m -> mv where
  getExec :: m (Exec g mv)

-----------------------
-- Dimensioned Lists --
-----------------------

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

---------------------
-- Monad Instances --
---------------------

-- ExecM instances
instance Monad (ExecM g mv) where
  return = ExecM . return
  (ExecM x) >>= f = ExecM (x >>= unG . f)

instance MonadState (Exec g mv) (ExecM g mv) where
  get = ExecM get
  put = ExecM . put

instance MonadIO (ExecM g mv) where
  liftIO = ExecM . liftIO

instance GameMonad (ExecM g mv) g mv where
  getExec = ExecM get

-- StratM instances
instance Monad (StratM g mv s) where
  return = StratM . return
  (StratM x) >>= f = StratM (x >>= unS . f)

instance MonadState s (StratM g mv s) where
  get = StratM get
  put = StratM . put

instance MonadIO (StratM g mv s) where
  liftIO = StratM . liftIO

instance GameMonad (StratM g mv s) g mv where
  getExec = StratM (lift getExec)
