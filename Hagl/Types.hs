{-# OPTIONS_GHC -fglasgow-exts #-}

module Game.Types where -- maybe rename to Hagl.Core?

import Control.Monad.State

---------------------
-- Game Definition --
---------------------

type PlayerIx = Int
type Payoff = ByPlayer Float

type Edge g = (GameMove g, GameTree g)
type Dist a = [(Int, a)] -- Maybe incorporate probability package?

data GameTree g = Node (GameState g) (NodeType g)
data NodeType g = DN PlayerIx [Edge g]
                | CN (Dist (Edge g))
                | PN Payoff

type family GameMove g
type family GameState g

class Game g where
  numPlayers :: g -> Int
  gameTree   :: g -> GameTree g

-----------------------
-- Player Definition --
-----------------------

type Name = String

data Player g = forall s.
  Player {
    name     :: Name,
    state    :: s,
    strategy :: Strategy g s
  }

instance Show (Player g) where
  show = name
instance Eq (Player g) where
  a == b = name a == name b
instance Ord (Player g) where
  compare a b = compare (name a) (name b)

--------------------
-- Game Execution --
--------------------

data Exec g = Exec {
    _game      :: g,
    _players   :: [Player g],
    _gameState :: GameState g
    --_history :: History mv
}

-- These should be moved into Iterated...
type History mv = ByGame (Summary mv)
type Summary mv = ([mv], ByPlayer [mv], Payoff)

data ExecM    g a   = ExecM  { unG :: StateT (Exec g) IO a }
data StratM   g s a = StratM { unS :: StateT s (ExecM g) a }
type Strategy g s   = StratM g s (GameMove g)

class Monad m => GameMonad m g | m -> g where
  getExec :: m (Exec g)

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
instance Monad (ExecM g) where
  return = ExecM . return
  (ExecM x) >>= f = ExecM (x >>= unG . f)

instance MonadState (Exec g) (ExecM g) where
  get = ExecM get
  put = ExecM . put

instance MonadIO (ExecM g) where
  liftIO = ExecM . liftIO

instance GameMonad (ExecM g) g where
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

instance GameMonad (StratM g s) g where
  getExec = StratM (lift getExec)
