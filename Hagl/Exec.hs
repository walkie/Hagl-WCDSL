{-# OPTIONS_GHC -fglasgow-exts #-}

module Game.Execution where

import Control.Monad.State

-----------
-- Types --
-----------

-- Game Execution State
data Exec g mv = Exec {
    _initial  :: g,             -- initial game definition
    _current  :: g,             -- current state of game
    _players  :: [Player g mv], -- players active in game
    _events   :: [Event mv],    -- events so far this game (newest at head)
    _history  :: History mv     -- a summary of each game
}

-- History
type History mv = ByGame ([Event mv], Summary mv)
type Summary mv = (ByPlayer [mv], ByPlayer Float)

type PlayerIx = Int

data Event mv = DecisionEvent PlayerIx mv
              | ChanceEvent mv
              | PayoffEvent [Float]
              deriving (Eq, Show)

data ByGame a = ByGame [a] deriving (Eq, Show)
data ByPlayer a = ByPlayer [a] deriving (Eq, Show)

class DList f where
  asList :: f a -> [a]
instance DList ByGame where
  asList (ByGame as) = as
instance DList ByPlayer where
  asList (ByPlayer as) = as

asList2 :: (DList f, DList g) => f (g a) -> [[a]]
asList2 = map asList . asList

-- Player
type Name = String

data Player g mv = forall s.
  Player {
    name     :: Name,
    state    :: s,
    strategy :: Strategy g mv s
  }

plays :: Name -> Strategy g mv () -> Player g mv
plays n s = Player n () s

instance Show (Player g mv) where
  show = name
instance Eq (Player g mv) where
  a == b = name a == name b
instance Ord (Player g mv) where
  compare a b = compare (name a) (name b)

----------------------------------------
-- Game Execution and Strategy Monads --
----------------------------------------

data ExecM    g mv a   = ExecM  { unG :: StateT (Exec g mv) IO a }
data StratM   g mv s a = StratM { unS :: StateT s (ExecM g mv) a }
type Strategy g mv s   = StratM g mv s mv

-- ExecM instances
instance Monad (ExecM g mv) where
  return = ExecM . return
  (ExecM x) >>= f = ExecM (x >>= unG . f)

instance MonadState (Exec g mv) (ExecM g mv) where
  get = ExecM get
  put = ExecM . put

instance MonadIO (ExecM g mv) where
  liftIO = ExecM . liftIO

-- StratM instances
instance Monad (StratM g mv s) where
  return = StratM . return
  (StratM x) >>= f = StratM (x >>= unS . f)

instance MonadState s (StratM g mv s) where
  get = StratM get
  put = StratM . put

instance MonadIO (StratM g mv s) where
  liftIO = StratM . liftIO

update :: MonadState s m => (s -> s) -> m s
update f = modify f >> get

--------------------------
-- GameMonad Type Class --
--------------------------

class Monad m => GameMonad m g mv | m -> g, m -> mv where
  getExec :: m (Exec g mv)

instance GameMonad (ExecM g mv) g mv where
  getExec = ExecM get

instance GameMonad (StratM g mv s) g mv where
  getExec = StratM (lift getExec)
