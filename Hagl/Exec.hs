{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Exec where

import Control.Monad.State

import Hagl.Game
import Hagl.Lists

---------------------
-- Execution State --
---------------------

-- Game Execution State
data Exec g = Exec {
  _game       :: g,            -- game definition
  _current    :: GameTree g,   -- the current location in the game tree
  _players    :: [Player g],   -- players active in game
  _transcript :: Transcript g, -- events so far this iteration (newest at head)
  _history    :: History g     -- a summary of each iteration
}

-- History
type Transcript g = [Event g]
type History g = ByGame ([Event g], Summary g)
type Summary g = (ByPlayer [Move g], ByPlayer Float)

data Event g = DecisionEvent PlayerIx (Move g)
             | ChanceEvent (Move g)
             | PayoffEvent Payoff

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

data ExecM    g a   = ExecM  { unE :: StateT (Exec g) IO a }
data StratM   g s a = StratM { unS :: StateT s (ExecM g) a }
type Strategy g s   = StratM g s (Move g)

class Monad m => GameM m g | m -> g where
  getExec :: m (Exec g)

update :: MonadState s m => (s -> s) -> m s
update f = modify f >> get

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
