{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Types where -- maybe rename to Hagl.Core?

import Control.Monad.State

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

data ExecM    g a   = ExecM  { unG :: StateT (Exec g) IO a }
data StratM   g s a = StratM { unS :: StateT s (ExecM g) a }
type Strategy g s   = StratM g s (GameMove g)

class Monad m => GameMonad m g | m -> g where
  getExec :: m (Exec g)

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
