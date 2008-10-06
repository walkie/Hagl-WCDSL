{-# OPTIONS_GHC -fglasgow-exts #-}

module Game.Execution where

import Control.Monad.State
import Game.Definition

-----------
-- Types --
-----------

-- Game Execution State
data ExecState mv = ExecState {
    _game       :: Game mv,       -- game definition
    _players    :: [Player mv],   -- players active in game
    _location   :: GameTree mv,   -- current node in game tree
    _transcript :: Transcript mv, -- events so far this game (newest at head)
    _history    :: History mv     -- a summary of each game
}

-- History
type History mv = ByGame (Transcript mv, Summary mv)
type Transcript mv = [Event mv]
type Summary mv = (ByPlayer [mv], ByPlayer Float)

data Event mv = DecisionEvent PlayerIx mv
              | ChanceEvent Int
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

data Player mv = forall s.
  Player {
    name     :: Name,
    state    :: s,
    strategy :: Strategy mv s
  }

plays :: Name -> Strategy mv () -> Player mv
plays n s = Player n () s

instance Show (Player mv) where
  show = name
instance Eq (Player mv) where
  a == b = name a == name b
instance Ord (Player mv) where
  compare a b = compare (name a) (name b)

----------------------------------------
-- Game Execution and Strategy Monads --
----------------------------------------

data ExecM mv m a = Monad m => ExecM { unG :: StateT (ExecState mv) m a }
data StratM mv s m a = Monad m => StratM { unS :: StateT s m a }

type GameExec mv = ExecM mv IO
type StratExec mv s = StratM mv s (GameExec mv)
type Strategy mv s = StratExec mv s mv

-- ExecM instances
instance Monad m => Monad (ExecM mv m) where
  return = ExecM . return
  (ExecM x) >>= f = ExecM (x >>= unG . f)

instance Monad m => MonadState (ExecState mv) (ExecM mv m) where
  get = ExecM get
  put = ExecM . put

instance MonadTrans (ExecM mv) where
  lift = ExecM . lift
    
instance MonadIO m => MonadIO (ExecM mv m) where
  liftIO = lift . liftIO

-- StratM instances
instance Monad m => Monad (StratM mv s m) where
  return = StratM . return
  (StratM x) >>= f = StratM (x >>= unS . f)

instance Monad m => MonadState s (StratM mv s m) where
  get = StratM get
  put = StratM . put

instance MonadTrans (StratM mv s) where
  lift = StratM . lift
    
instance MonadIO m => MonadIO (StratM mv s m) where
  liftIO = lift . liftIO

update :: MonadState s m => (s -> s) -> m s
update f = modify f >> get

-------------------------------
-- Execution State Shortcuts --
-------------------------------

class Monad m => GameMonad m mv | m -> mv where
  game       :: m (Game mv)
  players    :: m [Player mv]
  location   :: m (InfoGroup mv)
  _exactLoc  :: m (GameTree mv)
  transcript :: m (Transcript mv)
  history    :: m (History mv)
  numGames   :: m Int

instance Monad m => GameMonad (ExecM mv m) mv where
  game       = liftM _game get
  players    = liftM _players get
  location   = get >>= \s -> return $ info (_game s) (_location s)
  _exactLoc  = liftM _location get
  transcript = liftM _transcript get
  history    = liftM _history get
  numGames   = liftM (length . asList) history

instance (GameMonad m mv, Monad m) => GameMonad (StratM mv s m) mv where
  game       = lift game
  players    = lift players
  location   = lift location
  _exactLoc  = lift _exactLoc
  transcript = lift transcript
  history    = lift history
  numGames   = lift numGames
