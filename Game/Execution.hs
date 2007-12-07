module Game.Execution where

import Control.Monad.State
import Game.Definition

-----------
-- Types --
-----------

-- Game Execution Monad
type GameExec m a = StateT (ExecState m) IO a

-- Game Execution State
data ExecState m = ExecState {
    _game       :: (Game m),       -- game definition
    _players    :: [Player m],     -- players active in game
    _location   :: (GameTree m),   -- current node in game tree
    _transcript :: (Transcript m), -- events so far this game (newest at head)
    _history    :: (History m)     -- a summary of each game
}

-- History
type History m = ByGame (Transcript m, Summary m)
type Transcript m = [Event m]
type Summary m = (ByPlayer [m], ByPlayer Float)

data Event m = DecisionEvent Int m
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

-- Player/Strategy
type Name = String
type Strategy m = GameExec m m

data Player m = Player {
    playerName :: Name,
    strategy   :: (Strategy m)
}
instance Show (Player m) where
    show = playerName
instance Eq (Player m) where
    a == b = playerName a == playerName b
instance Ord (Player m) where
    compare a b = compare (playerName a) (playerName b)

-------------------------------
-- Execution State Shortcuts --
-------------------------------

game :: GameExec m (Game m)
game = liftM _game get

players :: GameExec m [Player m]
players = liftM _players get

location :: GameExec m (GameTree m)
location = liftM _location get

transcript :: GameExec m (Transcript m)
transcript = liftM _transcript get

history :: GameExec m (History m)
history = liftM _history get

numGames :: GameExec m Int
numGames = liftM (length . asList) history
