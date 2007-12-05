module Game.Execution where

import Control.Monad.State
import Game.Definition

-----------
-- Types --
-----------

-- Game Execution Monad
type GameExec m v a = StateT (ExecState m v) IO a

-- Game Execution State
data ExecState m v = ExecState {
    _game       :: (Game m v),       -- game definition
    _players    :: [Player m v],     -- players active in game
    _location   :: (GameTree m v),   -- current node in game tree
    _transcript :: (Transcript m v), -- events so far this game (newest at head)
    _history    :: (History m v)     -- a summary of each game
}

-- History
type History m v = ByGame (Transcript m v, Summary m v)
type Transcript m v = [Event m v]
type Summary m v = (ByPlayer [m], ByPlayer v)

data Event m v = DecisionEvent Int m
               | ChanceEvent Int
               | PayoffEvent [v]
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
type Strategy m v = GameExec m v m

data Player m v = Player {
    playerName :: Name,
    strategy   :: (Strategy m v)
}
instance Show (Player m v) where
    show = playerName
instance Eq (Player m v) where
    a == b = playerName a == playerName b
instance Ord (Player m v) where
    compare a b = compare (playerName a) (playerName b)

-------------------------------
-- Execution State Shortcuts --
-------------------------------

game :: GameExec m v (Game m v)
game = liftM _game get

players :: GameExec m v [Player m v]
players = liftM _players get

location :: GameExec m v (GameTree m v)
location = liftM _location get

transcript :: GameExec m v (Transcript m v)
transcript = liftM _transcript get

history :: GameExec m v (History m v)
history = liftM _history get

numGames :: GameExec m v Int
numGames = liftM (length . asList) history
