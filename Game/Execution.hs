module Game.Execution where

import Control.Monad.State
import Game.Definition

-----------
-- Types --
-----------

-- Monad
type Game d m v a = StateT (ExecState d m v) IO a

-- Game Execution State
data ExecState d m v = ExecState {
    _gameDef   :: (GameDef d m v),     -- game definition
    _players   :: [Player d m v],      -- players active in game
    _location  :: (GameTree d m v),    -- current node in game tree
    _events    :: [Event m v],         -- events thus far (newest at head)
    _history   :: (EventHistory m v),  -- a complete history of events for previous iterations
    _summaries :: (EventSummaries m v) -- a summary of each game
}

-- History
data ByGame a = ByGame [a] deriving (Eq, Show)
data ByPlayer a = ByPlayer [a] deriving (Eq, Show)

class DList f where
    asList :: f a -> [a]
instance DList ByGame where
    asList (ByGame as) = as
instance DList ByPlayer where
    asList (ByPlayer as) = as

data Event m v = MoveEvent Int m | PayoffEvent [v] deriving (Eq, Show)
type EventHistory m v = ByGame [Event m v]

type EventSummary m v = (ByPlayer [m], ByPlayer v)
type EventSummaries m v = ByGame (EventSummary m v)

-- Player/Strategy
type Name = String
type Strategy d m v = Game d m v m

data Player d m v = Player {
    playerName :: Name,
    strategy   :: (Strategy d m v)
}
instance Show (Player d m v) where
    show = playerName
instance Eq (Player d m v) where
    (Player n1 _) == (Player n2 _) = n1 == n2

-------------------------------
-- Execution State Shortcuts --
-------------------------------

gameDef :: Game d m v (GameDef d m v)
gameDef = liftM _gameDef get

players :: Game d m v [Player d m v]
players = liftM _players get

location :: Game d m v (GameTree d m v)
location = liftM _location get

events :: Game d m v [Event m v]
events = liftM _events get

history :: Game d m v (EventHistory m v)
history = liftM _history get

summaries :: Game d m v (EventSummaries m v)
summaries = liftM _summaries get

numGames :: Game d m v Int
numGames = liftM (length . asList) history

locationData :: Game d m v d
locationData = liftM (\(Turn _ d _) -> d) location

availMoves :: Game d m v [m]
availMoves = liftM2 ($) getAvailMoves locationData 

infoGroup :: Game d m v (InfoGroup d m v)
infoGroup = liftM2 ($) getInfoGroup locationData

-------------------------------
-- Game Definition Shortcuts --
-------------------------------

numPlayers :: Game d m v Int
numPlayers = liftM _numPlayers gameDef

gameTree :: Game d m v (GameTree d m v)
gameTree = liftM _gameTree gameDef

getInfoGroup :: Game d m v (d -> InfoGroup d m v)
getInfoGroup = liftM _getInfoGroup gameDef

getAvailMoves :: Game d m v (d -> [m])
getAvailMoves = liftM _getAvailMoves gameDef

---------------
-- Utilities --
---------------

scoreString :: (Show m, Num v) => [Player d m v] -> [v] -> String 
scoreString ps vs = unlines ["  "++show p++": "++show v | (p,v) <- zip ps vs]
