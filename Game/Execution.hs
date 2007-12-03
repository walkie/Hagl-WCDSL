module Game.Execution where

import Control.Monad.State
import Data.Maybe
import Game.Definition

-----------
-- Types --
-----------

-- Game Execution Monad
type GameExec m v a = StateT (ExecState m v) IO a

-- Game Execution State
data ExecState m v = ExecState {
    _root     :: (Game m v),   -- game definition
    _players  :: [Player m v], -- players active in game
    _location :: (Game m v),   -- current node in game tree
    _moves    :: [(Int, m)],   -- moves so far this game (newest at head)
    _history  :: (History m v) -- a summary of each game
}

-- History
type History m v = ByGame (Summary m v)
type Summary m v = (ByPlayer [m], ByPlayer v)

data ByGame a = ByGame [a] deriving (Eq, Show)
data ByPlayer a = ByPlayer [a] deriving (Eq, Show)

class DList f where
    asList :: f a -> [a]
instance DList ByGame where
    asList (ByGame as) = as
instance DList ByPlayer where
    asList (ByPlayer as) = as

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
    (Player n1 _) == (Player n2 _) = n1 == n2

--------------------
-- Game Execution --
--------------------

runGame :: Game m v -> [Player m v] -> GameExec m v a -> IO (ExecState m v)
runGame root ps f = execStateT f $ initState root ps

step :: (Eq m) => GameExec m v ()
step = get >>= \state ->
    let t = _location state in case t of
      Decision (Turn t _ next) ->
        do m <- strategy $ _players state !! (t-1)
           put state { _location = fromJust $ lookup m next,
                       _moves = (t, m) : _moves state }
      --Chance next -> 
      Payoff vs ->
        let summary = summarize (_moves state) vs in
          put state { _location = _root state,
                      _moves = [],
                      _history = ByGame (summary : asList (_history state)) }

once :: (Eq m) => GameExec m v ()
once = do loc <- location
          case loc of
            Payoff _ -> step
            _ -> step >> once 
                       
times :: (Eq m) => Int -> GameExec m v ()
times 0 = return ()
times n = once >> times (n-1)

-------------------------------
-- Execution State Shortcuts --
-------------------------------

root :: GameExec m v (Game m v)
root = liftM _root get

players :: GameExec m v [Player m v]
players = liftM _players get

location :: GameExec m v (Game m v)
location = liftM _location get

moves :: GameExec m v [(Int, m)]
moves = liftM _moves get

history :: GameExec m v (History m v)
history = liftM _history get

numGames :: GameExec m v Int
numGames = liftM (length . asList) history

---------------
-- Utilities --
---------------

initState :: Game m v -> [Player m v] -> ExecState m v
initState root ps = ExecState root ps root [] (ByGame [])

summarize :: [(Int, m)] -> [v] -> Summary m v
summarize ms vs =
    let np = length vs
        for a = filter (\(b, _) -> a == b) ms
    in (ByPlayer (map (\i -> map snd $ for i) [1..np]), ByPlayer vs)

scoreString :: (Show m, Num v) => [Player m v] -> [v] -> String 
scoreString ps vs = unlines ["  "++show p++": "++show v | (p,v) <- zip ps vs]

