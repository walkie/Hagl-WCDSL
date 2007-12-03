module Game.Definition where

import Data.Tree

---------------------
-- Game Definition --
---------------------

-- Game Tree
data GameTree d m v = Turn Int d (m -> GameTree d m v)
                    | Payoff [v] (GameTree d m v)
                    | End

-- Game Definition
type InfoGroup d m v = [GameTree d m v]
data GameDef d m v = GameDef {
    _numPlayers    :: Int,
    _gameTree      :: (GameTree d m v),
    _getInfoGroup  :: (d -> InfoGroup d m v),
    _getAvailMoves :: (d -> [m])
}

-- Instance Declarations
instance (Show v) => Show (GameTree d m v) where
  show (Turn t _ n) = "Player " ++ show t
  show (Payoff vs _) = show vs
  show End = "(End)"
instance (Show m, Show v) => Show (GameDef d m v) where
  show (GameDef _ tree _ moves) = showTree moves tree

showTree :: (Show m, Show v) => (d -> [m]) -> GameTree d m v -> String
showTree moves tree = 
    let node s (Turn t d next) = Node (s ++ "Player " ++ show t) [node (show m ++ " -> ") (next m) | m <- moves d]
        node s (Payoff v End) = Node (s ++ show v) []
        node s (Payoff v t) = Node (s ++ show v) [node " -> " t]
        node s End = Node (s ++ "(End)") []
    in drawTree $ node "" tree

------------------------
-- GameTree Traversal --
------------------------

-- The immediate children of a node.
children :: GameDef d m v -> GameTree d m v -> [GameTree d m v]
children _ End = []
children _ (Payoff _ t) = [t] 
children g (Turn _ d next) = map next (_getAvailMoves g d)

-- Search nodes in BFS order.
bfs :: GameDef d m v -> GameTree d m v -> [GameTree d m v]
bfs g t = let b [] = []
              b ts = ts ++ b (concatMap (children g) ts)
          in b [t]

-- Search nodes DFS order.
dfs :: GameDef d m v -> GameTree d m v -> [GameTree d m v]
dfs g t = t : concatMap (dfs g) (children g t)
