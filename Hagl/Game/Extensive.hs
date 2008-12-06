
-------------------------
-- Game Tree Traversal --
-------------------------

type GameTree mv s = Tree (Action mv s)

tree :: Game mv d s -> GameTree mv s
tree g@(Game _ _ _ s) = t s
  where t s = Node (next g s) (map t (children g s))

-- The moves that are available from this node.
availMoves :: Game mv d s -> s -> [mv]
availMoves g s = case next g s of
    (Decision _ f) -> [m | (m,_)   <- f]
    (Chance f)     -> [m | (_,m,_) <- f]
    _ -> []

-- The immediate children of a node.
children :: Game mv d s -> s -> [s]
children g s = case next g s of
    (Decision _ f) -> [s' | (_,s')   <- f]
    (Chance f)     -> [s' | (_,_,s') <- f]
    _ -> []

-- Nodes in BFS order.
bfs :: Game mv d s -> s -> [s]
bfs g s = let b [] = []
              b ss = ss ++ b (concatMap (children g) ss)
          in b [s]

-- Nodes DFS order.
dfs :: Game mv d s -> s -> [s]
dfs g s = s : concatMap (dfs g) (children g s)

-- Game tree as a Data.Tree structure.
stateTree :: Game mv d s -> s -> Tree s
stateTree g s = Node s $ map (stateTree g) (children g s)


instance (Show mv) => Show (Game mv d s) where
  show g = condense $ drawTree $ f "" (next g (initialState g))
    where f p (Decision i ts) = Node (p ++ "Player " ++ show i) [f (show m ++ " -> ") (next g s) | (m, s) <- ts]
          f p (Chance ts)     = Node (p ++ "Chance") [f (show m ++ "(" ++ show c ++ ") -> ") (next g s) | (c, m, s) <- ts]
          f p (Payoff vs)     = Node (p ++ show vs) []
          condense s = let empty = not . and . map (\c -> c == ' ' || c == '|')
                       in unlines $ filter empty $ lines s
