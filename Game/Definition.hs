{-# OPTIONS_GHC -fglasgow-exts #-}

module Game.Definition where

import Game.Types

runStrategy :: Player mv -> ExecM mv (mv, Player mv)
runStrategy (Player n s m) = do (mv, s') <- runStateT (unS m) s
                                return (mv, Player n s' m)
decide :: Player mv -> ExecM mv
decide 


-- The highest number player from this *finite* game tree.
{- Works, but why is this needed?
maxPlayer :: Game mv d s -> s -> PlayerIx
maxPlayer g s = foldl1 max $ map (player g) (dfs g s)
  where player (Game _ next _) s = case next s of
            (Decision p _) -> p
            _ -> 0
-}

{-
data (GameDef d, GameState s mv) => Game d s mv = Game {
    definition :: d,
    gameState  :: s
}

class GameDef d where
    numPlayers :: d -> Int

class GameState s mv | s -> mv where
    nextAction :: ExecM (Action s mv)
    -- or: nextAction :: d -> s -> Action s mv

data Action s mv = Decision PlayerIx [(mv, s)]
                 | Chance [(Int, mv, s)]
                 | Payoff [Float]



type PlayerIx = Int

data Action s mv = Decision PlayerIx [(mv, s)]
                 | Chance [(Int, mv, s)]
                 | Payoff [Float]

class Game g s mv | g -> s, g -> mv where
    numPlayers :: g -> Int
    nextAction :: g -> s -> Action s mv

-}

{-
type PlayerIx = Int

data Action g mv = Decision PlayerIx [(mv, g)]
                 | Chance [(Int, mv, g)]
                 | Payoff [Float]

class Game g mv | g -> mv where
    numPlayers :: g -> Int
    nextAction :: g -> Action g mv

-------------------------
-- Game Tree Traversal --
-------------------------

-- The moves that are available from this node.
availMoves :: Game g mv => g -> [mv]
availMoves g = case nextAction g of
    (Decision _ f) -> [m | (m,_)   <- f]
    (Chance d)     -> [m | (_,m,_) <- d]
    _ -> []

-- The immediate children of a node.
children :: Game g mv => g -> [g]
children g = case nextAction g of
    (Decision _ f) -> [g' | (_,g')   <- f]
    (Chance d)     -> [g' | (_,_,g') <- d]
    _ -> []

-- Nodes in BFS order.
bfs :: Game g mv => g -> [g]
bfs g = let b [] = []
            b gs = gs ++ b (concatMap children gs)
        in b [g]

-- Nodes DFS order.
dfs :: Game g mv => g -> [g]
dfs g = g : concatMap dfs (children g)

-- Game tree as a Data.Tree structure.
asTree :: Game g mv => g -> Tree g
asTree g = Node g $ map asTree (children g)

-- The highest number player from this *finite* game tree.
maxPlayer :: Game g mv => g -> PlayerIx
maxPlayer g = foldl1 max $ map player (dfs g)
  where player g = case nextAction g of
            (Decision p _) -> p
            _ -> 0

-}

-- Game Definition
{-
data Game mv = Game {
    numPlayers :: Int,
    info       :: GameTree mv -> InfoGroup mv,
    tree       :: GameTree mv
}

-- Game Tree
data GameTree mv = Decision PlayerIx [(mv, GameTree mv)]
                 | Chance [(Int, GameTree mv)]
                 | Payoff [Float]
                 deriving Eq

data InfoGroup mv = Perfect (GameTree mv)
                  | Imperfect [GameTree mv]
                  deriving Eq


-- Instance Declarations
instance (Show mv) => Show (GameTree mv) where
  show t = condense $ drawTree $ s "" t
    where s p (Decision i ts) = Node (p ++ "Player " ++ show i) [s (show m ++ " -> ") t | (m, t) <- ts]
          s p (Chance ts) = Node (p ++ "Chance") [s (show c ++ " -> ") t | (c, t) <- ts]
          s p (Payoff vs) = Node (p ++ show vs) []
          condense s = let empty = not . and . map (\c -> c == ' ' || c == '|')
                       in unlines $ filter empty $ lines s
instance (Show mv) => Show (Game mv) where
  show g = show (tree g)
instance (Show mv) => Show (InfoGroup mv) where
  show (Perfect t) = show t
  show (Imperfect ts) = unlines $ intersperse " ** or **" (map (init . show) ts)

----------------------------
-- Normal Form Definition --
----------------------------

-- Construct a game from a Normal-Form definition
normal :: Int -> [[mv]] -> [[Float]] -> Game mv
normal np mss vs = Game np group (head (level 1))
  where level n | n > np = [Payoff v | v <- vs]
                | otherwise = let ms = mss !! (n-1) 
                                  bs = chunk (length ms) (level (n+1)) 
                              in map (Decision n . zip ms) bs
        group (Decision n _) = Imperfect (level n)
        group t = Perfect t

-- Construct a two-player Normal-Form game, where each player has the same moves.
matrix :: [mv] -> [[Float]] -> Game mv
matrix ms = normal 2 [ms,ms]

-- Construct a two-player Zero-Sum game, where each player has the same moves.
zerosum :: [mv] -> [Float] -> Game mv
zerosum ms vs = matrix ms [[v, -v] | v <- vs]

-------------------------------
-- Extensive Form Definition --
-------------------------------

-- Build a game from a tree. Assumes a finite game tree.
extensive :: GameTree mv -> Game mv
extensive t = Game (maxPlayer t) Perfect t

-----------------------------
-- State-Driven Definition --
-----------------------------

{- Build a state-based game.
 - Args:
     * Number of players.
     * Whose turn is it?
     * Is the game over?
     * What are the available moves?
     * Execute a move and return the new state.
     * What is the payoff for this (final) state?
     * Initial state. -}
stateGame :: Int -> (s -> PlayerIx) -> (s -> PlayerIx -> Bool) -> 
             (s -> PlayerIx -> [mv]) -> (s -> PlayerIx -> mv -> s) -> 
             (s -> PlayerIx -> [Float]) -> s -> Game mv
stateGame np who end moves exec pay init = Game np Perfect (tree init)
  where tree s | end s p = Payoff (pay s p)
               | otherwise = Decision p [(m, tree (exec s p m)) | m <- moves s p]
          where p = who s

{- Build a state-based game where the players take turns. Player 1 goes first.
 - Args:
     * Number of players.
     * Is the game over?
     * What are the available moves?
     * Execute a move and return the new state.
     * What is the payoff for this (final) state?
     * Initial state. -}
takeTurns :: Int -> (s -> PlayerIx -> Bool) -> (s -> PlayerIx -> [mv]) ->
             (s -> PlayerIx -> mv -> s) -> (s -> PlayerIx -> [Float]) -> s ->
             Game mv
takeTurns np end moves exec pay init =
    stateGame np snd (lft end) (lft moves) exec' (lft pay) (init, 1)
  where exec' (s,_) p m = (exec s p m, (mod p np) + 1)
        lft f (s,_) p = f s p

----------------------------
-- Game Tree Construction --
----------------------------

-- Construct a payoff where player w wins (1) and all other players,
-- out of np, lose (-1).
winner :: Int -> PlayerIx -> [Float]
winner np w = replicate (w-1) (-1) ++ (fromIntegral np - 1) : replicate (np - w) (-1)

-- Construct a payoff where player w loses (-1) and all other players,
-- out of np, win (1).
loser :: Int -> PlayerIx -> [Float]
loser np l = replicate (l-1) 1 ++ (1 - fromIntegral np) : replicate (np - l) 1

tie :: Int -> [Float]
tie np = replicate np 0

-- Construct a decision node with only one option.
player :: PlayerIx -> (mv, GameTree mv) -> GameTree mv
player i m = Decision i [m]

-- Combines two game trees.
(<+>) :: GameTree mv -> GameTree mv -> GameTree mv
Payoff as <+> Payoff bs = Payoff (zipWith (+) as bs)
Chance as <+> Chance bs = Chance (as ++ bs)
Decision a as <+> Decision b bs | a == b = Decision a (as ++ bs)

-- Add a decision branch to a game tree.
(<|>) :: GameTree mv -> (mv, GameTree mv) -> GameTree mv
Decision i ms <|> m = Decision i (m:ms)

-------------------------
-- Game Tree Traversal --
-------------------------


-- Return the moves that are available from this node.
availMoves :: GameTree mv -> [mv]
availMoves (Decision _ ms) = map fst ms
availMoves _ = []

-- The immediate children of a node.
children :: GameTree mv -> [GameTree mv]
children (Decision _ ms) = map snd ms
children (Chance cs) = map snd cs
children _ = []

-}
