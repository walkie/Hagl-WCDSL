{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Game where

import Control.Monad.State

import Hagl.Lists
import Hagl.Types
import Hagl.Strategy.Accessor

setPlayerIx :: Game g => Maybe PlayerIx -> ExecM g ()
setPlayerIx i = do exec <- getExec
                   put exec { _playerIx = i }

playersTurn :: Game g => PlayerIx -> ExecM g ()
playersTurn i = setPlayerIx (Just i)

endTurn :: Game g => ExecM g ()
endTurn = setPlayerIx Nothing

getPlayer :: Game g => PlayerIx -> ExecM g (Player g)
getPlayer i = liftM (!! i) players

setPlayer :: Game g => PlayerIx -> Player g -> ExecM g ()
setPlayer i p = do e <- getExec
                   let (ph, _:pt) = splitAt (i-1) (_players e)
                    in put e { _players = ph ++ p : pt }

genericMoved :: Game g => (Maybe PlayerIx) -> Move g -> ExecM g ()
genericMoved i m = do e <- getExec
                      put e { _transcript = (i, m) : _transcript e }

marginal :: Game g => (Float -> Float) -> ExecM g Payoff
marginal f = liftM (fromList . map f . toList) score

playerMoved :: Game g => PlayerIx -> Move g -> ExecM g ()
playerMoved = genericMoved . Just

moved :: Game g => Move g -> ExecM g ()
moved = genericMoved Nothing

decide :: Game g => PlayerIx -> ExecM g (Move g)
decide i = do playersTurn i
              p <- getPlayer i
              (m, p') <- runStrategy p
              setPlayer i p
              playerMoved i m
              endTurn
              return m

allPlayers :: Game g => (PlayerIx -> ExecM g a) -> ExecM g (ByPlayer a)
allPlayers f = do n <- numPlayers
                  liftM ByPlayer (sequence (map f [1..n]))

{- Goal: Normal form games

runGame g = do ms <- allPlayers decide
               g `pays` ms

-}

{-
------------------------
-- Information Groups --
------------------------

data Info g = Perfect (GameTree g)
            | Imperfect [GameTree g]
            | Simultaneous

--
-- Smart constructors for deriving information groups for classes of games.
--

perfect :: g -> GameTree g -> Info g
perfect _ = Perfect

simultaneous :: g -> GameTree g -> Info g
simultaneous _ _ = Simultaneous

----------------
-- Game Trees --
----------------

type Edge g = (Move g, GameTree g)

data GameTree g = Node (State g) (NodeType g)
data NodeType g = Decision PlayerIx [Edge g] -- decision made by a player
                | Chance (Dist (Edge g))     -- random move from distribution
                | Payoff Payoff              -- terminating payoff

--
-- Smart constructors for defining stateless game trees.
--

decide :: State g ~ () => PlayerIx -> [Edge g] -> GameTree g
decide p = Node () . Decision p

chance :: State g ~ () => Dist (Edge g) -> GameTree g
chance = Node () . Chance

pay :: State g ~ () => Payoff -> GameTree g
pay = Node () . Payoff

--
-- Smart constructors for defining payoffs.
--

-- Payoff where player w wins (1) and all other players, out of np, lose (-1).
winner :: Int -> PlayerIx -> Payoff
winner np w = ByPlayer $ replicate (w-1) (-1) ++ (fromIntegral np - 1) : replicate (np - w) (-1)

-- Payoff where player w loses (-1) and all other players, out of np, win (1).
loser :: Int -> PlayerIx -> Payoff
loser np l = ByPlayer $ replicate (l-1) 1 ++ (1 - fromIntegral np) : replicate (np - l) 1

tie :: Int -> Payoff
tie np = ByPlayer $ replicate np 0

--
-- Functions for traversing game trees.
--

-- The moves available from a node.
movesFrom :: GameTree g -> [Move g]
movesFrom (Node _ (Decision _ es)) = [m | (m,_) <- es]
movesFrom (Node _ (Chance d)) = [m | (_,(m,_)) <- d]
movesFrom _ = []

-- The immediate children of a node.
children :: GameTree g -> [GameTree g]
children (Node _ (Decision _ es)) = [n | (_,n) <- es]
children (Node _ (Chance d)) = [n | (_,(_,n)) <- d]
children _ = []

-- Nodes in BFS order.
bfs :: GameTree g -> [GameTree g]
bfs t = let b [] = []
            b ns = ns ++ b (concatMap children ns)
        in b [t]

-- Nodes DFS order.
dfs :: GameTree g -> [GameTree g]
dfs t = t : concatMap dfs (children t)

-- The highest numbered player in this finite game tree.
maxPlayer :: GameTree g -> Int
maxPlayer t = foldl1 max $ map player (dfs t)
  where player (Node _ (Decision p _)) = p
        player _ = 0

---------------
-- Instances --
---------------

-- Eq

instance (Eq (Move g), Eq (State g)) => Eq (Info g) where
  (Perfect t1) == (Perfect t2) = t1 == t2
  (Imperfect t1) == (Imperfect t2) = t1 == t2
  Simultaneous == Simultaneous = True
  _ == _ = False

instance (Eq (Move g), Eq (State g)) => Eq (GameTree g) where
  (Node s1 t1) == (Node s2 t2) = s1 == s2 && t1 == t2

instance (Eq (Move g), Eq (State g)) => Eq (NodeType g) where
  (Decision p1 es1) == (Decision p2 es2) = p1 == p2 && es1 == es2
  (Chance d1) == (Chance d2) = d1 == d2
  (Payoff v1) == (Payoff v2) = v1 == v2
  _ == _ = False

-- Show

instance Show (Move g) => Show (Info g) where
  show (Perfect t) = show t
  show (Imperfect ts) = unlines $ intersperse "*** OR ***" (map show ts)
  show Simultaneous = "Cannot show this location in the game tree."

instance Show (Move g) => Show (GameTree g) where
  show g = condense $ Tree.drawTree $ t "" g
    where t pre (Node _ nt) =
            let s (Decision p es) = pre ++ "Player " ++ show p
                s (Chance d) = pre ++ "Chance"
                s (Payoff (ByPlayer vs)) = pre ++ show vs
                c (Decision p es) = [t (show m ++ " -> ") g | (m,g) <- es]
                c (Chance d) = [t (show i ++ " * " ++ show m ++ " -> ") g | (i,(m,g)) <- d]
                c (Payoff _) = []
            in Tree.Node (s nt) (c nt)
          condense s = let empty = not . and . map (\c -> c == ' ' || c == '|')
                       in unlines $ filter empty $ lines s
                       -}
