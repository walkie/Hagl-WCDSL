module Game.Execution.Strategy where

import Control.Monad.State
import Data.List
import Game.Definition
import Game.Execution
import System.Random hiding (random)

-----------------------
-- Common Strategies --
-----------------------

-- Construct a pure strategy. Always play the same move.
pure :: m -> Strategy d m v
pure m = return m

-- Pick a move from a list randomly.
randomFrom :: [m] -> Strategy d m v
randomFrom dist = do i <- liftIO $ getStdRandom $ randomR (0, length dist - 1)
                     return $ dist !! i

-- Pick a move from the list of available moves randomly.
random :: Strategy d m v
random = do moves <- availMoves
            randomFrom moves

-- Construct a mixed strategy. Play moves based on a distribution.
mixed :: [(Int, m)] -> Strategy d m v
mixed dist = randomFrom $ concatMap (\(i, m) -> take i (repeat m)) dist

--------------------------
-- History Manipulation --
--------------------------

isFirstGame :: Game d m v Bool
isFirstGame = do ByGame h <- history 
                 return $ null h

-- All moves made by each player in each game.
moves :: (Show m, Num v) => Game d m v (ByGame (ByPlayer [m]))
moves = do ByGame h <- summaries 
           return $ ByGame $ fst $ unzip h

-- The last move by each player in each game.
move :: (Num v, Show m) => Game d m v (ByGame (ByPlayer m))
move = do mss <- moves
          return $ ByGame $ map (ByPlayer . (map head)) $ asList2 mss

-- The total payoff for each player for each game.
payoff :: (Show m, Num v) => Game d m v (ByGame (ByPlayer v))
payoff = do ByGame h <- summaries
            return $ ByGame $ snd $ unzip h

-- The current score of each player.
score :: (Show m, Num v) => Game d m v (ByPlayer v)
score = do ByGame ps <- payoff
           return $ ByPlayer $ map sum $ transpose $ map asList ps

-------------------------
-- Selection Functions --
-------------------------

-- Apply selection to each element of a list.
each :: (Game d m v a -> Game d m v b) -> Game d m v [a] -> Game d m v [b]
each f xs = do ys <- xs
               sequence $ map f (map return ys)

-- ByPlayer Selection --

-- The index of the current player.
myIndex :: Game d m v Int
myIndex = do (Turn p _ _) <- location
             return (p-1)

my :: Game d m v (ByPlayer a) -> Game d m v a
my x = do ByPlayer as <- x
          i <- myIndex
          return $ as !! i

his :: Game d m v (ByPlayer a) -> Game d m v a
his x = do ByPlayer as <- x
           i <- myIndex
           n <- numPlayers
           return $ as !! ((i+1) `mod` n)

her :: Game d m v (ByPlayer a) -> Game d m v a
her = his

our :: Game d m v (ByPlayer a) -> Game d m v [a]
our = liftM asList

their :: Game d m v (ByPlayer a) -> Game d m v [a]
their x = do ByPlayer as <- x
             i <- myIndex
             return $ (take i as) ++ (drop (i+1) as)

player :: Int -> Game d m v (ByPlayer a) -> Game d m v a
player i x = do ByPlayer as <- x
                return $ as !! i

-- ByGame Selection --

every :: Game d m v (ByGame a) -> Game d m v [a]
every = liftM asList

first :: Game d m v (ByGame a) -> Game d m v a
first = liftM (last . asList)

firstn :: Int -> Game d m v (ByGame a) -> Game d m v [a]
firstn n = liftM (reverse . take n . reverse . asList)

prev :: Game d m v (ByGame a) -> Game d m v a
prev = liftM (head . asList)

prevn :: Int -> Game d m v (ByGame a) -> Game d m v [a]
prevn n = liftM (take n . asList)

game :: Int -> Game d m v (ByGame a) -> Game d m v a
game i x = do ByGame as <- x
              n <- numGames
              return $ as !! (n-i)

-----------------------
-- Utility Functions --
-----------------------

asList2 :: (DList f, DList g) => f (g a) -> [[a]]
asList2 ll = map asList $ asList ll
