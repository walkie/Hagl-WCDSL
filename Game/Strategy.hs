module Game.Strategy where

import Control.Monad.State
import Data.List
import Game.Definition
import Game.Execution
import Game.Execution.Util

-----------------------
-- Common Strategies --
-----------------------

-- Construct a pure strategy. Always play the same move.
pure :: m -> Strategy m v
pure m = return m

-- Pick a move from the list of available moves randomly.
random :: Strategy m v
random = randomFrom =<< liftM availMoves location

-- Construct a mixed strategy. Play moves based on a distribution.
mixed :: [(Int, m)] -> Strategy m v
mixed = randomFrom . expandDist

-- Perform some pattern of moves periodically.
periodic :: [m] -> Strategy m v
periodic ms = numGames >>= \n -> return $ ms !! mod n (length ms)

-- Perform some strategy on the first move, then another strategy thereafter.
firstThen :: Strategy m v -> Strategy m v -> Strategy m v
firstThen first rest = isFirstGame >>= \b -> if b then first else rest

--------------------------
-- History Manipulation --
--------------------------

-- True if this is the first iteration in this execution instance.
isFirstGame :: GameExec m v Bool
isFirstGame = liftM (null . asList) history

-- Transcript of each game.
transcripts :: GameExec m v (ByGame (Transcript m v))
transcripts = liftM (ByGame . fst . unzip . asList) history

-- Summary of each game.
summaries :: GameExec m v (ByGame (Summary m v))
summaries = liftM (ByGame . snd . unzip . asList) history

-- All moves made by each player in each game.
moves :: GameExec m v (ByGame (ByPlayer [m]))
moves = liftM (ByGame . fst . unzip . asList) summaries

-- The last move by each player in each game.
move :: GameExec m v (ByGame (ByPlayer m))
move = liftM (ByGame . map (ByPlayer . map head) . asList2) moves

-- The total payoff for each player for each game.
payoff :: GameExec m v (ByGame (ByPlayer v))
payoff = liftM (ByGame . snd . unzip . asList) summaries

-- The current score of each player.
score :: (Num v) => GameExec m v (ByPlayer v)
score = liftM (ByPlayer . map sum . transpose . asList2) payoff

-------------------------
-- Selection Functions --
-------------------------

-- Apply selection to each element of a list.
each :: (GameExec m v a -> GameExec m v b) -> GameExec m v [a] -> GameExec m v [b]
each f xs = (sequence . map f . map return) =<< xs

-- ByPlayer Selection --

-- The index of the current player.
myIndex :: GameExec m v Int
myIndex = do Decision p _ <- location
             return (p-1)

my :: GameExec m v (ByPlayer a) -> GameExec m v a
my x = liftM2 (!!) (liftM asList x) myIndex

-- Selects the next player's x.
his :: GameExec m v (ByPlayer a) -> GameExec m v a
his x = do ByPlayer as <- x
           i <- myIndex
           g <- game
           return $ as !! ((i+1) `mod` numPlayers g)

her :: GameExec m v (ByPlayer a) -> GameExec m v a
her = his

our :: GameExec m v (ByPlayer a) -> GameExec m v [a]
our = liftM asList

their :: GameExec m v (ByPlayer a) -> GameExec m v [a]
their x = do ByPlayer as <- x
             i <- myIndex
             return $ (take i as) ++ (drop (i+1) as)

playern :: Int -> GameExec m v (ByPlayer a) -> GameExec m v a
playern i x = do ByPlayer as <- x
                 return $ as !! i

-- ByGame Selection --

every :: GameExec m v (ByGame a) -> GameExec m v [a]
every = liftM asList

first :: GameExec m v (ByGame a) -> GameExec m v a
first = liftM (last . asList)

firstn :: Int -> GameExec m v (ByGame a) -> GameExec m v [a]
firstn n = liftM (reverse . take n . reverse . asList)

prev :: GameExec m v (ByGame a) -> GameExec m v a
prev = liftM (head . asList)

prevn :: Int -> GameExec m v (ByGame a) -> GameExec m v [a]
prevn n = liftM (take n . asList)

gamen :: Int -> GameExec m v (ByGame a) -> GameExec m v a
gamen i x = do ByGame as <- x
               n <- numGames
               return $ as !! (n-i)
