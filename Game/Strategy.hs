module Game.Strategy where

import Control.Monad.State
import Data.List
import Data.Maybe
import Game.Definition
import Game.Execution
import Game.Execution.Util
import Game.Util

-----------------------
-- Common Strategies --
-----------------------

-- Construct a pure strategy. Always play the same move.
pure :: mv -> Strategy mv s
pure = return

-- Pick a move from the list of available moves randomly.
randomly :: Eq mv => Strategy mv s
randomly = do loc <- location
              let ms = case loc of
                         Perfect t -> availMoves t 
                         Imperfect ts -> foldl1 intersect (map availMoves ts)
               in randomlyFrom ms

-- Pick a move randomly from a list.
randomlyFrom :: [mv] -> Strategy mv s
randomlyFrom as = liftM (as !!) (randomIndex as)

-- Construct a mixed strategy. Play moves based on a distribution.
mixed :: [(Int, mv)] -> Strategy mv s
mixed = randomlyFrom . expandDist

-- Perform some pattern of moves periodically.
periodic :: [mv] -> Strategy mv s
periodic ms = numGames >>= \n -> return $ ms !! mod n (length ms)

-- Begin a list of strategies.
atFirst :: Strategy mv s -> [Strategy mv s] -> Strategy mv s
atFirst s ss = numGames >>= \n -> (s:ss) !!! n

-- Next in a list of strategies.
next :: Strategy mv s -> [Strategy mv s] -> [Strategy mv s]
next = (:)

-- End a list of strategies.
finally :: Strategy mv s -> [Strategy mv s]
finally = (:[])

-- Play a strategy in the first game, then another strategy thereafter.
atFirstThen :: Strategy mv s -> Strategy mv s -> Strategy mv s
atFirstThen a b = atFirst a (finally b)

-- Play a move in the first game, then another strategy thereafter.
initiallyThen :: mv -> Strategy mv s -> Strategy mv s
initiallyThen a b = atFirst (return a) (finally b)

-- Minimax algorithm with alpha-beta pruning. Only defined for games with
-- perfect information and no Chance nodes.
minimax :: Strategy mv s
minimax = myIndex >>= \me -> location >>= \loc ->
  let isMe = (me + 1 ==)
      val alpha beta n@(Decision p _)
         | alpha >= beta = if isMe p then alpha else beta
         | otherwise =
             let mm (a,b) n = let v = val a b n
                              in if isMe p then (max a v, b)
                                           else (a, min b v)
                 (alpha', beta') = foldl mm (alpha, beta) (children n)
             in if isMe p then alpha' else beta'
      val _ _ (Payoff vs) = vs !! me
  in case loc of
       Perfect n -> 
         let vals = map (val (-infinity) infinity) (children n)
         in return $ availMoves n !! maxIndex vals

infinity :: Float
infinity = 1/0

--------------------------
-- History Manipulation --
--------------------------

-- True if this is the first iteration in this execution instance.
isFirstGame :: GameMonad m mv => m Bool
isFirstGame = liftM (null . asList) history

-- Transcript of each game.
transcripts :: GameMonad m mv => m (ByGame (Transcript mv))
transcripts = liftM (ByGame . fst . unzip . asList) history

-- Summary of each game.
summaries :: GameMonad m mv => m (ByGame (Summary mv))
summaries = liftM (ByGame . snd . unzip . asList) history

-- All moves made by each player in each game.
moves :: GameMonad m mv => m (ByGame (ByPlayer [mv]))
moves = liftM (ByGame . fst . unzip . asList) summaries

-- The last move by each player in each game.
move :: GameMonad m mv => m (ByGame (ByPlayer mv))
move = liftM (ByGame . map (ByPlayer . map head) . asList2) moves

-- The total payoff for each player for each game.
payoff :: GameMonad m mv => m (ByGame (ByPlayer Float))
payoff = liftM (ByGame . snd . unzip . asList) summaries

-- The current score of each player.
score :: GameMonad m mv => m (ByPlayer Float)
score = liftM (ByPlayer . map sum . transpose . asList2) payoff

-------------------------
-- Selection Functions --
-------------------------

-- Apply selection to each element of a list.
each :: Monad m => (m a -> m b) -> m [a] -> m [b]
each f xs = (sequence . map f . map return) =<< xs

-- ByPlayer Selection --

-- The index of the current player.
myIndex :: GameMonad m mv => m Int
myIndex = do Decision p _ <- _exactLoc
             return (p-1)

my :: GameMonad m mv => m (ByPlayer a) -> m a
my x = liftM2 (!!) (liftM asList x) myIndex

-- Selects the next player's x.
his :: GameMonad m mv => m (ByPlayer a) -> m a
his x = do ByPlayer as <- x
           i <- myIndex
           g <- game
           return $ as !! ((i+1) `mod` numPlayers g)

her :: GameMonad m mv => m (ByPlayer a) -> m a
her = his

our :: GameMonad m mv => m (ByPlayer a) -> m [a]
our = liftM asList

their :: GameMonad m mv => m (ByPlayer a) -> m [a]
their x = do ByPlayer as <- x
             i <- myIndex
             return $ (take i as) ++ (drop (i+1) as)

playern :: GameMonad m mv => Int -> m (ByPlayer a) -> m a
playern i x = do ByPlayer as <- x
                 return $ as !! (i-1)

-- ByGame Selection --

every :: GameMonad m mv => m (ByGame a) -> m [a]
every = liftM asList

first :: GameMonad m mv => m (ByGame a) -> m a
first = liftM (last . asList)

firstn :: GameMonad m mv => Int -> m (ByGame a) -> m [a]
firstn n = liftM (reverse . take n . reverse . asList)

prev :: GameMonad m mv => m (ByGame a) -> m a
prev = liftM (head . asList)

prevn :: GameMonad m mv => Int -> m (ByGame a) -> m [a]
prevn n = liftM (take n . asList)

gamen :: GameMonad m mv => Int -> m (ByGame a) -> m a
gamen i x = do ByGame as <- x
               n <- numGames
               return $ as !! (n-i)

---------------
-- Utilities --
---------------

maxIndex :: (Ord a) => [a] -> Int
maxIndex as = fromJust $ elemIndex (maximum as) as
