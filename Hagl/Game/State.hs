{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Game.State where

import Control.Monad.State hiding (State)
import Data.List

import Hagl.Strategy.Accessor hiding (numPlayers, moves)
import Hagl.Lists
import Hagl.Types
import Hagl.Game

-----------------
-- Tic Tac Toe --
-----------------

data Square = X | O | E deriving (Eq, Show)
type Board = [Square]

data TicTacToe = TicTacToe

instance Game TicTacToe where
  type Move TicTacToe = Int
  type State TicTacToe = Board
  initState _ = replicate 9 E
  runGame = turn 1

turn :: PlayerIx -> ExecM TicTacToe Payoff
turn p = do m <- decide p
            b <- mark p m
            if end b then return (pay p b)
                     else turn (nextPlayer 2 p)

pay :: PlayerIx -> Board -> Payoff
pay p b = if win p b then winner 2 p else tie 2

xo 1 = X
xo 2 = O

mark :: PlayerIx -> Int -> ExecM TicTacToe Board
mark p m = do b <- gameState
              putGameState (take m b ++ xo p : drop (m+1) b)

win :: PlayerIx -> Board -> Bool
win p b = any (all (xo p ==)) (rows b)

rows :: Board -> [[Square]]
rows b = let h = chunk 3 b
             v = transpose h
             d = map (map (b !!)) [[0,4,8],[2,4,6]]
         in h ++ v ++ d

empty :: Board -> [Int]
empty = elemIndices E

end :: Board -> Bool
end b = null (empty b) || win 1 b || win 2 b

{-
mark 1 = X
mark 2 = O

empty :: Board -> [Int]
empty = elemIndices Empty

end :: Board -> PlayerIx -> Bool
end b p = win b p || null (empty b)

avail :: Board -> PlayerIx -> [Move]
avail b _ = empty b

exec :: Board -> PlayerIx -> Move -> Board
exec b p m = take m b ++ mark p : drop (m+1) b

pay :: Board -> PlayerIx -> [Float]
pay b p | win b p = winner 2 p
        | otherwise = tie 2

win :: Board -> PlayerIx -> Bool
win b p = let h = chunk 3 b
              v = transpose h
              d = map (map (b !!)) [[0,4,8],[2,4,6]]
          in or $ map (and . map (mark p ==)) (h ++ v ++ d)

ticTacToe = takeTurns 2 end avail exec pay (replicate 9 Empty)

-- A Minimax Player
mm = "Minimaxi" `plays` minimax

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
-}
