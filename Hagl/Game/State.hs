{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Game.State where

import Control.Monad
import Data.List

import Hagl.Core
import Hagl.Game
import Hagl.Exec

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
  runGame = takeTurns turn (liftM end gameState)
    where turn p = do m <- decide p
                      b <- updateGameState (mark p m)
                      return (pay b)

xo 1 = X
xo 2 = O

mark :: PlayerIx -> Int -> Board -> Board
mark p m b = take m b ++ xo p : drop (m+1) b

pay :: Board -> Payoff
pay b | win 1 b   = winner 2 1
      | win 2 b   = winner 2 2
      | otherwise = tie 2

win :: PlayerIx -> Board -> Bool
win p b = any (all (xo p ==)) rows
  where h = chunk 3 b
        v = transpose h
        d = map (map (b !!)) [[0,4,8],[2,4,6]]
        rows = h ++ v ++ d

empty :: Board -> [Int]
empty = elemIndices E

end :: Board -> Bool
end b = null (empty b) || win 1 b || win 2 b
