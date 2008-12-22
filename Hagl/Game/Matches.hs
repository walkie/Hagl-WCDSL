{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Game.Matches where

import Control.Monad
import Data.Maybe
import Data.List hiding (take)
import Prelude hiding (take)

import Hagl.Core
import Hagl.Game hiding (winner)
import Hagl.Exec hiding (moves, payoff)
import Hagl.Exec.Run
import Hagl.Exec.Print
import Hagl.Strategy hiding (randomly)

----------------
-- Match Game --
----------------

data Matches = Matches Int [Int]

instance Game Matches where
  type Move Matches = Int
  type State Matches = Int
  initState (Matches n _) = n
  runGame = takeTurns turn end >>= payoff

matches :: GameM m Matches => m Int
matches = gameState

take n = updateGameState (subtract n)

turn p = decide p >>= take >> return p
          
end = do n <- matches
         return (n <= 0)

payoff p = do n <- numPlayers
              return (loser n p)

moves = do n <- matches
           (Matches _ ms) <- game
           return [m | m <- ms, n-m >= 0]

randomly = do ms <- moves
              randomlyFrom ms

randy = "Randy" `plays` randomly

matchy = "Matchy" `plays`
    do n  <- matches
       ms <- moves
       let winning m = mod (n-1) (maximum ms + 1) == m
        in maybe randomly play (find winning ms)
