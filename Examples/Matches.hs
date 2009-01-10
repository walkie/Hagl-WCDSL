{-# OPTIONS_GHC -fglasgow-exts #-}

{-

An implementation of the Match Game, described in the paper.

Example experiments from GHCi:
> execGame matchGame [matchy, randy] (once >> printTranscript)
> execGame matchGame [matchy, randy] (times 1000 >> printScore)
> execGame matchGame [randy, matchy] (times 1000 >> printScore)

-}
module Examples.Matches where

import Data.Maybe
import Data.List

import Hagl hiding (moves, payoff, randomly)

data Matches = Matches Int [Int]

instance Game Matches where
  type Move Matches = Int
  type State Matches = Int
  initState (Matches n _) = n
  runGame = takeTurns turn end >>= payoff

matches :: GameM m Matches => m Int
matches = gameState

draw n = updateGameState (subtract n)

turn p = decide p >>= draw >> return p
          
end = do n <- matches
         return (n <= 0)

payoff p = do n <- numPlayers
              return (loser n p)

moves = do n <- matches
           (Matches _ ms) <- game
           return [m | m <- ms, n-m >= 0]

randomly = do ms <- moves
              randomlyFrom ms

matchGame = Matches 15 [1,2,3] -- an example match game

-- Players

randy = "Randy" `plays` randomly

matchy = "Matchy" `plays`
    do n  <- matches
       ms <- moves
       let winning m = mod (n-1) (maximum ms + 1) == m
        in maybe randomly play (find winning ms)
