{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Game.Auction where

import Hagl.Strategy.Accessor
import Hagl.Lists
import Hagl.Types
import Hagl.Game

type Dollars = Float

data Bid = Bid Dollars | Pass deriving (Eq, Show)

data Auction = Auction {
    opening :: Dollars,
    prize   :: Dollars
}

instance Game Auction where
  type Move Auction = Bid
  type State Auction = Dollars -- the highest bid
  initState = opening
  runGame = undefined

round :: Int -> ExecM Auction Payoff
round last = do n <- numPlayers

-- take 5 (drop 3 (cycle [1..5]))
--end :: [Bid] -> 

