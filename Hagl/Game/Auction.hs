{-# OPTIONS_GHC -fglasgow-exts #-}

module Hagl.Game.Auction where

import Hagl.Core
import Hagl.Exec
import Hagl.Game

import Hagl.Exec.Run
import Hagl.Exec.Print
import Hagl.Strategy

type Cents = Int

data Bid = Bid Cents | Pass deriving (Eq, Show)

-- Dollar Auction

data Auction = Auction
type HighBid = (PlayerIx, Cents)

data HighBids = HighBids {
  highBidder :: PlayerIx, highBid :: Cents,
  nextBidder :: PlayerIx, nextBid :: Cents
}

instance Game Auction where
  type Move Auction = Bid
  type State Auction = HighBids
  initState _ = HighBids 0 0 0 0
  runGame = numPlayers >>= bid 1

bid p n = do
    h <- gameState
    if highBidder h == p then return (pay n h) else do
    b <- decide p
    update h p b
    bid (nextPlayer n p) n
  where update h p (Bid d) 
          | d < highBid h = error "Bid is too low!"
          | otherwise = putGameState (HighBids p d (highBidder h) (highBid h))
        update h p Pass = return ()

pay :: Int -> HighBids -> Payoff
pay n h = ByPlayer [fromIntegral (val p) | p <- [1..n]]
  where val p | p == highBidder h = 100 - highBid h
              | p == nextBidder h =   0 - nextBid h
              | otherwise         =   0

p1 :: Player Auction
p1 = plays "Penny" $ do 
       h <- gameState
       mixed [(4, Bid (highBid h + 1)), (1, Pass)]

p2 :: Player Auction
p2 = plays "Nickel" $ do
       h <- gameState
       return $ if highBid h < 50 then Bid (highBid h + 5) else Pass

p3 :: Player Auction
p3 = plays "Quarter" $ do
       h <- gameState
       mixed [(1, Bid (highBid h + 25)), (3, Pass)]
{-
data Auction = Auction {
    opening :: Dollars,
    prize   :: Dollars
}
-}
