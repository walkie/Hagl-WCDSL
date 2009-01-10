{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-

From GHCi:

> execGame rps [rocky, randy] (times 10 >> printTranscripts >> printScore)
> roundRobin rps [rocky, rotate, randy, pavlov, frequency] (times 100)

-}

module Examples.RPS where

import Hagl hiding (randomly)
import Hagl.Normal

-------------------------
-- Rock Paper Scissors --
-------------------------

data RPS = Rock | Paper | Scissors deriving (Enum, Eq, Show)

rps = square [Rock .. Scissors] [0,-1, 1,
                                 1, 0,-1,
                                -1, 1, 0]

-- Some simple players
rocky = "Stalone" `plays` pure Rock
rotate = "RPS" `plays` periodic [Rock .. Scissors]
randy = "Randy" `plays` randomly

-- If last move resulted in a "big" payoff, do it again, otherwise switch.
pavlov = "Pavlov" `plays`
    (randomly `atFirstThen`
     do p <- my (prev payoff)
        m <- my (prev move)
        if p > 0 then return m else randomly)

-- Play the move that will beat the move the opponent has played most.
frequency = "Huckleberry" `plays`
    do ms <- his `each` every move
       let r = length $ filter (Rock ==) ms
           p = length $ filter (Paper ==) ms
           s = length $ filter (Scissors ==) ms
           x = maximum [r,p,s]
        in return $ if x == r then Paper else 
                    if x == p then Scissors
                              else Rock
