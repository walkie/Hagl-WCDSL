{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-

Defines the prisoner's dilemma, stag hunt, and a suite of strategies.

From GHCi, try some of the following.

> nash pd
> pareto pd
> paretoNash pd
> paretoNash stag
> execGame pd [tft, pavlov] (times 10 >> printTranscripts >> printScore)
> axelrod [fink, tft, grim, pavlov, preserver]

-}

module Examples.Prisoner where

import Control.Monad.State

import Hagl hiding (randomly)
import Hagl.Normal

------------------------
-- Prisoner's Dilemma --
------------------------

-- Game definition
data Cooperation = C | D deriving (Show, Eq)

pd = symmetric [C, D] [2, 0, 3, 1]
stag = symmetric [C, D] [3, 0, 2, 1]

-- Some simple players.
fink = "Fink" `plays` pure D
mum = "Mum" `plays` pure C
alt = "Alternator" `plays` periodic [C, D]
dc = "(DC)*" `plays` periodic [C, D]
ccd = "(CCD)*" `plays` periodic [C, C, D]
randy = "Randy" `plays` randomlyFrom [C, D]
rr = "Russian Roulette" `plays` mixed [(5, C), (1, D)]

-- The famous Tit-for-Tat.
tft = "Tit for Tat" `plays` (C `initiallyThen` his (prev move))

-- An example of using state to define a strategy; alternates moves.
stately = Player "Stately Alternator" C $
  do m <- get
     put $ if m == C then D else C
     return m

mod3 = Player "Mod3 Cooperator" 0 $
  do i <- get
     put (i+1)
     return $ if i `mod` 3 == 0 then C else D
     
-- Suspicious Tit-for-Tat (like Tit-for-Tat but defect on first move)
suspicious = "Suspicious Tit-for-Tat" `plays` (D `initiallyThen` his (prev move))

-- Tit-for-Tat that only defects after two defects in a row.
titForTwoTats = "Tit-for-Two-Tats" `plays`
    do ms <- his `each` prevn 2 move
       return $ if ms == [D, D] then D else C

-- The Grim Trigger: Cs until opponent defects, then defects forever.
grim = "Grim Trigger" `plays`
    do ms <- his `each` every move
       return $ if D `elem` ms then D else C

grim' = Player "Stately Grim" False $ 
  C `initiallyThen`
  do m <- his (prev move)
     triggered <- update (|| m == D)
     if triggered then play D else play C

-- If last move resulted in a "big" payoff, do it again, otherwise switch.
pavlov = "Pavlov" `plays`
    (randomly `atFirstThen`
     do p <- my (prev payoff)
        m <- my (prev move)
        return $ if p > 1 then m else
          if m == C then D else C)

-- Made-up strategy: Pick randomlyly until we have a lead, then
-- preserve it by repeatedly choosing D.
preserver = "Preserver" `plays`
    (randomly `atFirstThen`
     do me <- my score
        he <- his score
        if me > he then return D else randomly)

axelrod ps = roundRobin pd ps (times 100 >> printScore)

{- Playing around with syntax...

a -! f = (liftM2 f) a
(!-) = ($)

(?) :: Monad m => m Bool -> (m a, m a) -> m a
mb ? (t,f) = mb >>= \b -> if b then t else f

preserver2 = "Preserver" `plays`
    (randomly `atFirstThen`
      (my score -! (>) !- his score ? (return D, randomly)))
-}
