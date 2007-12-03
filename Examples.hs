import Control.Monad
import Data.List
import Game

------------------------
-- Prisoner's Dilemma --
------------------------

-- Game definition
data PDMove = Cooperate | Defect deriving (Show, Eq)

pd :: GameDef Int PDMove Int
pd = matrix [Cooperate, Defect] [[2, 2], [0, 3], [3, 0], [1, 1]]

-- Some simple players.
fink = Player "Fink" (pure Defect)
mum = Player "Mum" (pure Cooperate)
randy = Player "Randy" random
roulette = Player "Russian Roulette" (mixed [(5, Cooperate), (1, Defect)])

-- The famous Tit-for-Tat.
titForTat :: Player Int PDMove Int
titForTat = Player "Tit-for-Tat" strategy
  where strategy = isFirstGame >>= \isFirst ->
          if isFirst then return Cooperate 
          else his $ prev move

-- The Grim Trigger: Cooperates until opponent defects, then defects forever.
grim :: Player Int PDMove Int
grim = Player "Grim Trigger" strategy
  where strategy =
          do ms <- his `each` every move
             return $ if Defect `elem` ms then Defect else Cooperate

-- Made-up strategy: Pick randomly until we have a lead, then
-- preserve it by repeatedly choosing Defect.
preserver :: Player Int PDMove Int
preserver = Player "Preserver" strategy
  where strategy = isFirstGame >>= \isFirst ->
          if isFirst then random
          else do me <- my score
                  he <- his score
                  if me > he then return Defect
                   else random

-- To run (example):
--runGame pd [randy, titForTat] (times 10 >> printTranscript >> printScores)

--------------------------
-- Cuban Missile Crisis --
--------------------------
ussr m = Tn 1 [m]
usa  m = Tn 2 [m]

nuclearWar    = Pay [-100,-100]
nukesInCuba   = Pay [   1,  -1]
nukesInTurkey = Pay [  -1,   1]
usaLooksGood  = Pay [   0,   1]
ussrLooksGood = Pay [   1,   0]

start = ussr ("Send Missiles to Cuba", usaResponse) 
         <|> ("Do Nothing", nukesInTurkey)

usaResponse = usa ("Do Nothing", nukesInTurkey <+> nukesInCuba <+> ussrLooksGood)
              <|> ("Blockade", ussrBlockadeCounter)
              <|> ("Invade", ussrInvasionCounter)

ussrBlockadeCounter = ussr ("Agree to Terms", usaLooksGood) 
                       <|> ("Escalate", nuclearWar)

ussrInvasionCounter = ussr ("Pull Out", nukesInTurkey <+> usaLooksGood) 
                       <|> ("Escalate", nuclearWar)

crisis = extensive start

--------------------
-- Extensive Form --
--------------------

-- An example of defining an extensive form game with arbitrary imperfect info
data ABC = A | B | C deriving (Eq, Show)
abc t _ _ A = t
abc _ t _ B = t
abc _ _ t C = t
t1 = Turn 1 0 $ abc t2a t2b t2c
t2a = Turn 2 1 $ abc (Payoff [0,0] End) (Payoff [2,1] End) (Payoff [1,3] End)
t2b = Turn 2 2 $ abc (Payoff [1,2] End) (Payoff [3,1] End) (Payoff [0,0] End)
t2c = Turn 2 1 $ abc (Payoff [3,0] End) (Payoff [1,2] End) (Payoff [0,3] End)
g = GameDef 2 t1 ([[t1],[t2a,t2c],[t2b]] !!) (\_ -> [A,B,C])
