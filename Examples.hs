import Data.List
import Game
import Game.Definition

------------------------
-- Prisoner's Dilemma --
------------------------

-- Game definition
data PDMove = Cooperate | Defect deriving (Show, Eq)

pd :: Game PDMove Int
pd = matrix [Cooperate, Defect] [[2, 2], [0, 3], [3, 0], [1, 1]]

-- Some simple players.
fink = Player "Fink" (pure Defect)
mum = Player "Mum" (pure Cooperate)
randy = Player "Randy" random
roulette = Player "Russian Roulette" (mixed [(5, Cooperate), (1, Defect)])

-- The famous Tit-for-Tat.
titForTat = Player "Tit-for-Tat" strategy
  where strategy = isFirstGame >>= \isFirst ->
          if isFirst then return Cooperate 
          else his $ prev move

-- The Grim Trigger: Cooperates until opponent defects, then defects forever.
grim = Player "Grim Trigger" strategy
  where strategy =
          do ms <- his `each` every move
             return $ if Defect `elem` ms then Defect else Cooperate

-- Made-up strategy: Pick randomly until we have a lead, then
-- preserve it by repeatedly choosing Defect.
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
ussr = decision 1
usa  = decision 2

nuclearWar    = Payoff [-100,-100] :: GameTree String Int
nukesInCuba   = Payoff [   1,  -1] :: GameTree String Int
nukesInTurkey = Payoff [  -1,   1] :: GameTree String Int
usaLooksGood  = Payoff [   0,   1] :: GameTree String Int
ussrLooksGood = Payoff [   1,   0] :: GameTree String Int

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

-----------------
-- Tic Tac Toe --
-----------------

data Square = X | O | Empty deriving (Eq, Show)
type Board = [Square]
type Move = (Int, Square)

empty :: Board -> [Int]
empty = elemIndices Empty

who :: Board -> Int
who b = ((length (empty b) + 1) `mod` 2) + 1

avail :: Board -> [Move]
avail b = if pay b /= [0,0] then []
          else zip (empty b) (repeat ([X,O] !! (who b - 1)))

exec :: Board -> Move -> Board
exec b (i, m) = take i b ++ m : drop (i+1) b

pay :: Board -> [Int]
pay [X,X,X,_,_,_,_,_,_] = [1,0]
pay [_,_,_,X,X,X,_,_,_] = [1,0]
pay [_,_,_,_,_,_,X,X,X] = [1,0]
pay [X,_,_,X,_,_,X,_,_] = [1,0]
pay [_,X,_,_,X,_,_,X,_] = [1,0]
pay [_,_,X,_,_,X,_,_,X] = [1,0]
pay [X,_,_,_,X,_,_,_,X] = [1,0]
pay [_,_,X,_,X,_,X,_,_] = [1,0]
pay [O,O,O,_,_,_,_,_,_] = [0,1]
pay [_,_,_,O,O,O,_,_,_] = [0,1]
pay [_,_,_,_,_,_,O,O,O] = [0,1]
pay [O,_,_,O,_,_,O,_,_] = [0,1]
pay [_,O,_,_,O,_,_,O,_] = [0,1]
pay [_,_,O,_,_,O,_,_,O] = [0,1]
pay [O,_,_,_,O,_,_,_,O] = [0,1]
pay [_,_,O,_,O,_,O,_,_] = [0,1]
pay _ = [0,0]

ticTacToe = stateGame 2 who avail exec pay (take 9 (repeat Empty))
