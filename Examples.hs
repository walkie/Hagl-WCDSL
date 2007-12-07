import Data.List
import Game

------------------------
-- Prisoner's Dilemma --
------------------------

-- Game definition
data PDMove = Cooperate | Defect deriving (Show, Eq)

pd = matrix [Cooperate, Defect] [[2, 2], [0, 3], [3, 0], [1, 1]]

-- Some simple players.
fink = Player "Fink" (pure Defect)
mum = Player "Mum" (pure Cooperate)
cd = Player "(CD)*" (periodic [Cooperate, Defect])
dc = Player "(DC)*" (periodic [Cooperate, Defect])
ccd = Player "(CCD)*" (periodic [Cooperate, Cooperate, Defect])
randy = Player "Randy" random
roulette = Player "Russian Roulette" (mixed [(5, Cooperate), (1, Defect)])

-- The famous Tit-for-Tat.
titForTat = Player "Tit-for-Tat" $ 
    return Cooperate `firstThen` his (prev move)

-- Suspicious Tit-for-Tat (like Tit-for-Tat but defect on first move)
suspicious = Player "Suspicious Tit-for-Tat" $ 
    return Defect `firstThen` his (prev move)

-- Tit-for-Tat that only defects after two defects in a row.
titForTwoTats = Player "Tit-for-Two-Tats" $
    do ms <- his `each` prevn 2 move
       return $ if ms == [Defect, Defect] then Defect else Cooperate

-- The Grim Trigger: Cooperates until opponent defects, then defects forever.
grim = Player "Grim Trigger" $
    do ms <- his `each` every move
       return $ if Defect `elem` ms then Defect else Cooperate

-- If last move resulted in a "big" payoff, do it again, otherwise switch.
pavlov = Player "Pavlov" $
    random `firstThen`
    do p <- my (prev payoff)
       m <- my (prev move)
       return $ if p > 1 then m else
         if m == Cooperate then Defect else Cooperate

-- Made-up strategy: Pick randomly until we have a lead, then
-- preserve it by repeatedly choosing Defect.
preserver = Player "Preserver" $
    random `firstThen`
    do me <- my score
       he <- his score
       if me > he then return Defect else random

-- Running from GHCi:
-- > runGame pd [titForTat, pavlov] (times 10 >> printTranscript >> printScores)
-- > roundRobin pd [titForTat, titForTwoTats, grim, suspicious, pavlov] (times 100 >> printScore)

-------------------------
-- Rock Paper Scissors --
-------------------------

data RPSMove = Rock | Paper | Scissors deriving (Eq, Show)

rps = zerosum [Rock,Paper,Scissors] [0,-1, 1,
                                     1, 0,-1,
                                    -1, 1, 0]

-- Some simple players
rocky = Player "Stalone" $ pure Rock
rotate = Player "RPS" $ periodic [Rock, Paper, Scissors]
-- can reuse randy from above!

-- Play the move that will beat the move the opponent has played most.
frequency = Player "Huckleberry" $
    do ms <- his `each` every move
       let r = length $ filter (Rock ==) ms
           p = length $ filter (Paper ==) ms
           s = length $ filter (Scissors ==) ms
           x = maximum [r,p,s]
        in return $ if x == r then Paper else 
                    if x == p then Scissors
                              else Rock

--------------------------
-- Cuban Missile Crisis --
--------------------------

ussr = decision 1
usa  = decision 2

nuclearWar    = Payoff [-100,-100] :: GameTree String
nukesInCuba   = Payoff [   1,  -1] :: GameTree String
nukesInTurkey = Payoff [  -1,   1] :: GameTree String
usaLooksGood  = Payoff [   0,   1] :: GameTree String
ussrLooksGood = Payoff [   1,   0] :: GameTree String

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

------------------
-- Dice Rolling --
------------------

die = Game 1 (Chance (zip (repeat 1) (map (\a -> Payoff [a]) [1..6]))) perfect
roll n = runGame die [Player "Total" $ return ()] (times n >> printScore)

-----------------
-- Tic Tac Toe --
-----------------

data Square = X | O | Empty deriving (Eq, Show)
type Board = [Square]
type Move = (Int, Square)

empty :: Board -> [Int]
empty = elemIndices Empty

who :: Board -> Int
who b | odd (length (empty b)) = 1
      | otherwise = 2

avail :: Board -> [Move]
avail b = if pay b /= [0,0] then []
          else zip (empty b) (repeat ([X,O] !! (who b - 1)))

exec :: Board -> Move -> Board
exec b (i, m) = take i b ++ m : drop (i+1) b

pay :: Board -> [Float]
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
