module Hagl (

  -- Hagl.Core
  Game(..), Player(..), GameM(..),
  Name, PlayerIx, Payoff, Dist,
  Moved, Transcript, History, Summary,
  ExecM, StratM, Strategy,
  plays, update, evalGame, execGame, runStrategy,
  ByGame(..), ByPlayer(..), forGame, forPlayer, 
  fromList, toList, toList2,

  -- Hagl.Exec
  game, players, gameState, playerIx, transcript, history,
  myIx, numGames, numPlayers, isFirstGame,
  transcripts, summaries, moves, move, payoff, score, movesSoFar,
  randomlyFrom, fromDist,

  -- Hagl.Exec.Run
  once, times, conclude,
  
  -- Hagl.Exec.Print
  print, printLn, printStr, printStrLn,
  printTranscript, printTranscriptOfGame,
  printSummaries, printSummaryOfGame, printScore,

  -- Hagl.Strategy
  
  

  -- Hagl.Game
  winner, loser, tie, nextPlayer,
  decide, allPlayers, takeTurns, marginal, startTurn, endTurn,
  chanceMoved, playerMoved, genericMoved,
  setPlayerIx, putGameState, updateGameState, getPlayer, setPlayer,


) where

import Hagl.Core

import Prelude hiding (print)
