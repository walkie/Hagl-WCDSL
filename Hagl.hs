-- This module provides a convenient way to import most of Hagl all at once.
-- Individual game types (e.g. normal form, extensive form, state-based)
-- should be imported separately, (Hagl.Normal, Hagl.Extensive,
-- Hagl.StateBased), as needed, since they contain overlapping function names.

module Hagl (

  -- Hagl.Core
  Game(..), Player(..), GameM(..),
  Name, PlayerIx, Payoff, Dist,
  Moved, Transcript, History, Summary,
  ExecM, StratM, Strategy,
  plays, update, evalGame, execGame, runStrategy,
  ByGame(..), ByPlayer(..),
  forGame, forPlayer, forGameM, forPlayerM,
  fromList, toList, toList2, randomlyFrom, fromDist,

  -- Hagl.Accessor
  game, players, gameState, playerIx, transcript, history,
  myIx, gameNumber, numGames, numPlayers, isFirstGame,
  transcripts, summaries, moves, move, payoff, score, movesSoFar,

  -- Hagl.Exec
  once, times, runGames,
  tournament, fullRoundRobin, roundRobin,

  -- Hagl.Game
  winner, loser, tie, nextPlayer,
  decide, allPlayers, takeTurns, marginal, startTurn, endTurn,
  chanceMoved, playerMoved, genericMoved,
  setPlayerIx, putGameState, updateGameState, getPlayer, setPlayer,
  
  -- Hagl.GameTree
  Edge, GameTree(..), Searchable(..),
  movesFrom, children, doMove, location, bfs, dfs, maxPlayer,
  gameTreeM, nextStateM, step, finish, runTree,
  
  -- Hagl.Print
  print, printLn, printStr, printStrLn,
  printTranscript, printTranscripts, printTranscriptOfGame,
  printSummary, printSummaries, printSummaryOfGame, printScore,
  
  -- Hagl.Selector
  each, my, his, her, our, their,
  every, first, firstn, this, prev, prevn,

  -- Hagl.Strategy
  play, pure, randomly, mixed, periodic, minimax,
  atFirst, next, finally, atFirstThen, initiallyThen

) where

import Hagl.Core
import Hagl.Accessor
import Hagl.Exec
import Hagl.Game
import Hagl.GameTree
import Hagl.Print
import Hagl.Selector
import Hagl.Strategy

import Prelude hiding (print)
