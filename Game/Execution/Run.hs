module Game.Execution.Run where

import Control.Monad.State
import Data.List
import Game.Definition
import Game.Execution
import Game.Execution.Strategy

--------------------
-- Game Execution --
--------------------

runGame :: GameDef d m v -> [Player d m v] -> Game d m v a -> IO (ExecState d m v)
runGame def ps f = execStateT f $ initState def ps

step :: (Show m, Num v) => Game d m v ()
step = do state <- get
          es <- events
          case _location state of
            (Turn t _ next) -> 
              do m <- strategy $ _players state !! (t-1)
                 put state {
                   _location = next m,
                   _events = MoveEvent t m : es}
            (Payoff vs t) -> 
              put state {
                _location = t, 
                _events = PayoffEvent vs : es}
            End -> do root <- gameTree
                      summary <- summarize
                      put state {
                        _location = root,
                        _events = [],
                        _history = ByGame (_events state : asList (_history state)),
                        _summaries = ByGame (summary : asList (_summaries state)) }

once :: (Show m, Num v) => Game d m v ()
once = do loc <- location
          case loc of
            End -> step
            _ -> step >> once 
                       
times :: (Show m, Num v) => Int -> Game d m v ()
times 0 = return ()
times n = once >> times (n-1)

------------------------
-- Printing Functions --
------------------------

printLocation :: (Show m, Show v) => Game d m v ()
printLocation =
    do loc <- location
       ms <- getAvailMoves
       liftIO $ putStr $ showTree ms loc

printTranscript :: (Show m, Num v) => Game d m v ()
printTranscript = 
    do n <- numGames
       let game i | i == n = printTranscriptOfGame i
                  | otherwise = printTranscriptOfGame i >> game (i+1)
        in game 1

printTranscriptOfGame :: (Show m, Num v) => Int -> Game d m v ()
printTranscriptOfGame n =
    do ByGame h <- history
       ps <- players
       liftIO $ putStrLn $ "Game "++show n++ ":"
       let g = reverse $ h !! (length h - n)
        in liftIO $ putStr $ unlines $ map (showEvent ps) g
  where showEvent ps (MoveEvent i m) = "  " ++ show (ps !! (i-1)) ++ "'s move: " ++ show m
        showEvent _ (PayoffEvent vs) = "  Payoff: " ++ show vs

printSummaries :: (Show m, Num v) => Game d m v ()
printSummaries =
    do n <- numGames
       let game i | i == n = printSummaryOfGame i
                  | otherwise = printSummaryOfGame i >> game (i+1)
        in game 1

printSummaryOfGame :: (Show m, Num v) => Int -> Game d m v ()
printSummaryOfGame n = 
    do ByGame ss <- summaries
       ps <- players
       liftIO $ putStrLn $ "Summary of Game "++show n++":"
       let (ByPlayer mss, ByPlayer vs) = ss !! (length ss - n)
        in do liftIO $ putStr $ unlines ["  "++show p++" moves: "++show ms | (p,ms) <- zip ps mss]
              liftIO $ putStrLn $ "  Score: "++show vs

printScore :: (Show m, Num v) => Game d m v ()
printScore = do s <- liftM2 scoreString players (our score)
                liftIO $ putStrLn "Score:"
                liftIO $ putStr s

---------------
-- Utilities --
---------------

initState :: GameDef d m v -> [Player d m v] -> ExecState d m v
initState def ps = ExecState def ps (_gameTree def) [] (ByGame []) (ByGame [])

summarize :: (Show m, Num v) => Game d m v (EventSummary m v)
summarize = do es <- events
               np <- numPlayers
               return $ s es (take np (repeat []),[])
  where s (MoveEvent i m : es) (mss, vs) =
            s es ((take (i-1) mss) ++ (m:(mss!!(i-1))) : drop i mss, vs)
        s (PayoffEvent vs' : es) (mss, vs) =
            s es (mss, map sum (transpose [vs', vs]))
        s [] (mss, vs) = (ByPlayer mss, ByPlayer vs)
