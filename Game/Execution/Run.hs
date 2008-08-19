module Game.Execution.Run where

import Control.Monad.State
import Data.Maybe
import Game.Definition
import Game.Execution
import Game.Execution.Util
import Game.Util

--------------------
-- Game Execution --
--------------------

evalGame :: Game m -> [Player m] -> GameExec m a -> IO a
evalGame g ps (ExecM f) = evalStateT f $ initState g ps

runGame :: Game m -> [Player m] -> GameExec m a -> IO (ExecState m)
runGame g ps (ExecM f) = execStateT f $ initState g ps

runStrategy :: StrategyState mv -> GameExec mv (mv, StrategyState mv)
runStrategy (StrategyState m s) = do (mv, s') <- runStateT (unS m) s
                                     return (mv, StrategyState m s')

step :: (Eq m) => GameExec m ()
step = get >>= \state ->
    let t = _location state in case t of
      Decision t next ->
        let (ph, (p:pt)) = splitAt (t-1) $ _players state 
        in do (m, s) <- runStrategy $ playerStrategy p
              put state { _players = ph ++ p { playerStrategy = s } : pt,
                          _location = fromJust $ lookup m next,
                          _transcript = DecisionEvent t m : _transcript state }
      Chance dist ->
        let expanded = expandDist dist
        in do i <- randomIndex expanded
              put state { _location = expanded !! i,
                          _transcript = ChanceEvent (branch dist i) : _transcript state }
      Payoff vs ->
        let transcript = PayoffEvent vs : _transcript state
            summary = summarize (_game state) transcript
            history = ByGame $ (transcript, summary) : asList (_history state)
        in put state { _location = tree $ _game state,
                       _transcript = [],
                       _history = history }

once :: (Eq m) => GameExec m ()
once = do loc <- liftM _location get
          case loc of
            Payoff _ -> step
            _ -> step >> once 
                       
times :: (Eq m) => Int -> GameExec m ()
times 0 = return ()
times n = once >> times (n-1)

---------------
-- Utilities --
---------------

initState :: Game m -> [Player m] -> ExecState m
initState game ps = ExecState game ps (tree game) [] (ByGame [])

summarize :: Game m -> Transcript m -> Summary m
summarize g t = 
    let np = numPlayers g
        addmove i a as = take i as ++ ((a:(as!!i)) : drop i as)
        payoffs (PayoffEvent vs : es) = zipWith (+) vs (payoffs es)
        payoffs (e : es) = payoffs es
        payoffs [] = take np (repeat 0)
        moves (DecisionEvent i m : es) = addmove (i-1) m (moves es)
        moves (e : es) = moves es
        moves [] = take np (repeat [])
    in (ByPlayer (moves t), ByPlayer (payoffs t))
