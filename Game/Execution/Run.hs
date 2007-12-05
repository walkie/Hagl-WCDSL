module Game.Execution.Run where

import Control.Monad.State
import Data.Maybe
import Game.Definition
import Game.Execution
import Game.Execution.Util

--------------------
-- Game Execution --
--------------------

runGame :: Game m v -> [Player m v] -> GameExec m v a -> IO (ExecState m v)
runGame g ps f = execStateT f $ initState g ps

step :: (Eq m, Num v) => GameExec m v ()
step = get >>= \state ->
    let t = _location state in case t of
      Decision t next ->
        do m <- strategy $ _players state !! (t-1)
           put state { _location = fromJust $ lookup m next,
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

once :: (Eq m, Num v) => GameExec m v ()
once = do loc <- location
          case loc of
            Payoff _ -> step
            _ -> step >> once 
                       
times :: (Eq m, Num v) => Int -> GameExec m v ()
times 0 = return ()
times n = once >> times (n-1)

---------------
-- Utilities --
---------------

initState :: Game m v -> [Player m v] -> ExecState m v
initState game ps = ExecState game ps (tree game) [] (ByGame [])

summarize :: (Num v) => Game m v -> Transcript m v -> Summary m v
summarize g t = 
    let np = numPlayers g
        addmove i a as = take i as ++ ((a:(as!!i)) : drop i as)
        payoffs (PayoffEvent vs : es) = [a + b | (a, b) <- zip vs (payoffs es)]
        payoffs (e : es) = payoffs es
        payoffs [] = take np (repeat 0)
        moves (DecisionEvent i m : es) = addmove (i-1) m (moves es)
        moves (e : es) = moves es
        moves [] = take np (repeat [])
    in (ByPlayer (moves t), ByPlayer (payoffs t))
