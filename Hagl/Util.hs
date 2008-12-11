module Hagl.Util where

import Control.Monad.State
import Data.List

-- TODO: Organize this file, figure out what's needed where, etc.

update :: MonadState s m => (s -> s) -> m s
update f = modify f >> get
