{-# OPTIONS_GHC -fglasgow-exts #-}

module Examples.Dice where

import Hagl

------------------
-- Dice Rolling --
------------------

type Sides = Int
data Die = Die Sides deriving (Eq, Show)

-- TODO
