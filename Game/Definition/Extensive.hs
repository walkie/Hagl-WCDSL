module Game.Definition.Extensive where

import Data.List
import Data.Maybe
import Game.Definition

-- A simple extensive-form game tree.
data Extensive m v = Tn Int [(m, Extensive m v)]
                   | Pay [v] deriving (Eq, Show)

-- Add a branch to an extensive form node.
(<|>) :: (Eq m, Num v) => Extensive m v -> (m, Extensive m v) -> Extensive m v
Tn t ms <|> m = Tn t (m:ms)

-- Add two extensive form payoffs.
(<+>) :: (Eq m, Num v) => Extensive m v -> Extensive m v -> Extensive m v
Pay vs1 <+> Pay vs2 = Pay $ map sum $ transpose [vs1, vs2]

-- Construct a game from an extensive form representation.
extensive :: (Eq m, Num v) => Extensive m v -> GameDef Int m v
extensive tree =
    let bfo :: Extensive m v -> [Extensive m v]
        bfo t = let children (Pay _) = []
                    children (Tn _ ms) = snd $ unzip ms
                    expand [] = []
                    expand ts = ts ++ expand (concatMap children ts)
                in expand [t]
        trees = bfo tree
        ids = zip trees $ take (length trees) [0..]
        convert (Pay v) = Payoff v End
        convert t@(Tn n ms) = Turn n (fromJust $ lookup t ids) 
                                (\m -> convert $ fromJust $ lookup m ms)
        avail = (map (\(Tn _ ms) -> fst $ unzip ms) trees !!)
        infoGrp d = [convert $ fromJust $ revLookup d ids]
    in GameDef 2 (convert tree) infoGrp avail

-- Reverse lookup in lookup list
revLookup :: Eq b => b -> [(a,b)] -> Maybe a
revLookup b' t = lookup b' $ map (\(a,b) -> (b,a)) t
