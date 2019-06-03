module Automata.NFA where 

import qualified Data.Set as Set

-- NFA representation as transition graph
data NFA a = 
    NFA { edges :: [(Int, Int, a)]
        , startState  :: Int
        , finalStates :: Int
        }
    deriving (Show, Eq)

singleton :: Int -> Int -> a -> NFA a
singleton i j a = NFA [(i, j, a)] i j