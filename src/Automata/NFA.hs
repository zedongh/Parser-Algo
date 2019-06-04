module Automata.NFA where 

-- NFA representation as transition graph
-- for simplicity, only one start State and one final state, 
-- even though every state connect to final state with epsilon also is final state
data NFA a = 
    NFA { edges :: [(Int, Int, a)]
        , startState  :: Int
        , finalState :: Int
        , charset :: [a]
        }
    deriving (Show, Eq)

singleton :: Int -> Int -> a -> NFA a
singleton i j a = NFA [(i, j, a)] i j [a]