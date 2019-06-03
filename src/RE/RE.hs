{-# LANGUAGE DeriveFunctor #-}
module RE.RE where 

import qualified Data.Set as Set
import Automata.NFA
import Control.Monad.State


data RE a = Or (RE a) (RE a)            -- r1 | r2 
          | Concat (RE a) (RE a)        -- r1 r2
          | Star (RE a)                 -- r*
          | Single a                    -- a
          deriving (Show, Functor)

-- Using Nothing for epsilon trans
re2nfa :: RE (Maybe a) -> NFA (Maybe a)
re2nfa re = evalState (re2nfas re) 0

pick :: State Int Int
pick = do 
    i <- get
    modify (+1)
    return i

re2nfas :: RE (Maybe a) -> State Int (NFA (Maybe a))
re2nfas (Or r1 r2) = do     
    i <- pick
    (NFA edges1 s1 f1) <- re2nfas r1
    (NFA edges2 s2 f2) <- re2nfas r2
    j <- pick
    let edges = [(i, s1, Nothing), (i, s2, Nothing), (f1, j, Nothing), (f2, j, Nothing)] ++ edges1 ++ edges2
    return $ NFA edges i j
re2nfas (Concat r1 r2) = do 
    NFA edges1 s1 f1 <- re2nfas r1
    modify (\x -> x - 1)
    NFA edges2 s2 f2 <- re2nfas r2
    if f1 /= s2 
        then error "should be impossible case, concat r1 final state should be r2 start state"
        else return $ NFA (edges1 ++ edges2) s1 f2
re2nfas (Star r) = do 
    i <- pick
    (NFA edges s f) <- re2nfas r
    j <- pick
    let nedges = (i, j, Nothing):(i, s, Nothing):(f, s, Nothing):(f, j, Nothing):edges
    return $ NFA nedges i j
--              a
-- start -> i -----> [j]  final state
re2nfas (Single a) = do
    i <- pick 
    j <- pick
    return $ singleton i j a