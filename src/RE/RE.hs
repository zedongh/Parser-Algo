{-# LANGUAGE DeriveFunctor #-}
module RE.RE where 

import Data.List (nub)
import Automata.NFA
import Control.Monad.State


data RE a = Alt (RE a) (RE a)            -- r1 | r2 
          | Seq (RE a) (RE a)        -- r1 r2
          | Repeat (RE a)                 -- r*
          | Single a                    -- a
          | Epsilon
          deriving (Show, Functor)

-- Using Nothing for epsilon trans
re2nfa :: Eq a => RE (Maybe a) -> NFA (Maybe a)
re2nfa re = evalState (re2nfas re) 0

pick :: State Int Int
pick = do 
    i <- get
    modify (+1)
    return i

re2nfas :: Eq a => RE (Maybe a) -> State Int (NFA (Maybe a))
re2nfas (Alt r1 r2) = do     
    i <- pick
    (NFA edges1 s1 f1 charset1) <- re2nfas r1
    (NFA edges2 s2 f2 charset2) <- re2nfas r2
    j <- pick
    let edges = [(i, s1, Nothing), (i, s2, Nothing), (f1, j, Nothing), (f2, j, Nothing)] ++ edges1 ++ edges2
        charset = nub $ charset1 ++ charset2
    return $ NFA edges i j charset
re2nfas (Seq r1 r2) = do 
    NFA edges1 s1 f1 charset1 <- re2nfas r1
    modify (\x -> x - 1)
    NFA edges2 s2 f2 charset2 <- re2nfas r2
    if f1 /= s2 
        then error "should be impossible case, concat r1 final state should be r2 start state"
        else return $ NFA (edges1 ++ edges2) s1 f2 (nub $ charset1 ++ charset2)
re2nfas (Repeat r) = do 
    i <- pick
    (NFA edges s f charset) <- re2nfas r
    j <- pick
    let nedges = (i, j, Nothing):(i, s, Nothing):(f, s, Nothing):(f, j, Nothing):edges
    return $ NFA nedges i j charset
--              a
-- start -> i -----> [j]  final state
re2nfas (Single a) = do
    i <- pick 
    j <- pick
    return $ singleton i j a
re2nfas Epsilon = return $ NFA [] 0 0 []