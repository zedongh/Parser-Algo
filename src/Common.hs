module Common where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)

data Sym = Term String 
         | NonTerm String 
         | Epsilon
         | Dollar -- should be construct be user, only using for follow set
         deriving (Show, Eq, Ord)

data Prod = Prod Sym [Sym] deriving (Show, Eq, Ord)

type Grammar = [Prod]
type SymSet = Set Sym 
type SymMap = Map Sym SymSet

-- | first(aBc)
--    = first (e) -> e
--      first (aB) -> a
--      first (ABC) -> if e in first(A) then first(A) \ {e} U first(BC) else first(A)
first :: [Sym] -> SymMap -> SymSet
first [] _ = S.singleton Epsilon
first (Epsilon:ts) mp = first ts mp
first (t@(Term _): _ ) _ = S.singleton t
first (t@(NonTerm _): ts) mp = 
    let fset = M.findWithDefault S.empty t mp
    in if S.member Epsilon fset then S.union (S.delete Epsilon fset) (first ts mp) else fset

-- | first(A -> B) => { A: first(B) }
updateFirst :: SymMap -> Prod -> SymMap
updateFirst oldMap (Prod nt ts) = M.insertWith S.union nt (first ts oldMap) oldMap

fixpoint :: (Eq a, Show a)=> (a -> a) -> a -> a
fixpoint f x = if f x == x then x else fixpoint f (f x)

-- | apply to whole grammar
firstSet grammar = fixpoint magic M.empty
    where magic m = foldl updateFirst m grammar

-- | follow(Ac) ->  c in follow(A)
-- | follow(X -> ABC) ->  if epsilon in first(BC) then first(BC)\{epsilon} U follow(X) in follow(A) else first(BC) in follow(A)
-- | follow(A -> B) follow(A) in follow(B)
updateFollow :: SymMap -> SymMap -> Prod -> SymMap
updateFollow firstSet followSet (Prod nt []) = followSet
updateFollow firstSet followSet (Prod nt (t@(NonTerm _): ts)) =
    let fset = first ts firstSet
        rest = updateFollow firstSet followSet (Prod nt ts)
    in if S.member Epsilon fset
        then let v = S.delete Epsilon fset `S.union` (M.findWithDefault S.empty t rest) `S.union` (M.findWithDefault S.empty nt followSet)
             in M.insert t v rest
        else M.insert t (fset `S.union` (M.findWithDefault S.empty t rest)) rest
updateFollow firstSet followSet (Prod nt (_:ts)) = updateFollow firstSet followSet (Prod nt ts)

-- | apply to whole grammar
followSet [] = error "grammar cannot be empty"
followSet grammar@((Prod nt _):_) = fixpoint magic (M.insert nt (S.singleton Dollar) M.empty)
    where magic m = foldl (updateFollow (firstSet grammar)) m grammar

followSetWith fiset [] = error "grammar cannot be empty"
followSetWith fiset grammar@((Prod nt _):_) = fixpoint magic (M.insert nt (S.singleton Dollar) M.empty)
    where magic m = foldl (updateFollow fiset) m grammar