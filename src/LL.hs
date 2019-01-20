module LL where 

import Common
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M 
import qualified Data.Set as S 

updateDefault :: (Ord a, Ord b) => Map a (Set b) -> a -> b -> Map a (Set b)
updateDefault mp k prod = 
    case mp M.!? k of
        Just v -> M.insert k (S.insert prod v) mp
        Nothing -> M.insert k (S.singleton prod) mp 

-- | using first set & follow set construct predicate table
--  A -> B => forall a in first(B); M[A, a] contains { A -> B }
--  if epsilon in first(B) => forall b in follow(A); M[A, b] contains { A -> B }
--  if epsilon and $ both in first(B) => M[A, $] contains { A -> B }
updateTable :: SymMap -> SymMap -> Map (Sym, Sym) (Set Prod) -> Prod -> Map (Sym, Sym) (Set Prod)
updateTable fiset foset table prod@(Prod nt ts) = 
    let fset = first ts fiset
        table1 = S.foldl (\tab t -> updateDefault tab (nt, t) prod) table (S.delete Epsilon fset)
        oset = foset M.! nt
        table2 = if S.member Epsilon fset then S.foldl (\tab t -> updateDefault tab (nt, t) prod) table1 oset else table1
        table3 = if S.member Epsilon fset && S.member Dollar oset then updateDefault table2 (nt, Dollar) prod else table2
    in table3

predictiveAnalysisTable :: Grammar -> Map (Sym, Sym) (Set Prod)
predictiveAnalysisTable grammar = 
    let fiset = firstSet grammar
        foset = followSetWith fiset grammar
    in foldl (updateTable fiset foset) M.empty grammar 