module LR where

import Common (Sym(..), Prod(..), Grammar, fixpoint)
import Data.List (groupBy)
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Set as Set
import qualified Data.MultiMap as MM
import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShowId, trace)

data LRAutomataState = LRAutomataState { edges :: [(Int, Int, Sym)]           -- ^ for construct GOTO/Action table
                                       , start :: Int                         -- ^ entry node, may always be 0 ...
                                       , final :: Int                         -- ^ final node
                                       , nodeMap :: IMap.IntMap (Set.Set Prod)-- ^ for construct reduction entry
                                       , grammar :: Grammar                   -- ^ for construct reduction nth entry
                                       , nodeIdx :: Map.Map (Set.Set Prod) Int -- ^ deal node
                                       , dealed :: Set.Set (Set.Set Prod)
                                       , current :: [Set.Set Prod]            -- ^ current to deal node
                                       } deriving (Show, Eq)

emptyLRAutomataState :: LRAutomataState
emptyLRAutomataState = LRAutomataState [] 0 0 IMap.empty [] Map.empty Set.empty []

prepare :: Grammar -> MM.MultiMap Sym Prod
prepare prods = MM.fromList $ map (\(Prod nt rights) -> (nt, Prod nt (Dot:rights))) prods

-- add dot for each production
augment :: Grammar -> Grammar
augment = map (\(Prod nt rights) -> Prod nt (Dot:rights))

closure :: MM.MultiMap Sym Prod -> Set.Set Prod -> Set.Set Prod
closure g = fixpoint (closure' g)

-- one step for compute term closure
closure' :: MM.MultiMap Sym Prod -> Set.Set Prod -> Set.Set Prod
closure' g prodSet = Set.fromList $ prods ++ concatMap (closurep g) prods
    where prods = Set.toList prodSet
          closurep :: MM.MultiMap Sym Prod -> Prod -> [Prod]
          closurep g (Prod nt (Dot:n@(NonTerm s):_)) = MM.lookup n g
          closurep _ (Prod nt (Dot:_)) = []
          closurep g (Prod nt (_:rights)) = closurep g (Prod nt rights)

-- augment grammar to make a new entry prod
entry :: Grammar -> Prod 
entry (Prod nt _:prods) = Prod Entry [Dot, nt, Dollar]

move :: Set.Set Prod -> Map.Map Sym (Set.Set Prod)
move prods = Map.map (Set.fromList) $ MM.toMap $ MM.fromList $ mapMaybe move' (Set.toList prods)

move' :: Prod -> Maybe (Sym, Prod)
move' (Prod nt [Dot]) = Nothing
move' (Prod nt (Dot:right:rights)) = Just (right, Prod nt (right:Dot:rights))
move' (Prod nt (right:rights)) = fmap (fmap $ \(Prod nt rs) -> (Prod nt (right:rs))) remain
    where remain = move' (Prod nt rights)
    
fromG :: Grammar -> IO LRAutomataState
fromG grammar = let grammar' = augment grammar
                    start = entry grammar'
                    gMap = prepare grammar
                    start' = closure gMap $ Set.singleton start
                    nMap = IMap.fromList [(0, start')]
                    nodeIdx = Map.fromList [(start', 0)]
                in runReaderT (execStateT iterator emptyLRAutomataState{ nodeMap = nMap, nodeIdx = nodeIdx, current = [start'] }) gMap

iterator :: StateT LRAutomataState (ReaderT (MM.MultiMap Sym Prod) IO) LRAutomataState
iterator = do 
    cur <- gets current
    modify $ \state -> state { current = [] } -- clear
    gMap <- ask
    handled <- gets dealed
    let cur' = filter (`Set.notMember` handled) cur
    if (null cur') 
        then get
        else do forM_ cur' $ \node -> do
                    indexMap <- gets nodeIdx
                    let i = indexMap Map.! node
                        trans = Map.toList $ move node
                    forM_ trans $ \(sym, prodSet) -> do 
                        let prodSet' = closure gMap prodSet
                        indexMap' <- gets nodeIdx
                        edges <- gets edges
                        current <- gets current
                        if prodSet' `Map.member` indexMap' 
                            then let j = traceShowId $ indexMap' Map.! prodSet' 
                                in do 
                                    modify $ \state -> state { edges = (i, j, sym):edges , current = prodSet':current }
                            else let indexMap'' = Map.insert prodSet' (Map.size indexMap') indexMap'
                                in do 
                                    modify $ \state -> state { edges = (i, Map.size indexMap'' - 1, sym):edges, nodeIdx = indexMap'', current = prodSet':current }
                    dealed <- gets dealed
                    modify $ \state -> state { dealed = Set.insert node dealed }
                iterator