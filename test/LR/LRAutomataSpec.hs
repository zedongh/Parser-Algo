module LR.LRAutomataSpec (spec) where 

import Test.Hspec
import Test.QuickCheck
import Common
import LR
import qualified Data.Set as Set


spec :: Spec
spec = do 
    describe "term automata" $ do 
        describe "closure" $ do 
            let grammar = [ Prod (NonTerm "E") [NonTerm "E", Term "+", NonTerm "T"]  -- ^ E -> E + T | T
                          , Prod (NonTerm "E") [NonTerm "T"]                          
                          , Prod (NonTerm "T") [NonTerm "T", Term "*", NonTerm "F"]  -- ^ T -> T * F | F
                          , Prod (NonTerm "T") [NonTerm "F"]                          
                          , Prod (NonTerm "F") [Term "(", NonTerm "E", Term ")"]     -- ^ F -> ( E ) | id
                          , Prod (NonTerm "F") [Term "id"]
                          ]
                g = prepare grammar
                grammar' = augment grammar
            it "E' -> .E" $ do
                let prod = Set.fromList $ [Prod Entry [Dot, NonTerm "E", Dollar]]
                closure g prod `shouldBe` Set.fromList [ Prod Entry [Dot, NonTerm "E", Dollar]  -- ^ augment entry
                                                       , Prod (NonTerm "E") [Dot, NonTerm "E", Term "+", NonTerm "T"]
                                                       , Prod (NonTerm "E") [Dot, NonTerm "T"]
                                                       , Prod (NonTerm "T") [Dot, NonTerm "T", Term "*", NonTerm "F"]
                                                       , Prod (NonTerm "T") [Dot, NonTerm "F"]
                                                       , Prod (NonTerm "F") [Dot, Term "(", NonTerm "E", Term ")"]
                                                       , Prod (NonTerm "F") [Dot, Term "id"]
                                                       ]
            it "E -> E + .T" $ do 
                let prod = Set.fromList $ [Prod (NonTerm "E") [NonTerm "E", Term "+", Dot, NonTerm "T"]]
                closure g prod `shouldBe` Set.fromList [ Prod (NonTerm "E") [NonTerm "E", Term "+", Dot, NonTerm "T"]
                                                       , Prod (NonTerm "T") [Dot, NonTerm "T", Term "*", NonTerm "F"]
                                                       , Prod (NonTerm "T") [Dot, NonTerm "F"]
                                                       , Prod (NonTerm "F") [Dot, Term "(", NonTerm "E", Term ")"]
                                                       , Prod (NonTerm "F") [Dot, Term "id"]
                                                       ]
            it "T -> T * .F" $ do 
                let prod = Set.fromList $ [Prod (NonTerm "T") [NonTerm "T", Term "*", Dot, NonTerm "F"]]
                closure g prod `shouldBe` Set.fromList [ Prod (NonTerm "T") [NonTerm "T", Term "*", Dot, NonTerm "F"]
                                                       , Prod (NonTerm "F") [Dot, Term "(", NonTerm "E", Term ")"]
                                                       , Prod (NonTerm "F") [Dot, Term "id"]
                                                       ]
            it "F -> (.E)" $ do 
                let prod = Set.fromList $ [Prod (NonTerm "F") [Term "(", Dot, NonTerm "E", Term ")"]]
                closure g prod `shouldBe` Set.fromList [ Prod (NonTerm "F") [Term "(", Dot, NonTerm "E", Term ")"]
                                                       , Prod (NonTerm "E") [Dot, NonTerm "E", Term "+", NonTerm "T"]
                                                       , Prod (NonTerm "E") [Dot, NonTerm "T"]
                                                       , Prod (NonTerm "T") [Dot, NonTerm "T", Term "*", NonTerm "F"]
                                                       , Prod (NonTerm "T") [Dot, NonTerm "F"]
                                                       , Prod (NonTerm "F") [Dot, Term "(", NonTerm "E", Term ")"]
                                                       , Prod (NonTerm "F") [Dot, Term "id"]
                                                       ]
            it "LR(0) Automata" $ do 
                -- todo: LR(0) automata test
                True `shouldBe` True