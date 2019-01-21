import Test.Hspec
import Test.QuickCheck
import Common
import LL
import qualified Data.Map as M
import qualified Data.Set as S 
import Parser

main :: IO ()
main = hspec $ do
  describe "grammar" $ do
    describe "arith grammar" $ do 
        let grammar = [ Prod (NonTerm "E") [NonTerm "T", NonTerm "E'"]            -- ^ E -> T E'
                      , Prod (NonTerm "E'") [Term "+", NonTerm "T", NonTerm "E'"] -- ^ E -> + T E |
                      , Prod (NonTerm "E'") [Epsilon]
                      , Prod (NonTerm "T") [NonTerm "F", NonTerm  "T'"]           -- ^ E -> T -> F T'
                      , Prod (NonTerm "T'") [Term "*", NonTerm "F", NonTerm "T'"] -- ^ T' -> * F T | 
                      , Prod (NonTerm "T'") [Epsilon]
                      , Prod (NonTerm "F") [Term "(", NonTerm "E", Term ")"]      -- ^ F -> ( E ) | id
                      , Prod (NonTerm "F") [Term "id"]
                      ]
            fiset = firstSet grammar
            foset = followSet grammar
        it "first(F)" $ do 
            fiset M.!? (NonTerm "F") `shouldBe` Just (S.fromList [Term "id", Term "("])
        it "first(F) == first(T)" $ do 
            fiset M.!? (NonTerm "F") `shouldBe` fiset M.!? (NonTerm "T")
        it "first(E) == first(F)" $ do 
            fiset M.!? (NonTerm "F") `shouldBe` fiset M.!? (NonTerm "T")
        it "first(E')" $ do
            fiset M.!? (NonTerm "E'") `shouldBe` Just (S.fromList [Term "+", Epsilon])
        it "first(T')" $ do 
            fiset M.!? (NonTerm "T'") `shouldBe` Just (S.fromList [Term "*", Epsilon])
        it "follow(E)" $ do 
            foset M.!? (NonTerm "E") `shouldBe` Just (S.fromList [Term ")", Dollar])
        it "follow(E)==follow(E')" $ do 
            foset M.!? (NonTerm "E") `shouldBe` foset M.!? (NonTerm "E'") 
        it "follow(T)" $ do 
            foset M.!? (NonTerm "T") `shouldBe` Just (S.fromList [Term "+", Term ")", Dollar])
        it "follow(T) == follow(T')" $ do 
            foset M.!? (NonTerm "T") `shouldBe` foset M.!? (NonTerm "T'")
        it "follow(F)" $ do 
            foset M.!? (NonTerm "F") `shouldBe` Just (S.fromList [Term "+", Term "*", Term ")", Dollar])
        
        describe "predicitve analysis table" $ do 
            let table = predictiveAnalysisTable grammar
            it "M[(E, id)] = { E -> T E'}" $ do
                table M.! (NonTerm "E", Term "id") `shouldBe` S.singleton (Prod (NonTerm "E") [NonTerm "T", NonTerm "E'"])
            it "M[(E, ()] = { E -> T E'}" $ do 
                table M.! (NonTerm "E", Term "(") `shouldBe` S.singleton (Prod (NonTerm "E") [NonTerm "T", NonTerm "E'"])
            it "M[(E', +] = { E -> + T E'}" $ do 
                table M.! (NonTerm "E'", Term "+") `shouldBe` S.singleton (Prod (NonTerm "E'") [Term "+", NonTerm "T", NonTerm "E'"])
            it "M[(E', ))] = { E' -> T E'}" $ do 
                table M.! (NonTerm "E'", Term ")") `shouldBe` S.singleton (Prod (NonTerm "E'") [Epsilon])
            it "M[(E', $)] = { E' -> '}" $ do 
                table M.! (NonTerm "E'", Dollar) `shouldBe` S.singleton (Prod (NonTerm "E'") [Epsilon])
            it "M[(T, id)] = { T -> F T' }" $ do 
                table M.! (NonTerm "T", Term "id") `shouldBe` S.singleton (Prod (NonTerm "T") [NonTerm "F", NonTerm "T'"])
            it "M[(T, ()] = { T -> F T' }" $ do 
                table M.! (NonTerm "T", Term "(") `shouldBe` S.singleton (Prod (NonTerm "T") [NonTerm "F", NonTerm "T'"])
            it "M[(T', +] = { T' -> }" $ do
                table M.! (NonTerm "T'", Term "+") `shouldBe` S.singleton (Prod (NonTerm "T'") [Epsilon])
            it "M[(T', *)] = { T' -> * F T' }" $ do 
                table M.! (NonTerm "T'", Term "*") `shouldBe` S.singleton (Prod (NonTerm "T'") [Term "*", NonTerm "F", NonTerm "T'"])
            it "M[(T', )] = { T' -> }" $ do 
                table M.! (NonTerm "T'", Term ")") `shouldBe` S.singleton (Prod (NonTerm "T'") [Epsilon])
            it "M[(T', $)] = { T' -> }" $ do 
                table M.! (NonTerm "T'", Dollar) `shouldBe` S.singleton (Prod (NonTerm "T'") [Epsilon])
            it "M[(F, id)] = { F -> id }" $ do 
                table M.! (NonTerm "F", Term "id") `shouldBe` S.singleton (Prod (NonTerm "F") [Term "id"])
            it "M[(F, ()] = { F -> ( E ) }" $ do 
                table M.! (NonTerm "F", Term "(") `shouldBe` S.singleton (Prod (NonTerm "F") [Term "(", NonTerm "E", Term ")"])

        let grammar = [ Prod (NonTerm "S") [NonTerm "A", Term "x"]
                      , Prod (NonTerm "S") [NonTerm "B", Term "y"] 
                      , Prod (NonTerm "S") [Term "z"]
                      , Prod (NonTerm "A") [Term "1", NonTerm "C", NonTerm "B"]
                      , Prod (NonTerm "A") [Term "2", NonTerm "B"] 
                      , Prod (NonTerm "B") [Term "3", NonTerm "B"] 
                      , Prod (NonTerm "B") [NonTerm "C"]
                      , Prod (NonTerm "C") [Term "4"] 
                      , Prod (NonTerm "C") [Epsilon]
                      ]
            fiset = firstSet grammar
            foset = followSet grammar
        it "first(A)" $ do 
            fiset M.!? (NonTerm "A") `shouldBe` Just (S.fromList [Term "2", Term "1"])
        it "first(C)" $ do 
            fiset M.!? (NonTerm "C") `shouldBe` Just (S.fromList [Term "4", Epsilon])
        it "first(S)" $ do 
            fiset M.!? (NonTerm "S") `shouldBe` Just (S.fromList [Term "4", Term "z", Term "3", Term "y", Term "2", Term "1"])
        it "first(B)" $ do 
            fiset M.!? (NonTerm "B") `shouldBe` Just (S.fromList [Term "4", Term "3", Epsilon])

        describe "grammar parser" $ do
            it "ok" $ do
                Just grammar `shouldBe` (parseGrammarMaybe "S->A x | B y | z; A -> 1 C B | 2 B; B -> 3 B | C ; C -> 4 |;")
