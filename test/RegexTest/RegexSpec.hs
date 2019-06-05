module RegexTest.RegexSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import RE.RE (RE(..))
import RE.Parser (regex)

spec :: Spec
spec = do 
    describe "primary test" $ do 
        it "epsilon" $ do 
            regex "" `shouldBe` Right Epsilon
        it "a" $ do 
            regex "a" `shouldBe` Right (Single 'a')
        it "ab" $ do 
            regex "ab" `shouldBe` Right (Seq (Single 'a') (Single 'b'))
        it "a|b" $ do 
            regex "a|b" `shouldBe` Right (Alt (Single 'a') (Single 'b'))
        it "(a)" $ do 
            regex "(a)" `shouldBe` Right (Single 'a')
        it "a*" $ do 
            regex "a*" `shouldBe` Right (Repeat (Single 'a'))
    
    describe "combinator test" $ do 
        it "|a" $ do 
            regex "|a" `shouldBe` Right (Alt Epsilon (Single 'a'))
        it "a**" $ do 
            regex "a**" `shouldBe` Right (Repeat (Repeat (Single 'a')))
        it "**" $ do 
            regex "**" `shouldBe`  Right (Repeat (Repeat Epsilon))
        it "(a|b)*" $ do 
            regex "(a|b)*" `shouldBe` Right (Repeat (Alt (Single 'a') (Single 'b')))
        it "a|b|c|d" $ do 
            regex "a|b|c|d" `shouldBe` Right (Alt (Alt (Alt (Single 'a') (Single 'b')) (Single 'c')) (Single 'd'))
        it "||||" $ do 
            regex "||||" `shouldBe` Right (Alt (Alt (Alt (Alt Epsilon Epsilon) Epsilon) Epsilon) Epsilon)
        it "(|a|)*" $ do 
            regex "(|a|)*" `shouldBe` Right (Repeat (Alt (Alt Epsilon (Single 'a')) Epsilon))
        it "abcd" $ do 
            regex "abcd" `shouldBe` Right (Seq (Seq (Seq (Single 'a') (Single 'b')) (Single 'c')) (Single 'd'))
        it "abc*" $ do 
            regex "abc*" `shouldBe` Right (Seq (Seq (Single 'a') (Single 'b')) (Repeat (Single 'c')))
        it "abc*|a*" $ do 
            regex "abc*|a*" `shouldBe` Right (Alt (Seq (Seq (Single 'a') (Single 'b')) (Repeat (Single 'c'))) (Repeat (Single 'a')))
        it "abc*|ab*" $ do
            regex "abc*|ab*" `shouldBe` Right (Alt (Seq (Seq (Single 'a') (Single 'b')) (Repeat (Single 'c'))) (Seq (Single 'a') (Repeat (Single 'b'))))



