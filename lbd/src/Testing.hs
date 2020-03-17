module Testing where

import Test.Hspec
import Test.Hspec.Attoparsec
import qualified Data.Text as T
import LambdaCalc
import qualified Data.Map as Map
import OutputMisc
import Parser

testset1 :: Spec
testset1 = do
  describe "test arithmetic" $ do
    it "should parse 'x+y+z*10-30+40'" $
       arith `shouldSucceedOn` ( T.pack("x + y + z * 10 - 30 + 40"))
    it "should parse 'x*1*y*2+3+4-5'" $
       arith `shouldSucceedOn` ( T.pack("x*1*y*2+3+4-5"))   
   
    
testset2 = do
    describe "test lambda" $ do
        it "should parse '\\x.x$5' " $
            expr `shouldSucceedOn` ( T.pack "\\x.x $ 5" )
        it "should parse '\\x y . x + y $' " $
            expr `shouldSucceedOn` ( T.pack "\\x y . x + y $" )
        it "should parse '\\x . \\ y . x + y $' " $
            expr `shouldSucceedOn` ( T.pack "\\x . \\y . x + y $ $" )
        it "should parse '\\x y . x + y $ (\\z.z+1 $ 10) (2 + 9)' " $
            expr `shouldSucceedOn` ( T.pack "\\x y . x + y $ (\\z.z+1 $ 10) (2 + 9)" )
        
testset3 = do
    describe "some evaluation tests" $ do
        it "'\\x y z . (x & y) | (x & z) | (y & z) $ True False False' evaluates to False" $
            runeval Map.empty "\\x y z . (x & y) | (x & z) | (y & z) $ True False False" `shouldBe` (Literal (XBool False))   
        it "\\x y z . (\\a.a+1 $ x) * (\\a.a+2 $ y) * (\\a.a+3 $ z) $ 0 0 0  evaluates to 6" $
            runeval Map.empty "\\x y z . (\\a.a+1 $ x) * (\\a.a+2 $ y) * (\\a.a+3 $ z) $ 0 0 0" `shouldBe` (Literal (XInt 6))
        it "\\x y z . 10 $ 500 250 False evaluates to 500" $
            runeval Map.empty "\\x y z . 10 $ 500 250 20" `shouldBe` (Literal (XInt 10))

test :: IO()
test = do
    hspec testset1
    hspec testset2
    hspec testset3