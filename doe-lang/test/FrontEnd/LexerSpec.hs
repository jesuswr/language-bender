module FrontEnd.LexerSpec (spec) where

import Test.Hspec
import qualified FrontEnd.Lexer as L


spec :: Spec
spec = do 
    describe "scanTokens" $ do
        it "tokenize string and parse errors if finds some" $ do
            L.scanTokens "-- asdaa" `shouldBe` ([], []) 

 