module FrontEnd.LexerSpec (spec) where

import Test.Hspec
import qualified FrontEnd.Lexer as L
import qualified FrontEnd.Tokens as TK
import qualified FrontEnd.Errors as E


spec :: Spec
spec = do 
    describe "scanTokens" $ do
        it "tokenize string and parse errors if finds some" $ do
            L.scanTokens "-- asdaa" `shouldBe` ([], []) 
        it "tokenize string of simple example" $ do
            L.scanTokens "aang is 5" `shouldBe` ([],[TK.TKid {TK.pos = (1,1), TK.id = "aang"},TK.TKis {TK.pos = (1,6)},TK.TKint {TK.pos = (1,9), TK.numI = 5}])
        it "tokenize bad id and return error" $ do
            L.scanTokens "_aang is 5" `shouldBe` ([E.LexerError {E.pos = (1,1), E.errorMessage = "Unexpected element: _"}],[TK.TKid {TK.pos = (1,2), TK.id = "aang"},TK.TKis {TK.pos = (1,7)},TK.TKint {TK.pos = (1,10), TK.numI = 5}])
