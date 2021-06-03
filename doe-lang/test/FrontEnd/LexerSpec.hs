module FrontEnd.LexerSpec (spec) where

import Test.Hspec
import qualified FrontEnd.Lexer as L
import qualified FrontEnd.Tokens as TK
import qualified FrontEnd.Errors as E


atlaIntro = "Water. Earth. Fire. Air. Long ago, the four nations lived together in harmony. \
			\Then, everything changed when the Fire Nation attacked. \
			\Only the Avatar, master of all four elements, could stop them"


spec :: Spec
spec = do 
    describe "scanTokens" $ do
        it "ignores comments" $ do
            L.scanTokens "-- asdaa" `shouldBe` ([], []) 

        it "Ignore ATLA part of the intro" $ do
            L.scanTokens atlaIntro `shouldBe` ([], [])         

        it "tokenize bad id and return error" $ do
            L.scanTokens "0sand_storm" `shouldBe` ([E.LexerError {E.pos = (1,1), E.errorMessage = "Unexpected element: 0sand_storm"}],[])

        it "tokenize valid variable declaration & ids" $ do
            L.scanTokens "bender aang of air" `shouldBe` ([],[TK.TKbender {TK.pos = (1,1)},TK.TKid {TK.pos = (1,8), TK.id = "aang"},TK.TKof {TK.pos = (1,13)},TK.TKair {TK.pos = (1,16)}])
            L.scanTokens "eternal bender aang of air" `shouldBe` ([],[TK.TKeternal {TK.pos = (1,1)},TK.TKbender {TK.pos = (1,9)},TK.TKid {TK.pos = (1,16), TK.id = "aang"},TK.TKof {TK.pos = (1,21)},TK.TKair {TK.pos = (1,24)}])
            L.scanTokens "sand_storm" `shouldBe` ([],[TK.TKid {TK.pos = (1,1), TK.id = "sand_storm"}])

        it "tokenize valid types" $ do
            L.scanTokens "earth fire water air" `shouldBe` ([],[TK.TKearth {TK.pos = (1,1)},TK.TKfire {TK.pos = (1,7)},TK.TKwater {TK.pos = (1,12)},TK.TKair {TK.pos = (1,18)}])

        it "parses multi word token, no matter how many white spaces in between" $ do
            L.scanTokens "is mastered by" `shouldBe` ([],[TK.TKmasteredBy {TK.pos = (1,1)}])
            L.scanTokens "is     mastered by" `shouldBe` ([],[TK.TKmasteredBy {TK.pos = (1,1)}])
            L.scanTokens "is     mastered     by" `shouldBe` ([],[TK.TKmasteredBy {TK.pos = (1,1)}])
            L.scanTokens "is mastered     by" `shouldBe` ([],[TK.TKmasteredBy {TK.pos = (1,1)}])