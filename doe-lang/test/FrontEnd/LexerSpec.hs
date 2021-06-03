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

        it "parses multiline strings ignoring line breaks" $ do
            L.scanTokens "\"hola\nchao\"" `shouldBe` ([],[TK.TKstring {TK.pos = (1,1), TK.str = "holachao"}])
            L.scanTokens "\"\\\"air\\\"\\n\\t\\\"water\\\"\\n\\t\\t\\\"earth\\\"\\n\\t\\t\\t\\\"fire\\\"\\n\\0\"" `shouldBe` ([],[TK.TKstring {TK.pos = (1,1), TK.str = "\"air\"\n\t\"water\"\n\t\t\"earth\"\n\t\t\t\"fire\"\n\NUL"}])

        it "" $ do
            L.scanTokens ",~,,:,).;.(,.--.~.~;,-.;.(,;-.~)())(" `shouldBe` ([E.LexerError {E.pos = (1,9), E.errorMessage = "Unexpected element: ;"},E.LexerError {E.pos = (1,20), E.errorMessage = "Unexpected element: ;"},E.LexerError {E.pos = (1,24), E.errorMessage = "Unexpected element: ;"},E.LexerError {E.pos = (1,28), E.errorMessage = "Unexpected element: ;"}],[TK.TKcomma {TK.pos = (1,1)},TK.TKunit {TK.pos = (1,2)},TK.TKcomma {TK.pos = (1,3)},TK.TKcomma {TK.pos = (1,4)},TK.TKcolon {TK.pos = (1,5)},TK.TKcomma {TK.pos = (1,6)},TK.TKcloseParent {TK.pos = (1,7)},TK.TKdot {TK.pos = (1,8)},TK.TKdot {TK.pos = (1,10)},TK.TKopenParent {TK.pos = (1,11)},TK.TKcomma {TK.pos = (1,12)},TK.TKbeginBlock {TK.pos = (1,13)},TK.TKendBlock {TK.pos = (1,15)},TK.TKunit {TK.pos = (1,17)},TK.TKdot {TK.pos = (1,18)},TK.TKunit {TK.pos = (1,19)},TK.TKcomma {TK.pos = (1,21)},TK.TKendBlock {TK.pos = (1,22)},TK.TKdot {TK.pos = (1,25)},TK.TKopenParent {TK.pos = (1,26)},TK.TKcomma {TK.pos = (1,27)},TK.TKendBlock {TK.pos = (1,29)},TK.TKunit {TK.pos = (1,31)},TK.TKcloseParent {TK.pos = (1,32)},TK.TKopenParent {TK.pos = (1,33)},TK.TKcloseParent {TK.pos = (1,34)},TK.TKcloseParent {TK.pos = (1,35)},TK.TKopenParent {TK.pos = (1,36)}])
