module FrontEnd.LexerSpec (spec) where

import Test.Hspec
import qualified FrontEnd.Lexer as L
import qualified FrontEnd.Tokens as TK
import qualified FrontEnd.Errors as E

import FrontEnd.Utils

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
            L.scanTokens "0sand_storm" `shouldBe` ([E.LexerError {E.pos = Position 1 1, E.error = E.InvalidToken "0sand_storm"}],[])

        it "tokenize valid variable declaration & ids" $ do
            L.scanTokens "bender aang of air" `shouldBe` ([],[TK.Token { TK.pos = Position 1 1, TK.tktype = TK.TKbender}, TK.Token { TK.pos = Position 1 8, TK.tktype = TK.TKid { TK.id = "aang"}}, TK.Token { TK.pos = Position 1 13, TK.tktype = TK.TKof}, TK.Token { TK.pos = Position 1 16, TK.tktype = TK.TKair}])
            L.scanTokens "eternal bender aang of air" `shouldBe` ([],[TK.Token { TK.pos = Position 1 1, TK.tktype = TK.TKeternal} ,TK.Token { TK.pos = Position 1 9, TK.tktype = TK.TKbender} ,TK.Token { TK.pos = Position 1 16, TK.tktype = TK.TKid { TK.id = "aang"}} ,TK.Token { TK.pos = Position 1 21, TK.tktype = TK.TKof} ,TK.Token { TK.pos = Position 1 24, TK.tktype = TK.TKair}])
            L.scanTokens "sand_storm" `shouldBe` ([], [TK.Token { TK.pos = Position 1 1, TK.tktype = TK.TKid { TK.id = "sand_storm" }}])

        it "tokenize valid types" $ do
            L.scanTokens "earth fire water air" `shouldBe` ([],[TK.Token { TK.pos = Position 1 1, TK.tktype = TK.TKearth} ,TK.Token { TK.pos = Position 1 7, TK.tktype = TK.TKfire} ,TK.Token { TK.pos = Position 1 12, TK.tktype = TK.TKwater} ,TK.Token { TK.pos = Position 1 18, TK.tktype = TK.TKair}])

        it "parses multi word token, no matter how many white spaces in between" $ do
            L.scanTokens "is mastered by" `shouldBe` ([],[TK.Token { TK.pos = Position 1 1, TK.tktype = TK.TKmasteredBy}])
            L.scanTokens "is     mastered by" `shouldBe` ([],[TK.Token { TK.pos = Position 1 1, TK.tktype = TK.TKmasteredBy}])
            L.scanTokens "is     mastered     by" `shouldBe` ([],[TK.Token { TK.pos = Position 1 1, TK.tktype = TK.TKmasteredBy}])
            L.scanTokens "is mastered     by" `shouldBe` ([],[TK.Token { TK.pos = Position 1 1, TK.tktype = TK.TKmasteredBy}])

        it "parses multiline strings ignoring line breaks" $ do
            L.scanTokens "\"hola\nchao\"" `shouldBe` ([],[TK.Token { TK.pos = Position 1 1, TK.tktype = TK.TKstring { TK.str = "holachao"} }])
            L.scanTokens "\"\\\"air\\\"\\n\\t\\\"water\\\"\\n\\t\\t\\\"earth\\\"\\n\\t\\t\\t\\\"fire\\\"\\n\\0\"" `shouldBe` ([],[TK.Token { TK.pos = Position 1 1, TK.tktype = TK.TKstring { TK.str = "\"air\"\n\t\"water\"\n\t\t\"earth\"\n\t\t\t\"fire\"\n\NUL"}}])

        it "reports errors when using unrecognized symbols" $ do
            L.scanTokens ",~,,:,).;.(,.--.~.~;,-.;.(,;-.~)())(" `shouldBe` ([E.LexerError {E.pos = Position 1 9, E.error = E.InvalidToken ";"},E.LexerError {E.pos = Position 1 20, E.error = E.InvalidToken ";"},E.LexerError {E.pos = Position 1 24, E.error = E.InvalidToken ";"},E.LexerError {E.pos = Position 1 28, E.error = E.InvalidToken ";"}],[TK.Token { TK.pos = Position 1 1, TK.tktype = TK.TKcomma} ,TK.Token { TK.pos = Position 1 2, TK.tktype = TK.TKunit} ,TK.Token { TK.pos = Position 1 3, TK.tktype = TK.TKcomma} ,TK.Token { TK.pos = Position 1 4, TK.tktype = TK.TKcomma} ,TK.Token { TK.pos = Position 1 5, TK.tktype = TK.TKcolon} ,TK.Token { TK.pos = Position 1 6, TK.tktype = TK.TKcomma} ,TK.Token { TK.pos = Position 1 7, TK.tktype = TK.TKcloseParent} ,TK.Token { TK.pos = Position 1 8, TK.tktype = TK.TKdot} ,TK.Token { TK.pos = Position 1 10, TK.tktype = TK.TKdot} ,TK.Token { TK.pos = Position 1 11, TK.tktype = TK.TKopenParent} ,TK.Token { TK.pos = Position 1 12, TK.tktype = TK.TKcomma} ,TK.Token { TK.pos = Position 1 13, TK.tktype = TK.TKbeginBlock} ,TK.Token { TK.pos = Position 1 15, TK.tktype = TK.TKendBlock} ,TK.Token { TK.pos = Position 1 17, TK.tktype = TK.TKunit} ,TK.Token { TK.pos = Position 1 18, TK.tktype = TK.TKdot} ,TK.Token { TK.pos = Position 1 19, TK.tktype = TK.TKunit} ,TK.Token { TK.pos = Position 1 21, TK.tktype = TK.TKcomma} ,TK.Token { TK.pos = Position 1 22, TK.tktype = TK.TKendBlock} ,TK.Token { TK.pos = Position 1 25, TK.tktype = TK.TKdot} ,TK.Token { TK.pos = Position 1 26, TK.tktype = TK.TKopenParent} ,TK.Token { TK.pos = Position 1 27, TK.tktype = TK.TKcomma} ,TK.Token { TK.pos = Position 1 29, TK.tktype = TK.TKendBlock} ,TK.Token { TK.pos = Position 1 31, TK.tktype = TK.TKunit} ,TK.Token { TK.pos = Position 1 32, TK.tktype = TK.TKcloseParent} ,TK.Token { TK.pos = Position 1 33, TK.tktype = TK.TKopenParent} ,TK.Token { TK.pos = Position 1 34, TK.tktype = TK.TKcloseParent} ,TK.Token { TK.pos = Position 1 35, TK.tktype = TK.TKcloseParent} ,TK.Token { TK.pos = Position 1 36, TK.tktype = TK.TKopenParent}])
            L.scanTokens "\"🤡\"" `shouldBe` ([E.LexerError {E.pos = Position 1 2, E.error = E.InvalidStrChar}],[TK.Token { TK.pos = Position 1 1, TK.tktype = TK.TKstring { TK.str = ""}}])

        it "parses arbitrary numbers" $ do
            L.scanTokens "10.0" `shouldBe` ([],[TK.Token { TK.pos = Position 1 1, TK.tktype = TK.TKfloat { TK.numF = 10.0}}])
            L.scanTokens "10" `shouldBe` ([],[TK.Token { TK.pos = Position 1 1, TK.tktype = TK.TKint { TK.numI = 10}}])