module FrontEnd.LexerSpec (spec) where

import Test.Hspec
import qualified FrontEnd.Lexer as L


atlaIntro = "Water. Earth. Fire. Air. Long ago, the four nations lived together in harmony. \
			\Then, everything changed when the Fire Nation attacked. \
			\Only the Avatar, master of all four elements, could stop them"


spec :: Spec
spec = do 
    describe "scanTokens" $ do
        it "tokenize string and parse errors if finds some" $ do
            L.scanTokens "-- asdaa" `shouldBe` ([], []) 




        -- neil tests

        it "Ignore ATLA part of the intro" $ do
            L.scanTokens atlaIntro `shouldBe` ([], []) 