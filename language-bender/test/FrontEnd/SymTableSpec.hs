module FrontEnd.SymTableSpec (spec) where

import Test.Hspec
import FrontEnd.SymTable 


spec = do
    describe "pushEmptyScope" $ do
        it "empiles a new scope" $ do
            let t = newTable
                expected = t{ stScopeStk = [1,0], stNextScope = 2}

            pushEmptyScope newTable `shouldBe` expected

    describe "popEmptyScope" $ do
        it "removes current scope" $ do
            let expected = newTable{ stNextScope = 2 }

            popCurrentScope (pushEmptyScope newTable) `shouldBe` expected

    describe "findSymbol" & do
        it "finds symbol by name when in scope" &

    