{-
    Standard library implementation for built-in functions 
-}

module BackEnd.StdLib where

-- External imports
import qualified TACTypes.TAC as TAC
import qualified Data.Map as M
import Utils.Types as UT

-- | Tac code for print function, pass the size to stack for this function in bytes, and its label name
stdPrint ::  Int -> String -> String -> [TAC.TACCode]
stdPrint size labelName nextTemporalName = 
    let 
        tacSize = TAC.Constant . TAC.Int $ size
    in
        [ 
            TAC.newTAC TAC.MetaLabel (TAC.Label labelName) [],
            TAC.newTAC TAC.MetaBeginFunc tacSize [],
            TAC.newTAC TAC.RDeref  (TAC.Id nextTemporalName) [TAC.Id TAC.base, TAC.Constant . TAC.Int $ 0],
            TAC.newTAC TAC.Print (TAC.Id nextTemporalName) [],
            TAC.newTAC TAC.MetaEndFunc  tacSize []
        ]