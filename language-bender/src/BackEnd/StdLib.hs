{-
    Standard library implementation for built-in functions 
-}

module BackEnd.StdLib where

-- External imports
import qualified TACTypes.TAC as TAC


-- | Tac code for print function, pass the size to stack for this function in bytes
stdPrint ::  Int -> [TAC.TACCode]
stdPrint size = undefined 