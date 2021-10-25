{-
    The following file contains all the functions to convert the AST data type into a TAC
    code. The TAC data types can be found in src/BackEnd/TACTypes/TAC.hs
-}
module BackEnd.TacGenerator where

-- <Language Bender Imports> ------------------------------------
import qualified FrontEnd.AST as AST
import qualified FrontEnd.SymTable as ST
import qualified TACTypes.TAC as TAC

-- <Utility Data types> -----------------------------------------
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad     as M

-- >> Data ------------------------------------------------------

-- | Stateful information required by the conversion process
data GeneratorState = State
                    { symTable :: ST.SymTable
                    , program :: AST.Program 
                    }

-- | Monad used to keep a state when traversing the AST to generate the code
type GeneratorMonad = RWS.RWST () TAC.TACProgram GeneratorState IO

-- >> Main Function ---------------------------------------------

-- | Generate a Tac program using the symbol table and the AST object, returning the 
-- | resulting state and the generated program
generateTac :: ST.SymTable  -> AST.Program  -> IO (GeneratorState, TAC.TACProgram)
generateTac symbolTable program = do
    (_, s, e) <- RWS.runRWST generateTac' () State{symTable=symbolTable, program=program}
    return (s, e)

-- | Utility function to generate the actual Tac Program.
generateTac' :: GeneratorMonad ()
generateTac' = error "not yet implemented"