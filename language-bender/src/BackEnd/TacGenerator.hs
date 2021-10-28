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
                    { nextTemporal :: Int
                    -- , symTable :: ST.SymTable
                    }

-- | Monad used to keep a state when traversing the AST to generate the code
type GeneratorMonad = RWS.RWST () TAC.TACProgram GeneratorState IO


-- >> Handling Monad --------------------------------------------

-- | get next temporal variable
getNextTemp :: GeneratorMonad String
getNextTemp = do
    State{nextTemporal = n} <- RWS.get
    RWS.put State{nextTemporal = (n+1)}
    return $ "T" ++ (show n) 

-- | write TAC instruccion
writeTac :: TAC.TACCode -> GeneratorMonad ()
writeTac tacInst = do
    RWS.tell [tacInst]

-- | Initial Generator State
initialGenState :: GeneratorState
initialGenState = State{nextTemporal = 0}

-- >> Auxiliar Functions ----------------------------------------

-- | generate an unique tac id from a language bender id
getTacId :: String -> Int -> String
getTacId id scope = id ++ "@" ++ (show scope)

-- >> Main Function ---------------------------------------------

-- | Generate a Tac program using the symbol table and the AST object, returning the 
-- | resulting state and the generated program
generateTac :: ST.SymTable  -> AST.Program  -> IO (GeneratorState, TAC.TACProgram)
generateTac symbolTable program =
    RWS.execRWST (generateTac' symbolTable program) () initialGenState

-- | Utility function to generate the actual Tac Program.
generateTac' :: ST.SymTable  -> AST.Program  -> GeneratorMonad ()
generateTac' symT program = do
    hasMain <- findMain symT
    --M.when hasMain $ writeTac (TACCode Goto (Just (Label "main@0")) Nothing Nothing) -- FIX
    M.when hasMain $ writeTac (TAC.TACCode TAC.Goto Nothing Nothing Nothing) -- FIX
    genTacDecls $ AST.decls program

findMain :: ST.SymTable -> GeneratorMonad Bool
findMain symT = do
    -- busca en la st si hay un id "main" (por ahora) que sea un procedimiento
    let foundSym = ST.findSymbolInScope "main" 0 symT

    case foundSym of
        Just ST.Symbol { ST.symType = ST.Function{} } -> return True
        _                             -> return False

-- | iterate over declarations in program
genTacDecls :: [AST.Declaration] -> GeneratorMonad ()
genTacDecls []     = return ()
genTacDecls (d:ds) = do
    genTacDecl d
    genTacDecls ds

--------------------------------------------------------
-- | generate tac for declarations ---------------------
genTacDecl :: AST.Declaration -> GeneratorMonad ()

-- | generate tac for variable decl
genTacDecl AST.Variable{} = undefined

-- | gen tac for references decl
genTacDecl AST.Reference{} = undefined

-- | gen tac for unions decl
genTacDecl AST.Union{} = undefined

-- | gen tac for structs decl
genTacDecl AST.Struct{} = undefined

-- | gen tac for functions and procedure decls
genTacDecl AST.Func{} = undefined

-- | gen tac for func args


--------------------------------------------------------

-- | generate tac for Expressions ----------------------

-- | gen tac for literal char

-- | gen tac for literal string

-- | gen tac for literal Int

-- | gen tac for literal Float

-- | gen tac for literal True

-- | gen tac for literal False

-- | gen tac for literal struct

-- | gen tac for literal union

-- | gen tac for literal unit ??

-- | gen tac for literal null

-- | gen tac for Id

-- | gen tac for Assignment

-- | gen tac for struct assignment

-- | gen tac for struct access

-- | gen tac for function call

-- | gen tac for For loop

-- | gen tac for While loop

-- | gen tac for If

-- | gen tac for Expressions Block

-- | gen tac for return

-- | gen tac for Break

-- | gen tac for Continue

-- | gen tac for Declaration 

-- | gen tac for Op2

-- | gen tac for Op1

-- | gen tac for literal Array

-- | gen tac for Union trying

-- | gen tac for Union using

-- | gen tac for new

-- | gen tac for delete

-- | gen tac for Array indexing
