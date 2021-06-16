module FrontEnd.StaticAnalysis where


-- <Language Bender Imports> ------------------------------------
import qualified FrontEnd.Errors     as E
import qualified FrontEnd.AST        as AST
import qualified FrontEnd.SymTable   as ST

-- <Utility Data types> -----------------------------------------
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad     as M
import Data.Maybe(isNothing)
-----------------------------------------------------------------

-- | State used to simulate an imperative process of type checking 
type LangBenderState a = RWS.RWST () (IO ()) AnalysisState a

-- | State of current analysis 
data AnalysisState = State {
    errors :: [E.Error],
    symTable :: ST.SymTable
}

addError :: E.Error -> AnalysisState -> AnalysisState
addError e st@State{errors = es} =  st{errors = e:es}

-- | Function to check if every name used is in the current scope
namesAnalysis :: AST.Program -> LangBenderState IO ()
namesAnalysis p@AST.Program{AST.decls=ds} = do

    M.forM_ ds checkDecls


    where
        -- | Add declarations to symbol table and check if they´re correct
        checkDecls :: AST.Declaration -> LangBenderState IO ()

        -- Check Variable Declaration 
        checkDecls AST.Variable{ AST.decName = sid, AST.varType =  t, AST.initVal = ival, AST.isConst = const} = do
            -- Get current state
            currSt@State{symTable = st} <- RWS.get

            -- check if type of variable is currently defined when it's customType 
            case t of
                Just (AST.CustomType id) -> do
                    case ST.findSymbol id st of
                        Nothing -> addError . E.StaticError . E.SymbolNotInScope $ id


            -- Create a new symbol
            let symType = ST.Variable{ ST.varType = t, ST.initVal = ival}
                newSym = ST.Symbol{ ST.identifier = sid, ST.enrtyType = Nothing, ST.symType = symType, ST.scope = 0}

            -- Check expression if necessary
            M.forM_ ival checkExpr

            -- Add new variable to symbol table 
            let mbSt = ST.insertSymbol newSym st

            -- Check if could add variable:
            case mbSt of
                Nothing -> addError . E.StaticError . E.SymbolRedefinition $ sid
                Just st -> RWS.put currSt{symTable=st}

        -- Check reference Declaration 
        checkDecls AST.Reference{ AST.decName=sid, AST.refName = refId } = do
            -- Get current state
            currSt@State{symTable = st} <- RWS.get

            let refSym = ST.findSymbol refId  st

            -- Check if referenced symbol exists and it's a variable
            case refSym of
                Nothing -> addError . E.StaticError . E.SymbolNotInScope $ refId
                Just ST.Symbol{ST.symType = ST.Variable{}} -> return ()
                _       -> addError . E.StaticError . E.ReferencingNonVariable $ refId

            -- Get reference type:
            let refType = case refSym of 
                    Just ST.Symbol{ST.symType = ST.Variable{ST.varType=t}} -> t
                    Nothing -> Nothing 

                newType = ST.Reference refId refType
            -- create new symbol 
                newSym = ST.Symbol {ST.identifier=sid, ST.symType=newType, ST.scope=0, ST.enrtyType=Nothing}

            -- try to add symbol 
            case ST.insertSymbol newSym st of 
                Nothing  -> addError . E.StaticError . E.SymbolRedefinition $ sid
                Just st' -> RWS.put currSt{symTable=st'}

            return ()
            -- Add current symbol definition to ast



        checkExpr :: AST.Expr -> LangBenderState IO ()
        checkExpr _ = undefined

        addError :: E.Error -> LangBenderState IO ()
        addError e = do
            st@State{errors = es} <- RWS.get
            RWS.put st{errors = e:es}

