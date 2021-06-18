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

type ErrorLog = [E.Error]

-- | State used to simulate an imperative process of type checking 
type AnalyzerState = RWS.RWST () ErrorLog AnalysisState IO

-- | State of current analysis 
data AnalysisState = State {
    symTable :: ST.SymTable,
    ast :: AST.Program 
}

-- | Add error to state of RWST
addError :: E.Error -> AnalyzerState ()
addError e = RWS.tell [e]

addStaticError :: E.StaticError -> AnalyzerState ()
addStaticError = addError . E.StaticError  

-- | Function to check if every name used is in the current scope
namesAnalysis :: AST.Program -> AnalyzerState ()
namesAnalysis p@AST.Program{AST.decls=ds} = do

    M.forM_ ds checkDecls


    where
        -- | Add declarations to symbol table and check if they´re correct
        checkDecls :: AST.Declaration -> AnalyzerState ()

        -- Check Variable Declaration 
        checkDecls AST.Variable{ AST.decName = sid, AST.varType =  t, AST.initVal = ival, AST.isConst = const} = do
            -- Get current state
            currSt@State{symTable = st} <- RWS.get

            -- check if type of variable is currently defined when it's customType 
            case t of
                Just (AST.CustomType id) -> do
                    case ST.findSymbol id st of
                        Nothing ->  addError . E.StaticError . E.SymbolNotInScope $ id


            -- Create a new symbol
            let symType = ST.Variable{ ST.varType = t, ST.initVal = ival}
                newSym = ST.Symbol{ ST.identifier = sid, ST.enrtyType = Nothing, ST.symType = symType, ST.scope = 0}

            -- Check expression if necessary
            M.forM_ ival checkExpr

            -- Add new variable to symbol table 
            tryAddSymbol newSym

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
            tryAddSymbol newSym

            return ()
            -- Add current symbol definition to ast

        -- Check union definition 
        checkDecls AST.Union {AST.decName=_decName, AST.fields=_fields} = do
            -- Get current state:
            st@State{symTable=symTb} <- RWS.get

            -- Create new symbol 
            --  Check that all types are valid 
            M.forM_ (map snd _fields) checkType

            --  Create symbol type
            let symType = ST.UnionType {ST.fields=_fields}
            --  Create Symbol itself 
                symbol = ST.Symbol { ST.identifier=_decName, ST.symType=symType, ST.scope=0, ST.enrtyType=Nothing }

            -- check if add symbol is possible 
            tryAddSymbol symbol
            
        -- Check Struct definition 
        checkDecls AST.Struct {AST.decName=_decName, AST.fields=_fields} = do
            -- Get current state:
            st@State{symTable=symTb} <- RWS.get

            -- Create new symbol 
            --  Check that all types are valid 
            M.forM_ (map snd _fields) checkType

            --  Create symbol type
            let symType = ST.StructType {ST.fields=_fields}
            --  Create Symbol itself 
                symbol = ST.Symbol { ST.identifier=_decName, ST.symType=symType, ST.scope=0, ST.enrtyType=Nothing }

            -- check if add symbol is possible 
            tryAddSymbol symbol

        checkExpr :: AST.Expr -> AnalyzerState ()
        checkExpr _ = undefined

-- | Checks if a given type is a valid one 
checkType :: AST.Type -> AnalyzerState ()
checkType AST.CustomType {AST.tName=_tName} = do
    st@State{symTable=symTb} <- RWS.get 

    -- try to find symbol
    let symbol =  ST.findSymbol _tName symTb 

    -- Check if symbol was correct 
    case symbol of 
        Nothing -> addStaticError $ E.SymbolNotInScope { E.symName=_tName}
        Just ST.Symbol { ST.symType= ST.Type{} } -> return ()
        _  -> addStaticError $ E.NotValidType{E.nonTypeName = _tName}   

    return ()

-- | Try add symbol. If possible, add it, otherwise write proper errors
tryAddSymbol :: ST.Symbol -> AnalyzerState ()
tryAddSymbol s@ST.Symbol {ST.identifier=_identifier} = do
    -- get current state
    currSt@State{symTable=st} <- RWS.get 
    case ST.insertSymbol s st of 
        Nothing -> addStaticError $ E.SymbolRedefinition _identifier -- if could not add, it's because of symbol redefinition
        Just st' -> RWS.put currSt{symTable = st'}                   -- update state