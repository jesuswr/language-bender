module FrontEnd.StaticAnalysis where


-- <Language Bender Imports> ------------------------------------
import qualified FrontEnd.StaticErrors  as SE
import qualified FrontEnd.AST           as AST
import qualified FrontEnd.SymTable      as ST
import qualified FrontEnd.Utils         as U

-- <Utility Data types> -----------------------------------------
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad     as M
import Data.Maybe(isNothing, maybe, fromMaybe)

-----------------------------------------------------------------

type ErrorLog = [SE.StaticError]

-- | State used to simulate an imperative process of type checking 
type AnalyzerState = RWS.RWST () ErrorLog AnalysisState IO

-- | State of current analysis 
data AnalysisState = State {
    symTable :: ST.SymTable,
    ast :: AST.Program
}

-- | Add error to state of RWST
addStaticError :: SE.StaticError -> AnalyzerState ()
addStaticError e = RWS.tell [e]

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
            case t of Just t' -> checkType t'

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
                Nothing -> addStaticError . SE.SymbolNotInScope $ refId
                Just ST.Symbol{ST.symType = ST.Variable{}} -> return ()
                _       -> addStaticError . SE.ReferencingNonVariable $ refId

            -- Get reference type:
            let refType = case refSym of
                    Just ST.Symbol{ST.symType = ST.Variable{ST.varType=t}} -> t
                    Nothing -> Nothing

                newType = ST.Reference refId refType
            -- create new symbol 
                newSym = ST.Symbol {ST.identifier=sid, ST.symType=newType, ST.scope=0, ST.enrtyType=Nothing}

            -- try to add symbol 
            tryAddSymbol newSym

        -- Check union definition 
        checkDecls AST.Union {AST.decName=_decName, AST.fields=_fields} = do

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

            -- Create new symbol 
            --  Check that all types are valid 
            M.forM_ (map snd _fields) checkType

            --  Create symbol type
            let symType = ST.StructType {ST.fields=_fields}
            --  Create Symbol itself 
                symbol = ST.Symbol { ST.identifier=_decName, ST.symType=symType, ST.scope=0, ST.enrtyType=Nothing }

            -- check if add symbol is possible 
            tryAddSymbol symbol

        -- Check function declaration
        checkDecls AST.Func {AST.decName=_decName, AST.args=_args, AST.retType=_retType, AST.body=_body} = do

            -- check valid return type if needed 
            case _retType of Just t -> checkType t

            -- Function to check a single function argument 
            let checkFArg :: AST.FuncArg -> AnalyzerState ()
                checkFArg AST.FuncArg {AST.argType=_argType, AST.defaultVal=_defaultVal} = do
                                checkType _argType -- check argument type 
                                case _defaultVal of Just expr -> checkExpr expr -- check expression validity

            -- check arguments
            M.forM_ _args checkFArg

            -- Create a new symbol for this function 
            let symbol  = ST.Symbol {
                                ST.identifier=_decName,
                                ST.symType=
                                    ST.Function {
                                        ST.args=_args,
                                        ST.retType=fromMaybe AST.TUnit _retType,
                                        ST.body=_body },
                                ST.scope=0,
                                ST.enrtyType=Nothing
                            }

            -- try to add function symbol 
            tryAddSymbol symbol

            -- Push an empty scope 
            pushEmptyScope  -- argument scope

            -- try add arguments as variables
            let variables = [
                        ST.Symbol {
                            ST.identifier=_argName,
                            ST.symType= ST.Variable{ST.varType= Just _argType, ST.initVal=_defaultVal} ,
                            ST.scope=0,
                            ST.enrtyType=Nothing
                        }
                        | (AST.FuncArg _argName _argType _defaultVal) <- _args
                    ]

            M.forM_ variables tryAddSymbol

            -- push body scope, and check body 
            pushEmptyScope  -- body scope

            checkExpr _body

            popEmptyScope   -- body scope

            popEmptyScope   -- argument scope

        -- | Check if a given expression uses valid names only
        checkExpr :: AST.Expr -> AnalyzerState ()

        -- Check Id expression:
        checkExpr AST.Id {AST.name=_name, AST.position=_position} = checkIdIsVarOrReference _name
            
        -- Check assign expression 
        checkExpr AST.Assign {AST.variable=_variable, AST.value=_value} = do
            -- check if left hand corresponds to a variable or reference name
            checkIdIsVarOrReference _variable
            -- Check right side 
            checkExpr _value

        -- Check assign to struct
        checkExpr AST.StructAssign {AST.struct =_struct, AST.value=_value} = do
            -- check that given symbol is valid expression  
            checkExpr _struct
            -- check right hand value
            checkExpr _value

        -- Check struct access
        checkExpr AST.StructAccess {AST.struct =_struct} = do
            checkExpr _struct 

        -- Check Function Call
        checkExpr AST.FunCall {AST.fname=_fname, AST.actualArgs=_actualArgs} = do
            
            -- check symbol definition 
            mbSym <- checkSymbolDefined _fname

            --case mbSym of 
            --    Nothing 
            -- @TODO todavia hay que terminar de escribir el checkeo de funciones
            undefined


        checkExpr _ = undefined

-- | Checks if a given type is a valid one 
checkType :: AST.Type -> AnalyzerState ()
checkType AST.CustomType {AST.tName=_tName} = do -- When it is a custom type
    st@State{symTable=symTb} <- RWS.get

    -- try to find symbol
    let symbol =  ST.findSymbol _tName symTb

    -- Check if symbol was correct 
    case symbol of
        Nothing -> addStaticError $ SE.SymbolNotInScope { SE.symName=_tName}
        Just ST.Symbol { ST.symType= ST.Type{} } -> return ()
        _  -> addStaticError $ SE.NotValidType{SE.nonTypeName = _tName}

checkType ptr = checkType . AST.ptrType $ ptr

-- | Try add symbol. If possible, add it, otherwise write proper errors
tryAddSymbol :: ST.Symbol -> AnalyzerState ()
tryAddSymbol s@ST.Symbol {ST.identifier=_identifier} = do
    -- get current state
    currSt@State{symTable=st} <- RWS.get
    case ST.insertSymbol s st of
        Nothing -> addStaticError $ SE.SymbolRedefinition _identifier -- if could not add, it's because of symbol redefinition
        Just st' -> RWS.put currSt{symTable = st'}                   -- update state

-- | Utility function to push an empty scope, updating state
pushEmptyScope :: AnalyzerState ()
pushEmptyScope = do
    st@State{symTable = symTb} <- RWS.get

    let symTb' = ST.pushEmptyScope symTb

    RWS.put st{symTable=symTb'}

-- | Utility function to pop an empty scope, possibly changing current state
popEmptyScope :: AnalyzerState ()
popEmptyScope = do
    st@State{symTable = symTb} <- RWS.get

    let symTb' = ST.popCurrentScope  symTb

    RWS.put st{symTable = symTb'}

-- | Utility function to check if a given name corresponds to a valid symbol, and return it if exists
checkSymbolDefined :: U.Name -> AnalyzerState (Maybe ST.Symbol)
checkSymbolDefined name = do
    st@State{symTable = symTb} <- RWS.get

    -- Try find symbol 
    case ST.findSymbol name symTb of
        Nothing  -> addStaticError SE.SymbolNotInScope {SE.symName=name} >> return Nothing
        Just sym -> return . Just $ sym

checkIdIsVarOrReference :: U.Name -> AnalyzerState ()
checkIdIsVarOrReference name = do -- Check that given name is a valid one and it is a variable
            mbSym <- checkSymbolDefined name

            -- Raise invalid symbol if this symbol is not a variable nor a reference
            case mbSym of
                Just sym -> M.unless ( ST.isVariable  sym || ST.isReference  sym) $ 
                            addStaticError SE.NotAValidVariable {SE.symName=name, SE.actualSymType=ST.symType sym}
                Nothing  -> addStaticError SE.SymbolNotInScope {SE.symName=name}
