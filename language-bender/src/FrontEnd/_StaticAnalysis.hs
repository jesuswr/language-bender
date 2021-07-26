module FrontEnd.StaticAnalysis where


-- <Language Bender Imports> ------------------------------------
import qualified FrontEnd.StaticErrors  as SE
import qualified FrontEnd.AST           as AST
import qualified FrontEnd.SymTable      as ST
import qualified FrontEnd.Utils         as U
import qualified FrontEnd.Parser        as P

-- <Utility Data types> -----------------------------------------
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad     as M
import Data.Functor((<&>))
import Data.Maybe(isNothing, maybe, fromMaybe, isJust, fromJust)
-----------------------------------------------------------------

type ErrorLog = [SE.StaticError]

-- | State used to simulate an imperative process of type checking 
type AnalyzerState = RWS.RWST () ErrorLog AnalysisState IO

-- | State of current analysis 
data AnalysisState = State {
    symTable :: ST.SymTable,
    ast :: AST.Program
}

startingState :: AST.Program  -> AnalysisState
startingState = State ST.newTable

analyzeProgram :: AST.Program -> IO (AnalysisState, ErrorLog)
analyzeProgram p = do
    (_, s, e) <- RWS.runRWST (namesAnalysis p) () (startingState p)
    return (s, e)

-- | Add error to state of RWST
addStaticError :: SE.StaticError -> AnalyzerState ()
addStaticError e = RWS.tell [e]

-- | Function to check if every name used is in the current scope
namesAnalysis :: AST.Program -> AnalyzerState ()
namesAnalysis p@AST.Program{AST.decls=ds} = M.forM_ ds checkDecls


-- | Add declarations to symbol table and check if they´re correct
checkDecls :: AST.Declaration -> AnalyzerState AST.Declaration

-- Check Variable Declaration 
checkDecls v@AST.Variable{ AST.decName = sid, AST.varType =  t, AST.initVal = ival, AST.isConst = const} = do

    -- Get current state
    currSt@State{symTable = st} <- RWS.get

    -- check if type of variable is currently defined when it's customType 
    --t checkType

    -- Create a new symbol
    let newSym = declToSym v

    -- Check expression if necessary
    M.forM_ ival checkExpr

    -- Add new variable to symbol table 
    tryAddSymbol newSym

    return v

-- Check reference Declaration 
checkDecls r@AST.Reference{ AST.decName=sid, AST.refName = refId } = do
    -- Get current state
    currSt@State{symTable = st} <- RWS.get

    let refSym = ST.findSymbol refId  st

    -- Check if referenced symbol exists and it's a variable
    case refSym of
        Nothing -> addStaticError . SE.SymbolNotInScope $ refId
        Just ST.Symbol{ST.symType = ST.Variable{}} -> return ()
        Just ST.Symbol{ST.symType = ST.Reference{}} -> return ()
        _       -> addStaticError . SE.ReferencingNonVariable $ sid refId

    -- Get reference type:
    let refType = case refSym of
            Just ST.Symbol{ST.symType = ST.Variable{ST.varType=t}} -> t
            _ -> AST.TypeError 

        newType = ST.Reference refId refType
    -- create new symbol 
        newSym = ST.Symbol {ST.identifier=sid, ST.symType=newType, ST.scope=0, ST.enrtyType=Nothing}

    -- try to add symbol 
    tryAddSymbol newSym

    return r

-- Check union definition 
checkDecls u@AST.Union {AST.decName=_decName, AST.fields=_fields} = do

    -- Create new symbol 
    --  Check that all types are valid 
    -- M.forM_ (map snd _fields) checkType

    --  Create symbol type
    let symbol = declToSym u

    -- Check if fields are duplicated
    M.when (U.hasDuplicates . map fst $ _fields) $ addStaticError SE.DuplicateNamesInCompound{SE.symName=_decName}

    -- check if add symbol is possible 
    tryAddSymbol symbol

    return u

-- Check Struct definition 
checkDecls s@AST.Struct {AST.decName=_decName, AST.fields=_fields} = do

    -- Create new symbol 
    --  Check that all types are valid 
    M.forM_ (map snd _fields) checkType

    --  Create symbol 
    let symbol = declToSym s

    -- Check for duplicate fields
    M.when (U.hasDuplicates $ map fst _fields)  $ addStaticError SE.DuplicateNamesInCompound{SE.symName=_decName}

    -- check if add symbol is possible 
    tryAddSymbol symbol

    return s

-- Check function declaration
checkDecls f@AST.Func {AST.decName=_decName, AST.args=_args, AST.retType=_retType, AST.body=_body} = do

    --case _retType of Just t -> checkType t

    -- Function to check a single function argument 
    let checkFArg :: AST.FuncArg -> AnalyzerState ()
        checkFArg AST.FuncArg {AST.argType=_argType, AST.defaultVal=_defaultVal} = do
                        checkType _argType -- check argument type 
                        M.forM_ _defaultVal checkExpr -- check expression validity

    -- check arguments
    M.forM_ _args checkFArg

    -- Create a new symbol for this function 
    let symbol  = declToSym f

    -- try to add function symbol 
    tryAddSymbol symbol

    -- Push an empty scope 
    pushEmptyScope  -- argument scope

    -- try add arguments as variables
    let variables = [
                ST.Symbol {
                    ST.identifier=_argName,
                    ST.symType= ST.Variable{ST.varType= _argType, ST.initVal=_defaultVal, ST.isConst = False} ,
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
    return f

-- | Check if a given expression uses valid names only
checkExpr :: AST.Expr -> AnalyzerState AST.Expr

-- Check Id expression:
checkExpr id@AST.Id {AST.name=_name, AST.position=_position} = checkIdIsVarOrReference _name >> return id

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
checkExpr f@AST.FunCall {AST.fname=_fname, AST.actualArgs=_actualArgs} = do

    -- check symbol definition 
    mbSym <- checkSymbolDefined _fname

    -- Check if symbol is a valid function 
    case mbSym of
        Just sym -> M.unless (ST.isFunction sym || ST.isProc sym) . addStaticError $
            SE.NotAValidFunction {
                SE.symName=_fname,
                SE.actualSymType=ST.symType sym
            }
        _ -> return ()

    -- check args expressions            
    M.forM_ _actualArgs checkExpr
    return f

--  Check for
checkExpr f@AST.For {AST.iteratorName=_iteratorName, AST.step=_step, AST.start=_start, AST.end=_end, AST.cicBody=_cicBody} = do

    -- check start, step & end expressions
    checkExpr _start

    checkExpr _step

    checkExpr _end

    -- push a new scope for iteration variable declaration 
    pushEmptyScope  -- iterator variable declaration

    -- create symbol for iterator variable 
    let iter = ST.Symbol {
                        ST.identifier=_iteratorName,
                        ST.symType= ST.Variable {
                            ST.varType=AST.TInt,
                            ST.initVal=Just _start,
                            ST.isConst = False
                            },
                        ST.scope=0,
                        ST.enrtyType=Nothing
                    }

    -- add symbol. Its name it's trivially valid since we push an empty scope before 
    -- adding it, so there's no symbol redefinition
    tryAddSymbol iter

    pushEmptyScope  -- body scope

    checkExpr _cicBody -- check body

    popEmptyScope   -- body scope

    popEmptyScope   -- iterator variable declaration

    return f

--  Check While
checkExpr w@AST.While {AST.cond=_cond, AST.cicBody=_cicBody} = do
    -- check condition expression
    checkExpr _cond

    -- check body
    pushEmptyScope          -- body scope

    -- check body expression
    checkExpr _cicBody

    popEmptyScope           -- body scope

    return w

--  Check if conditional expression
checkExpr i@AST.If {AST.cond=_cond, AST.accExpr=_accExpr, AST.failExpr=_failExpr} = do

    -- check boolean condition
    checkExpr _cond

    -- push an empty scope for each different body

    -- check accepted body
    pushEmptyScope  -- acc body scope

    checkExpr _accExpr

    popEmptyScope   -- acc body scope

    -- check failed body
    pushEmptyScope  -- acc body scope

    checkExpr _failExpr

    popEmptyScope   -- acc body scope

    return i

--  Check expression block 
checkExpr bl@AST.ExprBlock {AST.exprs=_exprs} = do

    -- push scope for this block
    pushEmptyScope -- body scope

    M.forM_ _exprs checkExpr

    popEmptyScope  -- body scope
    return bl

--  Check return 
checkExpr r@AST.Return {AST.expr=_expr} = checkExpr _expr >> return r

--  Check break 
checkExpr b@AST.Break {AST.expr=_expr} = checkExpr _expr >> return b

--  Check continue 
checkExpr c@AST.Continue {AST.expr=_expr} = checkExpr _expr >> return c

--  Check Declarations
checkExpr d@AST.Declaration {AST.decl=_decl} = checkDecls _decl >> return d

--  Check Binary Operation
checkExpr op@AST.Op2 {AST.opr1=_opr1, AST.opr2=_opr2} = checkExpr _opr1 >> checkExpr _opr2 >> return op

--  Check Unary Operation
checkExpr op@AST.Op1 {AST.opr=_opr} = checkExpr _opr >> return op

--  Check Array Literal Expression
checkExpr ar@AST.Array {AST.list=_list} = M.forM_ _list checkExpr >> return ar

--  Check Union type guessing
checkExpr un@AST.UnionTrying {AST.union=_union} = checkExpr _union >> return un

--  Check Union access 
checkExpr un@AST.UnionUsing {AST.union=_union} = checkExpr _union >> return un

--  Check New Expression
checkExpr n@AST.New {AST.typeName=_typeName} = checkType _typeName >> return n

--  Check Delete
checkExpr d@AST.Delete {AST.ptrExpr=_ptrExpr} = checkExpr _ptrExpr >> return d

--  Check array index access
checkExpr ind@AST.ArrayIndexing {AST.index=_index, AST.expr=_expr} = checkExpr _index >> checkExpr _expr >> return ind

-- Check Struct Literal
checkExpr strc@AST.ConstStruct {AST.structName=_structName, AST.list=_list} = do

    -- Check struct name of literal struct
    mbSym <- checkSymbolDefined _structName

    -- Check if symbol is a valid struct name 
    case mbSym of
        Just sym -> M.unless (ST.isStruct sym) . addStaticError $
            SE.NotAValidStruct {
                SE.symName=_structName,
                SE.actualSymType=ST.symType sym
            }
        _ -> return ()

    -- Check list of expressions
    M.forM_ _list checkExpr

    return strc

-- Check Union Literal
checkExpr un@AST.ConstUnion {AST.unionName=_unionName, AST.value=_value} = do

    -- Check union name of literal union
    mbSym <- checkSymbolDefined _unionName

     -- Check if symbol is a valid union name 
    case mbSym of
        Just sym -> M.unless (ST.isUnion sym) . addStaticError $
            SE.NotAValidUnion {
                SE.symName=_unionName,
                SE.actualSymType=ST.symType sym
            }
        _ -> return ()

    -- Check value expression
    checkExpr _value
    return un

checkExpr x = return x

-- | Checks if a given type is a valid one 
checkType :: AST.Type -> AnalyzerState AST.Type
checkType ct@AST.CustomType {AST.tName=_tName} = do -- When it is a custom type
    st@State{symTable=symTb} <- RWS.get

    -- try to find symbol
    let symbol =  ST.findSymbol _tName symTb

    -- Check if symbol was correct 
    case symbol of
        Nothing -> addStaticError SE.SymbolNotInScope { SE.symName=_tName} >> return AST.TypeError 
        Just s@ST.Symbol { ST.symType= ST.Type{} } -> return ct
        _  -> addStaticError SE.NotValidType{SE.nonTypeName = _tName} >> return AST.TypeError 


checkType a@(AST.TArray t sz) = do

    -- check type of array elements
    checkType t

    -- check size expression
    --M.void $ checkExpr sz

    return a

checkType (AST.TPtr t) = checkType t

checkType (AST.TReference t) = checkType t

checkType x = error $ "Unknown type: " ++ show x

-- | Generate a symbol from an AST declaration
declToSym :: AST.Declaration -> ST.Symbol
declToSym decl = ST.Symbol {
                    ST.identifier=AST.decName decl,
                    ST.symType=declToSymType decl,
                    ST.scope=0,
                    ST.enrtyType=Nothing
                }
    where
        declToSymType :: AST.Declaration -> ST.SymType
        declToSymType AST.Variable {AST.decName = _, AST.varType=_varType, AST.initVal=_initVal, AST.isConst=_isConst} =
            ST.Variable {
                ST.varType=_varType,
                ST.initVal=_initVal,
                ST.isConst=_isConst
            }

        declToSymType AST.Reference {AST.refName=_refName} = ST.Reference {ST.refName=_refName, ST.refType=Nothing}


        declToSymType AST.Union {AST.fields=_fields} = ST.UnionType {ST.fields=_fields}

        declToSymType AST.Struct {AST.fields=_fields} = ST.StructType {ST.fields=_fields}

        declToSymType AST.Func {AST.args=_args, AST.retType=_retType, AST.body=_body} =
            ST.Function {
                ST.args=_args,
                ST.retType= _retType,
                ST.body=_body
            }

-- | Try add symbol. If possible, add it, otherwise write proper errors to error log
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
                _        -> return ()


-- < Type Checking > -----------------------------------------------------------------------------------------------


-- | Get type of a given expression based on the current state, and report error if some
--   inconsistent behavior is found. Also report an error if such behavior is found
getType :: AST.Expr -> AnalyzerState AST.Type
getType AST.ConstChar   {} = return AST.TChar

getType AST.ConstString {} = return AST.TString

getType AST.ConstFalse  {} = return AST.TBool

getType AST.ConstTrue   {} = return AST.TBool

getType AST.ConstInt    {} = return AST.TInt

getType AST.ConstFloat  {} = return AST.TFloat

getType AST.ConstUnit      = return AST.TUnit

getType AST.ConstNull      = undefined

getType AST.ConstStruct {AST.structName=_structName, AST.list=_list} = do

    st@State{symTable=symTb} <- RWS.get

    -- find struct symbol to check if its a valid struct type 
    let fields = case ST.findSymbol _structName symTb of
                Just ST.Symbol { ST.symType=ST.StructType {ST.fields=_fields} } -> Just _fields
                _  -> Nothing

    -- use list of fields to check if given expression list matches
    let Just l = fields
        typeList = map snd l
        actualTypes = map getType _list
        -- TODO tengo que terminar esta función

    let res
         | isNothing fields = AST.TypeError
         | length _list /= length typeList = AST.TypeError
         | otherwise        = undefined

    return res

getType AST.ConstUnion {AST.unionName=_unionName} = do

    st@State{symTable=symTb} <- RWS.get

    -- find struct symbol to check if its a valid struct type 
    case ST.findSymbol _unionName symTb of
        Nothing -> return AST.TypeError
        Just sym -> return $ if ST.isUnion sym
                                then AST.CustomType {AST.tName=_unionName}
                                else AST.TypeError

    return undefined -- TODO tengo que terminar esta función

getType AST.Id {AST.name=_name} = do

    st@State{symTable=symTb} <- RWS.get

    -- Id in an expression could only refer to a variable, as we don't have first
    -- class functions or types
    case ST.findSymbol _name symTb of
        Just ST.Symbol {ST.symType=ST.Variable {ST.varType=Just t}} -> return t
        Just ST.Symbol {ST.symType=ST.Variable {ST.initVal= Just expr}} -> getType expr
        Just ST.Symbol {ST.symType=ST.Variable {}} -> error "should not have uninitialized variable whith no type and no initial value"
        _  -> return AST.TypeError

getType AST.Assign {AST.value=_value, AST.variable=_variable} = do

    -- Get state and value of value expression 
    State{symTable = symTb} <- RWS.get
    valType <- getType _value

    isOk <- case ST.findSymbol _variable symTb of
                Just ST.Symbol {ST.symType=ST.Variable {ST.varType=_varType, ST.isConst=_isConst, ST.initVal = _initVal}} -> do

                    -- if trying to add to a const variable, raise an error
                    isOkConst <- if _isConst
                                    then addStaticError SE.AssignToConst{SE.symName = _variable} >> return False
                                    else return True

                    -- Check if current type matches with expected type. This should not be necessary, we should know its exact type
                    -- as we parse, we change it 
                    isOkExactType <-  if valType /= AST.TypeError && isJust _varType
                                            then _checkTypeMatch' [fromJust _varType]  valType
                                            else return True

                    -- Check if init val, if exists, is a valid one 
                    isOkInitValType <- case _initVal of
                                            Nothing -> return False
                                            Just v  ->
                                                do
                                                    initVType <- getType v
                                                    _checkTypeMatch' [initVType] valType

                    return $ isOkConst && (isOkExactType || isOkInitValType)

                Nothing -> return False -- don't report error as this symbol is already reported as missing by the names analyzer


    let res
            | valType == AST.TypeError = valType
            | not isOk = AST.TypeError

    return res

getType AST.StructAssign {AST.value=_value} = getType _value -- TODO add check if struct is valid and tag is correct

getType AST.StructAccess {AST.struct=_struct, AST.tag = _value} = do

    -- Get type of given expression:
    stType <- getType _struct

    -- Get current state
    st@State{symTable=symTb} <- RWS.get

    -- Try to get struct name 
    let mbStructName = case stType of
                        AST.CustomType {AST.tName=_tName} -> Just _tName
                        _          -> Nothing
        -- Assume that struct is an actual struct
        Just structName = mbStructName

        -- Try to get symbol details
        mbStructSym = ST.findSymbol structName symTb

        -- Assume symbol is not nothing 
        Just structSym = mbStructSym

        -- Assume symbol is struct 
        ST.StructType {ST.fields=_fields} = ST.symType structSym

        -- Check if tag is in fields
        mbTagType = lookup _value _fields

        -- Assume tag is not nothing
        Just tagType = mbTagType
        -- TODO report errors with addStaticError
    let res
            | isNothing mbStructName =  AST.TypeError -- check if struct is an actual struct
            | isNothing mbStructSym  =  error $ "name of struct " ++ structName ++ "should be in scope" -- if strcut is an actual struct, its name should be in scope
            | not $ ST.isStruct structSym  = AST.TypeError  -- if name does not correspond to a struct, return type error 
            | isNothing mbTagType          = AST.TypeError
            | otherwise                    = tagType

    return res

-- | Check if a given expr typematchs an expected set of types, and report error if they don't
_checkTypeMatch :: [AST.Type] -> AST.Expr -> AnalyzerState Bool
_checkTypeMatch expected expr = getType expr >>= _checkTypeMatch' expected

-- | Check if a given type matches some of the expected ones 
_checkTypeMatch' :: [AST.Type] -> AST.Type -> AnalyzerState Bool
_checkTypeMatch' expected exprType = do
    -- Check if current type matches one of expected types
    case exprType of
        AST.TypeError -> return False
        t             -> if t `elem` expected
                            then return True
                            else do
                                addStaticError $ SE.UnmatchingTypes expected exprType
                                return False

-- | Check if a sorted list of expression matches a sorted list of types
_checkTypeMatchesArgs :: [[AST.Type]] -> [AST.Expr] -> AnalyzerState Bool
_checkTypeMatchesArgs expected args = M.zipWithM _checkTypeMatch expected args <&> and

