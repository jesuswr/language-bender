-- Parser Functions and types for static analisys and type checking.
module FrontEnd.ParserCheck where



-- <Language Bender Imports> ------------------------------------
import qualified FrontEnd.StaticErrors  as SE
import qualified FrontEnd.AST           as AST
import qualified FrontEnd.SymTable      as ST
import qualified FrontEnd.Utils         as U

-- <Utility Data types> -----------------------------------------
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad     as M
import Data.Maybe(isNothing, maybe, fromMaybe, isJust, fromJust)

-- ---------------------------------------------------------------
-- >> data ------------------------------------------------------

type ErrorLog = [SE.StaticError]

-- | State used to simulate an imperative process of type checking 
type ParserState = RWS.RWST () ErrorLog ParsingState IO

-- | State of current analysis 
newtype ParsingState = State { symTable :: ST.SymTable }

-- -------------------------------------------------------------------
-- >> Commons -------------------------------------------------------

--unPack :: ParserState a -> a
--unPack (RWS.RWST _ _ _ _ r) = r

startingState :: ParsingState
startingState = State ST.newTable

-- parseProgram :: AST.Program -> IO (ParsingState, ErrorLog)
-- parseProgram p = do
--     (_, s, e) <- RWS.runRWST (namesAnalysis p) () (startingState p)
--     return (s, e)

-- | Add error to state of RWST
addStaticError :: SE.StaticError -> ParserState ()
addStaticError e = RWS.tell [e]

-- | Function to check if every name used is in the current scope
-- namesAnalysis :: AST.Program -> ParserState ()
-- namesAnalysis p@AST.Program{AST.decls=ds} = M.forM_ ds checkDecls

-- --------------------------------------------------------------------
-- >> PreParser ------------------------------------------------------

preCheckDecls :: AST.Declaration -> ParserState ()

preCheckDecls f@AST.Func {AST.decName=_decName, AST.args=_args, AST.retType=_retType, AST.body=_body} = do

    -- check valid return type if needed 
    M.when (isJust _retType) $ do
        let Just t = _retType
        M.void $ checkType t

    -- Create a new symbol for this function 
    let symbol  = declToSym f

    -- try to add function symbol 
    tryAddSymbol symbol


preCheckFunArgs :: [AST.FuncArg] -> ParserState [AST.FuncArg]
preCheckFunArgs _args = do

    -- Function to check a single function argument 
    let checkFArg :: AST.FuncArg -> ParserState ()
        checkFArg AST.FuncArg {AST.argType=_argType, AST.defaultVal=_defaultVal} = do
                        M.void $ checkType _argType -- check argument type 
                        --M.when (isJust _defaultVal) $ checkExpr (fromJust _defaultVal) -- check expression validity

    -- check arguments
    M.forM_ _args checkFArg

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

    return _args



-- --------------------------------------------------------------------
-- >> Parser ---------------------------------------------------------


checkFunArg :: AST.FuncArg -> ParserState AST.FuncArg
checkFunArg arg@(AST.FuncArg _argName _argType _defaultVal) = let sym = ST.Symbol {
                                                        ST.identifier=_argName,
                                                        ST.symType= ST.Variable{ST.varType= _argType, ST.initVal=_defaultVal, ST.isConst = False} ,
                                                        ST.scope=0,
                                                        ST.enrtyType=Nothing
                                                    }
                                            in tryAddSymbol sym >> return arg

-- | Add declarations to symbol table and check if they´re correct
checkDecls :: AST.Declaration -> ParserState AST.Declaration

-- Check Variable Declaration 
checkDecls v@AST.Variable{ AST.decName = sid, AST.varType =  t, AST.initVal = ival, AST.isConst = const} = do

    -- Create a new symbol
    let newSym = declToSym v

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
        _       -> addStaticError . SE.ReferencingNonVariable $ refId

    -- Get reference type:
    let refType = case refSym of
            Just ST.Symbol{ST.symType = ST.Variable{ST.varType=t}} -> t
            Just ST.Symbol{ST.symType = ST.Reference{ST.refType=t}} -> t
            _ -> AST.TypeError

    -- path comprehension. As the references should be declared in correct order
    --                     (inverse topological order), recursion is not necessary.
        refId' = case refSym of
            Just ST.Symbol{ST.symType = ST.Reference{ST.refName=nm}} -> nm
            _ -> refId

        newType = ST.Reference refId' refType
    -- create new symbol 
        newSym = ST.Symbol {ST.identifier=sid, ST.symType=newType, ST.scope=0, ST.enrtyType=Nothing}

    -- try to add symbol 
    tryAddSymbol newSym

    return r

-- Check union definition 
checkDecls u@AST.Union {AST.decName=_decName, AST.fields=_fields} = do

    -- Create new symbol 
    --  Check that all types are valid 
    M.forM_ (map snd _fields) checkType

    --  Create symbol type
    let symbol = declToSym u

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

    -- check if add symbol is possible 
    tryAddSymbol symbol

    return s

-- Check function declaration
checkDecls f@AST.Func {AST.decName=_decName, AST.args=_args, AST.retType=_retType, AST.body=_body} = do


    let bodyType = AST.expType _body
        
        checkFuncType (Just t)
            | bodyType == t = Just bodyType       -- | types of firm and body are equal
            | otherwise     = Just AST.TypeError  -- | type error
        checkFuncType Nothing = Just bodyType     -- | Inferred type

        funcType = checkFuncType _retType

        f' = f{AST.retType=funcType}

    -- Create a new symbol for this function 
        symbol  = declToSym f'

    -- try to add function symbol 
    tryAddSymbol symbol

    return f'

-- | Check if a given expression uses valid names only
checkExpr :: AST.Expr -> ParserState AST.Expr

-- Check Id expression:
checkExpr i@AST.Id {AST.name=_name, AST.position=_position} = do
    
    -- Get current state
    currSt@State{symTable = st} <- RWS.get
    
    checkIdIsVarOrReference _name

    let idSym = ST.findSymbol _name  st

        idType = case idSym of 
            Just idSym' -> ST.getIdType _name $ ST.symType idSym'
            Nothing -> AST.TypeError

        i' = i{AST.expType = idType}

    return i'

-- Check assign expression 
checkExpr a@AST.Assign {AST.variable=_variable, AST.value=_value} = do
    -- check if left hand corresponds to a variable or reference name
    checkIdIsVarOrReference _variable
    -- Check right side 
    --checkExpr _value
    return a

-- Check assign to struct
checkExpr s@AST.StructAssign {AST.struct =_struct, AST.value=_value} = do
    -- check that given symbol is valid expression  
    --checkExpr _struct
    -- check right hand value
    --checkExpr _value
    return s

-- Check struct access
checkExpr s@AST.StructAccess {AST.struct =_struct} = do
    --checkExpr _struct
    return s

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
    --M.forM_ _actualArgs checkExpr
    return f

--  Check for
checkExpr f@AST.For {AST.iteratorName=_iteratorName, AST.step=_step, AST.start=_start, AST.end=_end, AST.cicBody=_cicBody} = do

    -- check start, step & end expressions
    --checkExpr _start

    --checkExpr _step

    --checkExpr _end

    -- push a new scope for iteration variable declaration 
    --pushEmptyScope  -- iterator variable declaration

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

    --pushEmptyScope  -- body scope

    --checkExpr _cicBody -- check body

    --popEmptyScope   -- body scope

    --popEmptyScope   -- iterator variable declaration
    return f

--  Check While
checkExpr w@AST.While {AST.cond=_cond, AST.cicBody=_cicBody} = do
    -- check condition expression
    --checkExpr _cond

    -- check body
    --pushEmptyScope          -- body scope

    -- check body expression
    --checkExpr _cicBody

    --popEmptyScope           -- body scope
    return w

--  Check if conditional expression
checkExpr i@AST.If {AST.cond=_cond, AST.accExpr=_accExpr, AST.failExpr=_failExpr} = do

    -- check boolean condition
    --checkExpr _cond

    -- push an empty scope for each different body

    -- check accepted body
    --pushEmptyScope  -- acc body scope

    --checkExpr _accExpr

    --popEmptyScope   -- acc body scope

    -- check failed body
    --pushEmptyScope  -- acc body scope

    --checkExpr _failExpr

    --popEmptyScope   -- acc body scope
    return i

--  Check expression block 
checkExpr e@AST.ExprBlock {AST.exprs=_exprs} = do

    -- push scope for this block
    --pushEmptyScope -- body scope

    --M.forM_ _exprs checkExpr

    --popEmptyScope  -- body scope
    return e

--  Check return 
checkExpr r@AST.Return {AST.expr=_expr} = do
    --checkExpr _expr
    return r

--  Check break 
checkExpr b@AST.Break {AST.expr=_expr} = do
    --checkExpr _expr
    return b

--  Check continue 
checkExpr c@AST.Continue {AST.expr=_expr} = do
    --checkExpr _expr
    return c

--  Check Declarations
checkExpr d@AST.Declaration {AST.decl=_decl} = do
    --checkDecls _decl
    return d

--  Check Binary Operation
checkExpr o@AST.Op2 {AST.opr1=_opr1, AST.opr2=_opr2} = do
    --checkExpr _opr1
    --checkExpr _opr2
    return o

--  Check Unary Operation
checkExpr o@AST.Op1 {AST.opr=_opr} = do
    --checkExpr _opr
    return o

--  Check Array Literal Expression
checkExpr a@AST.Array {AST.list=_list} = do
    --M.forM_ _list checkExpr
    return a

--  Check Union type guessing
checkExpr u@AST.UnionTrying {AST.union=_union} = do
    --checkExpr _union
    return u

--  Check Union access 
checkExpr u@AST.UnionUsing {AST.union=_union} = do
    --checkExpr _union
    return u

--  Check New Expression
checkExpr n@AST.New {AST.typeName=_typeName} = do
    --checkType _typeName
    return n

--  Check Delete
checkExpr d@AST.Delete {AST.ptrExpr=_ptrExpr} = do
    --checkExpr _ptrExpr
    return d

--  Check array index access
checkExpr a@AST.ArrayIndexing {AST.index=_index, AST.expr=_expr} = do
    --checkExpr _index >> checkExpr _expr
    return a

-- Check Struct Literal
checkExpr c@AST.ConstStruct {AST.structName=_structName, AST.list=_list} = do

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
    --M.forM_ _list checkExpr
    return c

-- Check Union Literal
checkExpr c@AST.ConstUnion {AST.unionName=_unionName, AST.value=_value} = do

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
    --checkExpr _value
    return c

checkExpr x = return x

-- | Checks if a given type is a valid one 
checkType :: AST.Type -> ParserState AST.Type
checkType t@AST.CustomType {AST.tName=_tName} = do -- When it is a custom type
    st@State{symTable=symTb} <- RWS.get

    -- try to find symbol
    let symbol =  ST.findSymbol _tName symTb

    -- Check if symbol was correct 
    case symbol of
        Nothing -> addStaticError $ SE.SymbolNotInScope { SE.symName=_tName}
        Just ST.Symbol { ST.symType= ST.Type{} } -> return ()
        _  -> addStaticError $ SE.NotValidType{SE.nonTypeName = _tName}

    return t

checkType arrType@(AST.TArray t sz) = do

    -- check type of array elements
    checkType t
    
    -- check that size of array is int
    let sizeType = AST.expType sz

    case sizeType of
        AST.TInt -> return arrType
        _ -> return AST.TypeError

checkType ptr@(AST.TPtr t) = do
    checkType t
    return ptr

checkType ref@(AST.TReference t) = do
    checkType t
    return ref

checkType x = return x

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
            
        declToSymType AST.Reference {AST.refName=_refName} = ST.Reference {ST.refName=_refName, ST.refType=undefined}


        declToSymType AST.Union {AST.fields=_fields} = ST.UnionType {ST.fields=_fields}

        declToSymType AST.Struct {AST.fields=_fields} = ST.StructType {ST.fields=_fields}

        declToSymType AST.Func {AST.args=_args, AST.retType=_retType, AST.body=_body} =
            ST.Function {
                ST.args=_args,
                ST.retType=fromMaybe AST.TUnit  _retType,
                ST.body=_body
            }

-- | Try add symbol. If possible, add it, otherwise write proper errors
tryAddSymbol :: ST.Symbol -> ParserState ()
tryAddSymbol s@ST.Symbol {ST.identifier=_identifier} = do
    -- get current state
    currSt@State{symTable=st} <- RWS.get

    case ST.insertSymbol s st of
        Nothing -> addStaticError $ SE.SymbolRedefinition _identifier -- if could not add, it's because of symbol redefinition
        Just st' -> RWS.put currSt{symTable = st'}                   -- update state

-- | Utility function to push an empty scope, updating state
pushEmptyScope :: ParserState ()
pushEmptyScope = do
    st@State{symTable = symTb} <- RWS.get

    let symTb' = ST.pushEmptyScope symTb

    RWS.put st{symTable=symTb'}

-- | Utility function to pop an empty scope, possibly changing current state
popEmptyScope :: ParserState ()
popEmptyScope = do
    st@State{symTable = symTb} <- RWS.get

    let symTb' = ST.popCurrentScope  symTb

    RWS.put st{symTable = symTb'}

-- | Utility function to check if a given name corresponds to a valid symbol, and return it if exists
checkSymbolDefined :: U.Name -> ParserState (Maybe ST.Symbol)
checkSymbolDefined name = do
    st@State{symTable = symTb} <- RWS.get

    -- Try find symbol 
    case ST.findSymbol name symTb of
        Nothing  -> addStaticError SE.SymbolNotInScope {SE.symName=name} >> return Nothing
        Just sym -> return . Just $ sym

checkIdIsVarOrReference :: U.Name -> ParserState ()
checkIdIsVarOrReference name = do -- Check that given name is a valid one and it is a variable

            mbSym <- checkSymbolDefined name
            -- Raise invalid symbol if this symbol is not a variable nor a reference
            case mbSym of
                Just sym -> M.unless ( ST.isVariable  sym || ST.isReference  sym) $
                            addStaticError SE.NotAValidVariable {SE.symName=name, SE.actualSymType=ST.symType sym}
                _        -> return ()