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
import Data.Functor((<&>))
-- ---------------------------------------------------------------
-- >> data ------------------------------------------------------

type ErrorLog = [SE.StaticError]

-- | State used to simulate an imperative process of type checking 
type ParserState = RWS.RWST () ErrorLog ParsingState IO

-- | State of current analysis 
newtype ParsingState = State { symTable :: ST.SymTable } deriving(Eq, Show)

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

    -- Create a new symbol for this function 
    let symbol  = declToSym f

    -- try to add function symbol 
    tryAddSymbol symbol                

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

checkField :: [(String, AST.Type)] -> ParserState [(String, AST.Type)]
checkField l@((nm, t):fields) = do

    let newSymT = ST.Variable{ ST.varType = t, ST.initVal = Nothing, ST.isConst = False }
        newSym = ST.Symbol
            { ST.identifier = nm
            , ST.symType = newSymT
            , ST.scope = 0
            , ST.enrtyType = Nothing
            }
    
    tryAddSymbol newSym

    return l    

-- | Add declarations to symbol table and check if they´re correct
checkDecls :: AST.Declaration -> ParserState AST.Declaration

-- Check Variable Declaration 
checkDecls v@AST.Variable{ AST.decName = sid, AST.varType =  t, AST.initVal = ival, AST.isConst = const} = do

    -- Create a new symbol
    let newSym = declToSym v

    -- Add new variable to symbol table 
    tryAddSymbol newSym

    -- check initial value if provided 
    M.forM_ ival (_checkTypeMatch'' t)

    return v

-- Check reference Declaration 
checkDecls r@AST.Reference{ AST.decName=sid, AST.refName = refId } = do
    -- Get current state
    currSt@State{symTable = st} <- RWS.get

    let refSym = ST.findSymbol refId  st
        currScope = ST.stCurrScope st

    -- Check if referenced symbol exists and it's a variable
    case refSym of
        Just ST.Symbol{ST.symType = ST.Variable{}} -> return ()
        Just ST.Symbol{ST.symType = ST.Reference{}} -> return ()
        Nothing -> addStaticError . SE.SymbolNotInScope $ refId
        _       -> addStaticError . SE.ReferencingNonVariable $ refId

    -- Get reference type and scope:
    let (refType, refScope) = case refSym of
            Just ST.Symbol{ST.symType = ST.Variable{ST.varType=t}, ST.scope = scp} -> (t, scp)
            Just ST.Symbol{ST.symType = ST.Reference{ST.refType=t, ST.refScope = scp}} -> (t, scp)
            _ -> (AST.TypeError, 0)

    -- path comprehension. As the references should be declared in correct order
    --                     (inverse topological order), recursion is not necessary.
        refId' = case refSym of
            Just ST.Symbol{ST.symType = ST.Reference{ST.refName=nm}} -> nm
            _ -> refId

        newType = ST.Reference refId' refType refScope
    -- create new symbol 
        newSym = ST.Symbol {ST.identifier=sid, ST.symType=newType, ST.scope=currScope, ST.enrtyType=Nothing}

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

    -- check if add symbol is possible 
    tryAddSymbol symbol

    return u

-- Check Struct definition 
checkDecls s@AST.Struct {AST.decName=_decName, AST.fields=_fields} = do

    --  Check that all types are valid (are defined)
    --M.forM_ (map snd _fields) checkType

    --  Create symbol 
    let symbol = declToSym s

    -- check if add symbol is possible 
    tryAddSymbol symbol

    return s

-- Check function declaration
checkDecls f@AST.Func {AST.decName=_decName, AST.args=_args, AST.retType=_retType, AST.body=_body} = do

    -- Check if function body matches return type
    _checkTypeMatch'' _retType _body

    -- Try to update function, as in the parser it just has a the signature, not the body
    let sym = declToSym f
    updateSymbol sym

    return f


-- | Check if a given expression uses valid names only
checkExpr :: AST.Expr -> ParserState AST.Expr
-- Check Id expression:
checkExpr i@AST.Id {AST.name=_name, AST.position=_position} = do

    --check if a valid symbol for variable or reference
    checkIdIsVarOrReference _name

    -- set new type 
    new_type <- getTypeOfId _name

    return i{AST.expType = new_type}

-- Check assign expression 
checkExpr a@AST.Assign {AST.variable=_variable, AST.value=_value} = do
    -- check if left hand corresponds to a variable or reference name
    checkIdIsVarOrReference _variable
    -- Check right side 
    --checkExpr _value
    return a

-- Check assign to struct
checkExpr structAsg@AST.StructAssign {AST.struct =_struct, AST.value=_value,  AST.tag=_tag} = do

    -- check that _struct is of struct type
    -- check that _tag is part of struct _struct 
    -- check _value has the same type than _tag
    
    -- Get struct name
    let strNm = case AST.expType _struct of
                    (AST.CustomType s) -> s
                    _ -> "$"

    -- Get type of the struct assignment
    structType <- if (strNm == "$") 
        then
            do 
               addStaticError (SE.UnmatchingTypes [AST.CustomType "<struct_type>"] (AST.expType _struct))
               return AST.TypeError 
        else
            do 

                -- Get current state
                currSt@State{symTable = st} <- RWS.get
                
                let structSym = ST.findSymbol strNm st

                -- Check if struct symbol exists and it's a struct type.
                case structSym of
                    Just ST.Symbol{ST.symType = ST.StructType{ST.fields=_fields}} -> do 
                        
                        -- get fields that has name tag
                        let ltag = filter (\(s,t)-> s ==_tag) _fields

                        if null ltag then do
                            addStaticError . SE.SymbolNotInScope $ _tag
                            return AST.TypeError
                        else do
                            -- Check the type of the tag is the same of the value
                            let tagType = snd . head $ ltag

                                valType = AST.expType _value

                            if (not $ typeMatch tagType valType) then do
                                addStaticError $ SE.UnmatchingTypes [tagType] valType
                                return AST.TypeError
                            else do
                                return $ AST.CustomType strNm

                    Nothing -> do
                        addStaticError . SE.SymbolNotInScope $ strNm
                        return AST.TypeError
                    
                    xd       -> do
                        addStaticError (SE.UnmatchingTypes [AST.CustomType "<struct_type>"] (AST.expType _struct))
                        return AST.TypeError
    
    let structAsg' = structAsg{AST.expType = structType}
    
    return structAsg'

-- Check struct access
checkExpr structAcc@AST.StructAccess {AST.struct =_struct, AST.tag =_tag} = do
    -- check _struct is a struct
    -- check that _tag is part of struct _struct

    -- Get struct name
    let strNm = case AST.expType _struct of
                    (AST.CustomType s) -> s
                    _ -> "$"

    -- Get type of the struct access
    structType <- if (strNm == "$") 
        then
            do 
               addStaticError (SE.UnmatchingTypes [AST.CustomType "<struct_type>"] (AST.expType _struct))
               return AST.TypeError 
        else
            do 

                -- Get current state
                currSt@State{symTable = st} <- RWS.get
                
                let structSym = ST.findSymbol strNm st

                -- Check if struct symbol exists and it's a struct type.
                case structSym of
                    Just ST.Symbol{ST.symType = ST.StructType{ST.fields=_fields}} -> do 
                        
                        -- get fields that has name tag
                        let ltag = filter (\(s,t)-> s ==_tag) _fields

                        if null ltag then do
                            addStaticError . SE.SymbolNotInScope $ _tag
                            return AST.TypeError
                        else do
                            return $ AST.CustomType strNm

                    Nothing -> do
                        addStaticError . SE.SymbolNotInScope $ strNm
                        return AST.TypeError
                    
                    xd       -> do
                        addStaticError (SE.UnmatchingTypes [AST.CustomType "<struct_type>"] (AST.expType _struct))
                        return AST.TypeError
    
    let structAcc' = structAcc{AST.expType = structType}

    return structAcc'

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

    -- Check that _step, _start and _end are of integer type
    checkExprList [AST.TInt, AST.TInt, AST.TInt] [_step, _start, _end]

    -- Set return type as the return type of the body
    let f' = f{AST.expType = AST.expType _cicBody}

    return f'

--  Check While
checkExpr w@AST.While {AST.cond=_cond, AST.cicBody=_cicBody} = do
    
    -- check condition expression is boolean
    _checkTypeMatch'' AST.TBool _cond

    -- Set return type as the return type of the body
    let w' = w{AST.expType = AST.expType _cicBody}

    return w'

--  Check if conditional expression
checkExpr i@AST.If {AST.cond=_cond, AST.accExpr=_accExpr, AST.failExpr=_failExpr} = do

    -- check condition expression is boolean
    _checkTypeMatch'' AST.TBool _cond

    -- Check _accExpr expression and _failExpr expression has the same type
    match <- _checkTypeMatch' (getCastClass $ AST.expType _accExpr) (AST.expType _failExpr)

    let ifType = if match
        then AST.expType _accExpr
        else AST.TypeError

    -- Set return type as the return type of the body
        i' = i{AST.expType = ifType}

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

--  Check Union type guessing (return a boolean)
checkExpr unionTrying@AST.UnionTrying {AST.union=_union, AST.tag=_tag} = do
    
    -- check _union is an union
    -- check that _tag is part of union _union

    -- Get union name
    let unionNm = case AST.expType _union of
                    (AST.CustomType s) -> s
                    _ -> "$"

    -- Get type of the union trying
    unionType <- if (unionNm == "$") 
        then
            do 
               addStaticError (SE.UnmatchingTypes [AST.CustomType "<union_type>"] (AST.expType _union))
               return AST.TypeError 
        else
            do 

                -- Get current state
                currSt@State{symTable = st} <- RWS.get
                
                let unionSym = ST.findSymbol unionNm st

                -- Check if struct symbol exists and it's a struct type.
                case unionSym of
                    Just ST.Symbol{ST.symType = ST.UnionType{ST.fields=_fields}} -> do 
                        
                        -- get fields that has name tag
                        let ltag = filter (\(s,t)-> s ==_tag) _fields

                        if null ltag then do
                            addStaticError . SE.SymbolNotInScope $ _tag
                            return AST.TypeError
                        else do
                            return $ AST.TBool

                    Nothing -> do
                        addStaticError . SE.SymbolNotInScope $ unionNm
                        return AST.TypeError
                    
                    xd       -> do
                        addStaticError (SE.UnmatchingTypes [AST.CustomType "<union_type>"] (AST.expType _union))
                        return AST.TypeError
    
    let unionTrying' = unionTrying{AST.expType = unionType}

    return unionTrying'

--  Check Union access 
checkExpr unionUsing@AST.UnionUsing {AST.union=_union, AST.tag=_tag} = do
    
    -- check _union is an union
    -- check that _tag is part of union _union

    -- Get union name
    let unionNm = case AST.expType _union of
                    (AST.CustomType s) -> s
                    _ -> "$"

    -- Get type of the union trying
    unionType <- if (unionNm == "$") 
        then
            do 
               addStaticError (SE.UnmatchingTypes [AST.CustomType "<union_type>"] (AST.expType _union))
               return AST.TypeError 
        else
            do 

                -- Get current state
                currSt@State{symTable = st} <- RWS.get
                
                let unionSym = ST.findSymbol unionNm st

                -- Check if struct symbol exists and it's a struct type.
                case unionSym of
                    Just ST.Symbol{ST.symType = ST.UnionType{ST.fields=_fields}} -> do 
                        
                        -- get fields that has name tag
                        let ltag = filter (\(s,t)-> s ==_tag) _fields

                        if null ltag then do
                            addStaticError . SE.SymbolNotInScope $ _tag
                            return AST.TypeError
                        else do
                            return $ AST.TBool

                    Nothing -> do
                        addStaticError . SE.SymbolNotInScope $ unionNm
                        return AST.TypeError
                    
                    xd       -> do
                        addStaticError (SE.UnmatchingTypes [AST.CustomType "<union_type>"] (AST.expType _union))
                        return AST.TypeError
    
    let unionUsing' = unionUsing{AST.expType = unionType}

    return unionUsing'

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
    cStructType <- case mbSym of
        Just sym -> do
            if (not $ ST.isStruct sym) 
                then do 
                    addStaticError $ SE.NotAValidStruct {
                        SE.symName=_structName,
                        SE.actualSymType=ST.symType sym
                    }
                    return AST.TypeError
                else do
                    return $ AST.CustomType _structName

        _ -> return AST.TypeError 

    let fieldsTypes = case mbSym of
            Just ST.Symbol{ST.symType = ST.StructType{ST.fields=_fields}} -> 
                map snd _fields
            _ -> []

    -- Check that the types in _list match
    expListOk <- checkExprList fieldsTypes _list

    let cStructType' = if expListOk
        then cStructType
        else AST.TypeError
    
        c' = c{AST.expType = cStructType'}

    return c'

-- Check Union Literal
checkExpr c@AST.ConstUnion {AST.unionName=_unionName, AST.value=_value, AST.tag=_tag} = do

    -- Check union name of literal union
    mbSym <- checkSymbolDefined _unionName

     -- Check if symbol is a valid union name 
    cUnionType <- case mbSym of
        Just sym -> do
            if (not $ ST.isUnion sym)
                then do
                    addStaticError $ SE.NotAValidUnion {
                        SE.symName=_unionName,
                        SE.actualSymType=ST.symType sym
                    }
                    return AST.TypeError
                else do
                    return $ AST.CustomType _unionName

        _ -> return AST.TypeError

    let tagType = case mbSym of
            Just ST.Symbol{ST.symType = ST.UnionType{ST.fields=_fields}} -> 
                (map snd) $ filter (\(str,t)->str==_tag) _fields
            _ -> []

    -- Check that the types in _list match
    tagTypeOk <- _checkTypeMatch' (concatMap getCastClass tagType) (AST.expType _value)

    let cUnionType' = if tagTypeOk && (not $ null tagType)
        then cUnionType
        else AST.TypeError
    
        c' = c{AST.expType = cUnionType'}

    return c'

checkExpr x = return x

-- | Checks if a given type is a valid one 
checkType :: AST.Type -> ParserState AST.Type
checkType t@AST.CustomType {AST.tName=_tName} = do -- When it is a custom type
    st@State{symTable=symTb} <- RWS.get

    -- try to find symbol
    let symbol =  ST.findSymbol _tName symTb

    -- Check if symbol was correct 
    case symbol of
        Just ST.Symbol { ST.symType= ST.Type{} } -> return t
        Just ST.Symbol { ST.symType= ST.StructType{} } -> return t
        Just ST.Symbol { ST.symType= ST.UnionType{} } -> return t
        Nothing -> do
            addStaticError $ SE.SymbolNotInScope { SE.symName=_tName}
            return AST.TypeError
        _  -> do 
            addStaticError $ SE.NotValidType{SE.nonTypeName = _tName}
            return AST.TypeError


checkType arrType@(AST.TArray t sz) = do

    -- check type of array elements
    -- checkType t
    
    -- check that size of array is int
    let sizeType = AST.expType sz

    case sizeType of
        AST.TInt -> return arrType
        _ -> return AST.TypeError

checkType ptr@(AST.TPtr t) = do
    -- checkType t
    return ptr

checkType ref@(AST.TReference t) = do
    --checkType t
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
            
        declToSymType AST.Reference {AST.refName=_refName} = ST.Reference {ST.refName=_refName, ST.refType=AST.TypeError, ST.refScope=0}


        declToSymType AST.Union {AST.fields=_fields} = ST.UnionType {ST.fields=_fields}

        declToSymType AST.Struct {AST.fields=_fields} = ST.StructType {ST.fields=_fields}

        declToSymType AST.Func {AST.args=_args, AST.retType=_retType, AST.body=_body} =
            ST.Function {
                ST.args=_args,
                ST.retType=_retType,
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

-- | Update a symbol with a new one 
updateSymbol :: ST.Symbol -> ParserState ()
updateSymbol sym = do 
    currSt@State{symTable=st} <- RWS.get
    let newSymTable = ST.updateSymbol sym st
    RWS.put currSt{symTable=newSymTable}

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

-- | Check if an if corresponds to a valid variable or reference, raise an error if not 
checkIdIsVarOrReference :: U.Name -> ParserState ()
checkIdIsVarOrReference name = do -- Check that given name is a valid one and it is a variable

    mbSym <- checkSymbolDefined name
    -- Raise invalid symbol if this symbol is not a variable nor a reference
    case mbSym of
        Just sym -> M.unless ( ST.isVariable  sym || ST.isReference  sym) $
                    addStaticError SE.NotAValidVariable {SE.symName=name, SE.actualSymType=ST.symType sym}
        _        -> return ()

-- | Return type of given id if a valid variable or reference, return typerrror if none of them
getTypeOfId :: U.Name -> ParserState AST.Type 
getTypeOfId name = do 
    st@State{symTable = symTb} <- RWS.get

    case ST.findSymbol name symTb of 
        Just ST.Symbol { ST.symType=ST.Variable {ST.varType=_varType} } -> return _varType
        Just ST.Symbol { ST.symType=ST.Reference { ST.refType=_refType } } -> return _refType
        _ -> return AST.TypeError 

    



-- < Utility functions to check matching types > ---------------------------------------------------- 


-- Check if two types match
typeMatch :: AST.Type -> AST.Type -> Bool
typeMatch t1 t2 = t1 == t2

-- return the list of cast-able types with each other
-- that contains the given type
getCastClass :: AST.Type -> [AST.Type]
getCastClass AST.TInt   = [AST.TInt, AST.TFloat]
getCastClass AST.TFloat = [AST.TInt, AST.TFloat]
getCastClass t          = [t]

-- check that the given types match the given expressions
checkExprList :: [AST.Type] -> [AST.Expr] -> ParserState Bool
checkExprList ts = _checkTypeMatchesArgs (map getCastClass ts)


-- | Check if a given expr typematchs an expected set of types, and report error if they don't
_checkTypeMatch :: [AST.Type] -> AST.Expr -> ParserState Bool
_checkTypeMatch expected  = _checkTypeMatch' expected . AST.expType  

-- | Check if a given type matches some of the expected ones 
_checkTypeMatch' :: [AST.Type] -> AST.Type -> ParserState Bool
_checkTypeMatch' expected exprType = do
    -- Check if current type matches one of expected types
    case exprType of
        AST.TypeError -> return False
        t             -> if t `elem` expected
                            then return True
                            else do
                                addStaticError $ SE.UnmatchingTypes expected exprType
                                return False

_checkTypeMatch'' :: AST.Type -> AST.Expr -> ParserState Bool
_checkTypeMatch'' t = _checkTypeMatch [t]

-- | Check if a sorted list of expression matches a sorted list of types
_checkTypeMatchesArgs :: [[AST.Type]] -> [AST.Expr] -> ParserState Bool
_checkTypeMatchesArgs expected args = M.zipWithM _checkTypeMatch expected args <&> and

