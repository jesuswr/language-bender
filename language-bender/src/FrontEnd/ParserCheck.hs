-- Parser Functions and types for static analisys and type checking.

{-# OPTIONS_GHC -Wall #-}

module FrontEnd.ParserCheck where

-- <Language Bender Imports> ------------------------------------
import qualified FrontEnd.StaticErrors  as SE
import qualified FrontEnd.AST           as AST
import qualified FrontEnd.SymTable      as ST
import qualified FrontEnd.Utils         as U

-- <Utility Data types> -----------------------------------------
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad     as M
import Data.Functor((<&>))
import Data.Maybe(isJust, fromMaybe)
-- ---------------------------------------------------------------
-- >> data ------------------------------------------------------

type ErrorLog = [SE.StaticError]

-- | State used to simulate an imperative process of type checking 
type ParserState = RWS.RWST () ErrorLog ParsingState IO

-- | State of current analysis 
data ParsingState =
    State
    { symTable :: ST.SymTable
    , expectedTypeStack :: [AST.Type]
    , expectedLoopTypeStack :: [AST.Type]
    } deriving(Eq, Show)

-- -------------------------------------------------------------------
-- >> Commons -------------------------------------------------------

startingState :: ParsingState
startingState = State ST.newTable [AST.TUnit] [AST.TUnit]

-- | Add error to state of RWST
addStaticError :: SE.StaticError -> ParserState ()
addStaticError e = RWS.tell [e]

-- | Get Current Scope
getScope :: ParserState ST.Scope
getScope = RWS.get <&> (ST.stCurrScope . symTable)

-- | Try to get a type from a name, return type error if not possible with the current state
getCustomType :: U.Name -> ParserState AST.Type
getCustomType sid = do
    State{symTable = symTb} <- RWS.get  -- get current symbol table

    let foundType = ST.findSymbol sid symTb

    case foundType of
         Just s@ST.Symbol {ST.identifier=_identifier, ST.symType=ST.UnionType {}} -> return $ AST.CustomType sid $ ST.scope s
         Just s@ST.Symbol {ST.identifier=_identifier, ST.symType=ST.StructType {}} -> return $ AST.CustomType sid $ ST.scope s
         Nothing -> do
             addStaticError $ SE.SymbolNotInScope{SE.symName=sid}
             return AST.TypeError
         _  -> do
             addStaticError $ SE.NotValidType{SE.nonTypeName = sid}
             return AST.TypeError

-- | Check if exist nested functions and add error if so
checkNestedFunctions :: String -> ParserState ()
checkNestedFunctions name = do
    State{expectedTypeStack = stk} <- RWS.get
    M.when (length stk > 2) $ do
        addStaticError $ SE.NestedFunctions{SE.symName = name}

-- | Add this type to the stack of expected types
pushType :: AST.Type -> ParserState ()
pushType t = do
    s@State{expectedTypeStack = stk} <- RWS.get
    RWS.put s{expectedTypeStack = t:stk}

-- | Pop type from top of stack 
popType :: ParserState ()
popType = do
    s@State{expectedTypeStack = stk} <- RWS.get

    case stk of
            [_]  -> return ()
            _:ts -> RWS.put s{expectedTypeStack=ts}
            _ -> error "Error in ParserState: Inconsistent state, stack of expected types should not be empty"

-- | Replace type at the top of the stack with a new one
replaceType :: AST.Type -> ParserState ()
replaceType newType = do
    s@State{expectedTypeStack=stk} <- RWS.get

    case stk of
        [_] -> error "Error in ParserState: You shouldn't be replacing base expected type"
        AST.TVoid:ts -> RWS.put s{expectedTypeStack=newType:ts}
        _ -> return ()

-- | Get top of the type stack 
topType :: ParserState AST.Type
topType = RWS.get <&> head . expectedTypeStack

-- | Add this type to the stack of expected loop types
pushLoopType :: AST.Type -> ParserState ()
pushLoopType t = do
    s@State{expectedLoopTypeStack = stk} <- RWS.get
    RWS.put s{expectedLoopTypeStack = t:stk}

-- | Pop type from top of loop stack 
popLoopType :: ParserState ()
popLoopType = do
    s@State{expectedLoopTypeStack = stk} <- RWS.get

    case stk of
            [_]  -> return ()
            _:ts -> RWS.put s{expectedLoopTypeStack=ts}
            _ -> error "Error in ParserState: Inconsistent state, stack of expected types should not be empty"

-- | Replace type at the top of the loop stack with a new one
replaceLoopType :: AST.Type -> ParserState ()
replaceLoopType newType = do
    s@State{expectedLoopTypeStack=stk} <- RWS.get

    case stk of
        [_] -> error "Error in ParserState: You shouldn't be replacing base expected type"
        AST.TVoid:ts -> RWS.put s{expectedLoopTypeStack=newType:ts}
        _ -> return ()

-- | Get top of the loop type stack 
topLoopType :: ParserState AST.Type
topLoopType = RWS.get <&> head . expectedLoopTypeStack

-- | Tells if the given type is scalar or not
isScalarType :: AST.Type -> Bool
isScalarType AST.TFloat     = True
isScalarType AST.TInt       = True
isScalarType AST.TChar      = True
isScalarType AST.TBool      = True
isScalarType AST.TPtr{}     = True
isScalarType AST.TUnit      = True
isScalarType AST.TypeError  = True
isScalarType _ = False

-- | Tells if the given type is a reference
isRef :: AST.Type -> Bool 
isRef AST.TReference{} = True
isRef _ = False

-- --------------------------------------------------------------------
-- >> PreParser ------------------------------------------------------

preCheckDecls :: AST.Declaration -> ParserState ()

preCheckDecls f@AST.Func {AST.decName=_decName, AST.args=_args, AST.retType=_retType, AST.body=_body} = do

    -- Create a new symbol for this function 
    let symbol  = declToSym f

    -- try to add function symbol 
    tryAddSymbol symbol

preCheckDecls s@AST.Struct {AST.decName=_decName, AST.fields=_fields} = do

    --  Create symbol 
    let symbol = declToSym s

    -- check if add symbol is possible 
    tryAddSymbol symbol


preCheckDecls u@AST.Union {AST.decName=_decName, AST.fields=_fields} = do

    --  Create symbol type
    let symbol = declToSym u

    -- check if add symbol is possible 
    tryAddSymbol symbol

preCheckDecls x = error $ "Could no check available for  the following object: " ++ show x

preCheckFunArg :: AST.FuncArg -> ParserState AST.FuncArg
preCheckFunArg arg@(AST.FuncArg _argName _argType _defaultVal _) = do

    let sym = ST.Symbol {
            ST.identifier=_argName,
            ST.symType= ST.Variable{ST.varType= _argType, ST.initVal=_defaultVal, ST.isConst = False, ST.offset = 0,ST.staticLabel=Nothing} , --arreglar despues
            ST.scope=0,
            ST.enrtyType=Nothing
        }

    tryAddSymbol sym

    return arg

-- --------------------------------------------------------------------
-- >> Parser ---------------------------------------------------------

checkFunArg :: AST.FuncArg -> ParserState AST.FuncArg
checkFunArg arg@(AST.FuncArg _argName _argType _defaultVal _) = do

    State{symTable = st} <- RWS.get

    let width = ST.getTypeSize st _argType
        align = ST.getTypeAlign st _argType
    o <- updateOffset align width

    let sym = ST.Symbol {
            ST.identifier=_argName,
            ST.symType= ST.Variable{ST.varType= _argType, ST.initVal=_defaultVal, ST.isConst = False, ST.offset = o,ST.staticLabel=Nothing} ,
            ST.scope=0,
            ST.enrtyType=Nothing
        }

    M.when (isJust _defaultVal) $ do
        _ <- _checkTypeMatch'' _argType (fromMaybe (AST.ConstUnit AST.TUnit) _defaultVal)
        return ()

    currScope <- getScope

    updateSymbol sym
    return arg{AST._declScope = currScope}

checkField :: [(String, AST.Type)] -> ParserState [(String, AST.Type)]
checkField l@((nm, t):_) = do
    State{symTable = st} <- RWS.get

    let newSymT = ST.Variable{ ST.varType = t, ST.initVal = Nothing, ST.isConst = False , ST.offset = 0, ST.staticLabel=Nothing}  --arreglar despues
        width = ST.getTypeSize st t
        align = ST.getTypeAlign st t
    o <- updateOffset align width

    let newSym = ST.Symbol
            { ST.identifier = nm
            , ST.symType = newSymT{ST.offset=o}
            , ST.scope = 0
            , ST.enrtyType = Nothing
            }

    tryAddSymbol newSym

    return l

checkField x = error $ "Unsupported field variation in checkField function: " ++ show x

-- | Add declarations to symbol table and check if they´re correct
checkDecls :: AST.Declaration -> ParserState AST.Declaration

-- Check Variable Declaration 
checkDecls v@AST.Variable{ AST.decName = sid, AST.varType =  t, AST.initVal = ival} = do
    State{symTable = st} <- RWS.get

    -- Create a new symbol
    let newSym = declToSym v

    let width = ST.getTypeSize st t
        align = ST.getTypeAlign st t
    o <- updateOffset align width

    -- Add new variable to symbol table 
    tryAddSymbol newSym{ST.symType=(ST.symType newSym){ST.offset=o}}

    -- check initial value if provided 
    match <- case ival of
        Just ival_ -> _checkTypeMatch'' t ival_
        _          -> return True
    -- check that unit is not assigned
    assignUnit <- case ival of
        Just ival_ -> case AST.expType ival_ of
            AST.TUnit -> return True
            _      -> return False
        _          -> return False

    M.when assignUnit $ do
        addStaticError $ SE.AssignUnit{SE.symName = sid}

    let varType_ = if match && not assignUnit
        then t
        else AST.TypeError 

    currScope <- getScope

    return v{ AST.declScope = currScope, AST.varType=varType_ }

-- Check reference Declaration 
checkDecls r@AST.Reference{ AST.decName=sid, AST.refName = refId } = do
    -- Get current state
    State{symTable = st} <- RWS.get

    currScope <- getScope

    let refSym = ST.findSymbol refId  st
        -- currScope = ST.stCurrScope st

    let width = ST.getTypeSize st (AST.TReference AST.TVoid)
        align = ST.getTypeAlign st (AST.TReference AST.TVoid)
    o <- updateOffset align width

    -- Check if referenced symbol exists and it's a variable
    case refSym of
        Just ST.Symbol{ST.symType = ST.Variable{}} -> return ()
        Just ST.Symbol{ST.symType = ST.Reference{}} -> return ()
        Nothing -> addStaticError . SE.SymbolNotInScope $ refId
        _       -> addStaticError $ SE.ReferencingNonVariable sid refId

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

        newType = ST.Reference refId' refType refScope o Nothing
    -- create new symbol 
        newSym = ST.Symbol {ST.identifier=sid, ST.symType=newType, ST.scope=currScope, ST.enrtyType=Nothing}
    
    -- try to add symbol 
    tryAddSymbol newSym

    return r{ AST.declScope = currScope}

-- Check union definition 
checkDecls u@AST.Union {AST.decName=_decName, AST.fields=_fields, AST.width=_w, AST.align=_a, AST.fieldScope=_fieldScope} = do

    --  Create symbol type
    let symbol = declToSym u

    -- check if add symbol is possible 
    updateSymbol symbol{ST.symType=(ST.symType symbol){ST.width=_w+4, ST.align=_a, ST.fieldScope=_fieldScope}}

    currScope <- getScope

    return u{ AST.declScope = currScope, AST.width = _w+4 }

-- Check Struct definition 
checkDecls s@AST.Struct {AST.decName=_decName, AST.fields=_fields, AST.width=_w, AST.align=_a, AST.fieldScope=_fieldScope} = do

    --  Create symbol 
    let symbol = declToSym s

    -- check if add symbol is possible 
    updateSymbol symbol{ST.symType=(ST.symType symbol){ST.width=_w, ST.align=_a, ST.fieldScope=_fieldScope}}

    currScope <- getScope

    return s{ AST.declScope = currScope }

-- Check function declaration
checkDecls f@AST.Func {AST.decName=_decName, AST.args=_args, AST.retType=_retType, AST.body=_body} = do

    -- Check if function body matches return type
    _ <- _checkTypeMatch'' _retType _body

    -- Check if return type is scalar
    M.unless (isScalarType _retType) $ do 
        addStaticError (SE.NonScalarReturnType _decName _retType)

    -- Check if return type is reference
    M.when (isRef _retType) $ do 
        addStaticError (SE.ReturnTypeIsRef _decName _retType)

    -- Try to update function, as in the parser it just has a the signature, not the body
    let sym = declToSym f
    updateSymbol sym

    currScope <- getScope

    return f{ AST.declScope = currScope }


-- | Check if a given expression uses valid names only
checkExpr :: AST.Expr -> ParserState AST.Expr
-- Check Id expression:
checkExpr i@AST.Id {AST.name=_name, AST.position=_position} = do

    State{symTable = st} <- RWS.get

    --check if a valid symbol for variable or reference
    checkIdIsVarOrReference _name

    -- set new type 
    new_type <- getTypeOfId _name

    let idScope = case ST.findSymbol _name st of
            Just ST.Symbol{ST.scope = scope_ } -> scope_
            Nothing -> -1

    return i{AST.expType = new_type, AST.declScope_ = idScope}

-- Check assign expression 
checkExpr a@AST.Assign {AST.variable=_variable, AST.value=_value} = do

    State{symTable = st} <- RWS.get
    -- check if left hand corresponds to a variable or reference name
    checkIdIsVarOrReference _variable

    -- get the variable type 
    var_type <- getTypeOfId _variable

    -- check that var_type and the type of _value match
    match <- _checkTypeMatch (getCastClass var_type) _value
    -- check that unit is not assigned
    assignUnit <- case AST.expType _value of
        AST.TUnit -> return True
        _         -> return False

    
    let idScope = case ST.findSymbol _variable st of
            Just ST.Symbol{ST.scope = scope_ } -> scope_
            Nothing -> -1

    M.when (ST.getVarIsConst st _variable idScope) $ do
        addStaticError (SE.AssignToConst _variable)

    -- get return type
    let assgType = if match && not assignUnit
        then var_type
        else AST.TypeError


    return a{AST.expType = assgType, AST.declScope_ = idScope}

-- Check assign to struct
checkExpr structAsg@AST.StructAssign {AST.struct =_struct, AST.value=_value,  AST.tag=_tag} = do

    -- check that _struct is of struct type
    -- check that _tag is part of struct _struct 
    -- check _value has the same type than _tag

    -- Get struct name
    let (strNm, scope) = case AST.expType _struct of
                    (AST.CustomType s scope') -> (s,scope')
                    _ -> ("$", -1)

    -- Get type of the struct assignment
    structType <- if strNm == "$"
        then
            do
               addStaticError (SE.UnmatchingTypes [AST.CustomType "<struct_type>" (-1)] (AST.expType _struct))
               return AST.TypeError
        else
            do

                -- Get current state
                State{symTable = st} <- RWS.get

                let structSym = ST.findSymbolInScope strNm scope st

                -- Check if struct symbol exists and it's a struct type.
                case structSym of
                    Just ST.Symbol{ST.symType = ST.StructType{ST.fields=_fields}} -> do

                        -- get fields that has name tag
                        let ltag = filter (\(s, _)-> s ==_tag) _fields

                        if null ltag then do
                            addStaticError . SE.SymbolNotInScope $ _tag
                            return AST.TypeError
                        else do
                            -- Check the type of the tag is the same of the value
                            let tagType = snd . head $ ltag

                                valType = AST.expType _value

                            if not $ typeMatch tagType valType then do
                                addStaticError $ SE.UnmatchingTypes [tagType] valType
                                return AST.TypeError
                            else
                                return $ AST.CustomType strNm (scope)

                    Nothing -> do
                        addStaticError . SE.SymbolNotInScope $ strNm
                        return AST.TypeError

                    _       -> do
                        addStaticError (SE.UnmatchingTypes [AST.CustomType "<struct_type>" (-1)] (AST.expType _struct))
                        return AST.TypeError

    let structAsg' = structAsg{AST.expType = structType}

    return structAsg'

-- Check struct access
checkExpr structAcc@AST.StructAccess {AST.struct =_struct, AST.tag =_tag} = do
    -- check _struct is a struct
    -- check that _tag is part of struct _struct

    -- Get struct name
    let (strNm, scope) = case AST.expType _struct of
                    (AST.CustomType s scope') -> (s, scope')
                    _ -> ("$", -1)

    -- Get type of the struct access
    structType <- if strNm == "$"
        then
            do
               addStaticError (SE.UnmatchingTypes [AST.CustomType "<struct_type>" (-1)] (AST.expType _struct))
               return AST.TypeError
        else
            do

                -- Get current state
                State{symTable = st} <- RWS.get

                let structSym = ST.findSymbolInScope strNm scope st

                -- Check if struct symbol exists and it's a struct type.
                case structSym of
                    Just ST.Symbol{ST.symType = ST.StructType{ST.fields=_fields}} -> do

                        -- get fields that has name tag
                        let ltag = filter (\(s,_)-> s ==_tag) _fields

                        if null ltag then do
                            addStaticError . SE.SymbolNotInScope $ _tag
                            return AST.TypeError
                        else
                            return . snd . head $ ltag

                    Nothing -> do
                        addStaticError . SE.SymbolNotInScope $ strNm
                        return AST.TypeError

                    _       -> do
                        addStaticError (SE.UnmatchingTypes [AST.CustomType "<struct_type>" (-1)] (AST.expType _struct))
                        return AST.TypeError

    let structAcc' = structAcc{AST.expType = structType}

    return structAcc'

-- Check Function Call
checkExpr f@AST.FunCall {AST.fname=_fname, AST.actualArgs=_actualArgs} = do

    -- check symbol definition 
    mbSym <- checkSymbolDefined _fname

    -- Check if symbol is a valid function and get its type
    fType <- case mbSym of
        Just sym -> do
            if ST.isFunction sym || ST.isProc sym
                then if ST.isFunction sym
                    then return $ (ST.retType . ST.symType) sym
                    else return AST.TUnit

                else do
                    addStaticError $ SE.NotAValidFunction {
                        SE.symName=_fname,
                        SE.actualSymType=ST.symType sym
                    }
                    return AST.TypeError

        _ -> do
            addStaticError $ SE.SymbolNotInScope {
                SE.symName = _fname
            }
            return AST.TypeError

    -- Error on foward reference with inferred type
    M.when (fType == AST.TVoid) $ do
        addStaticError $ SE.CouldNotInferType{
            SE.symName = _fname
        }

    -- Get the types the arguments should have
    let (args, isFunc) = case mbSym of
            Just sym -> do
                if ST.isFunction sym || ST.isProc sym
                    then ((ST.args . ST.symType) sym, True)
                    else ([], False)
            _ -> ([], False)

        argsTypes = map AST.argType args

        maxNumOfArgs = length argsTypes

        minNumOfArgs = maxNumOfArgs - (length . filter (isJust . AST.defaultVal) $ args)

        numberOfArgs = length _actualArgs

    M.when (numberOfArgs > maxNumOfArgs && isFunc) $ do
        addStaticError SE.TooManyArguments{
            SE.refTo = _fname,
            SE.expectedNumOfArgs=maxNumOfArgs,
            SE.actualNumOfArgs=numberOfArgs
        }

    M.when (numberOfArgs < minNumOfArgs && isFunc) $ do
        addStaticError SE.FewArguments{
            SE.refTo = _fname,
            SE.expectedNumOfArgs=maxNumOfArgs,
            SE.actualNumOfArgs=numberOfArgs
        }

    -- Check that the types in _actualArgs match
    _ <- checkExprList argsTypes _actualArgs

    currScope <- getScope

    return f{AST.expType = fType, AST.declScope_ = currScope}

--  Check for
checkExpr f@AST.For {AST.iteratorSym = _iteratorSym, AST.step=_step, AST.start=_start, AST.end=_end, AST.cicBody=_cicBody} = do

    -- Check declaration of iterator
    case _iteratorSym of 
        AST.Variable {AST.varType=AST.TInt}   -> return ()
        AST.Variable {AST.varType=AST.TFloat} -> return ()
        s                 -> error $ "Inconsistent AST: Iterator declaration should be a numeric variable.\n\tActual declaration: " ++ show s

    -- Check that _step, _start and _end are of integer type
    _ <- checkExprList [AST.TInt, AST.TInt, AST.TInt] [_step, _start, _end]

    -- Set return type as the return type of the body
    let f' = f{AST.expType = AST.expType _cicBody}

    return f'

--  Check While
checkExpr w@AST.While {AST.cond=_cond, AST.cicBody=_cicBody} = do

    -- check condition expression is boolean
    _ <- _checkTypeMatch'' AST.TBool _cond

    -- Set return type as the return type of the body
    let w' = w{AST.expType = AST.expType _cicBody}

    return w'

--  Check if conditional expression
checkExpr i@AST.If {AST.cond=_cond, AST.accExpr=_accExpr, AST.failExpr=_failExpr} = do

    -- check condition expression is boolean
    _ <- _checkTypeMatch'' AST.TBool _cond

    -- Check _accExpr expression and _failExpr expression has the same type
    match <- _checkTypeMatch' (getCastClass $ AST.expType _accExpr) (AST.expType _failExpr)

    let ifType = if match
        then AST.expType _accExpr
        else AST.TypeError

    -- Set return type as the return type of the body
        i' = i{AST.expType = ifType}

    return i'

--  Check expression block 
checkExpr e@AST.ExprBlock {AST.exprs=_exprs} =
    return e


--  Check Declarations
checkExpr d@AST.Declaration {AST.decl=_decl} =
    return d

--  Check Binary Operation
checkExpr o@AST.Op2 {AST.op2 =_operator, AST.opr1=_opr1, AST.opr2=_opr2} = do

    -- Get expected types for the given operation
    let expecTypes = getOperationTypes _operator

    -- Check both operators have the expected types
    ok1 <- _checkTypeMatch expecTypes _opr1
    ok2 <- _checkTypeMatch expecTypes _opr2

    -- get expression type
    let opType
          | isAritmethic _operator = head $ getCastClass (AST.expType _opr1)
          | isMod _operator = AST.TInt
          | otherwise = AST.TBool

    -- Check both operators match its types
    ok3 <- _checkTypeMatch [AST.expType _opr1] _opr2

    if ok3 && ok1 && ok2
    -- Set the expression type
        then return o{AST.expType = opType}
        else return o{AST.expType = AST.TypeError}

  where
    isMod AST.Mod = True
    isMod _       = False
    isAritmethic AST.Sum  = True
    isAritmethic AST.Sub  = True
    isAritmethic AST.Mult = True
    isAritmethic AST.Div  = True
    isAritmethic _        = False

--  Check Unary Operation
checkExpr o@AST.Op1 {AST.op1=_operator, AST.opr=_opr} = do

    -- get expression type and check _opr type
    opType <- case _operator of

        AST.UnitOperator  -> return AST.TUnit

        AST.DerefOperator -> do

            ok <- _checkTypeMatch [AST.TPtr AST.TVoid] _opr
            if ok then return (AST.ptrType $ AST.expType _opr)
                else return AST.TypeError 

        _                 -> do

            let expectedTypes = getOPTypes _operator
            ok <- _checkTypeMatch expectedTypes _opr
            if ok then return (AST.expType _opr)
                else return AST.TypeError

    return o{AST.expType = opType}
  where
    getOPTypes AST.Negation      = [AST.TBool]
    getOPTypes AST.Negative      = [AST.TFloat, AST.TInt]
    getOPTypes x = error $ "Un suported Opr1 variation in getOPTypes: " ++ show x

--  Check Array Literal Expression
checkExpr a@AST.Array {AST.list=_list} = do

    State{symTable = st} <- RWS.get

    -- Get the type of the first elem
    let t  = (AST.expType . head) _list
        sz = length _list
    -- Check all the element have the same type
        homogeneous = all (typeMatch t . AST.expType) (tail _list)

    -- calculate offset
        width = ST.getTypeSize st (AST.TArray t (AST.ConstInt sz AST.TInt))
        align = ST.getTypeAlign st (AST.TArray t (AST.ConstInt sz AST.TInt))
    o <- updateOffset align width

    -- Set the expression type
    if typeMatch t AST.TypeError || not homogeneous
        then
            return a{AST.expType = AST.TypeError, AST.offset = o}
        else
            return a{AST.expType = AST.TArray t (AST.ConstInt sz AST.TInt), AST.offset = o }


--  Check Union type guessing (return a boolean)
checkExpr unionTrying@AST.UnionTrying {AST.union=_union, AST.tag=_tag} = do

    -- check _union is an union
    -- check that _tag is part of union _union

    -- Get union name
    let (unionNm, scope) = case AST.expType _union of
                    (AST.CustomType s scope') -> (s, scope')
                    _ -> ("$", -1)

    -- Get type of the union trying
    unionType <- if unionNm == "$"
        then
            do
               addStaticError (SE.UnmatchingTypes [AST.CustomType "<union_type>" (-1)] (AST.expType _union))
               return AST.TypeError
        else
            do

                -- Get current state
                State{symTable = st} <- RWS.get

                let unionSym = ST.findSymbolInScope unionNm scope st

                -- Check if struct symbol exists and it's a struct type.
                case unionSym of
                    Just ST.Symbol{ST.symType = ST.UnionType{ST.fields=_fields}} -> do

                        -- get fields that has name tag
                        let ltag = filter (\(s, _)-> s ==_tag) _fields

                        if null ltag then do
                            addStaticError . SE.SymbolNotInScope $ _tag
                            return AST.TypeError
                        else
                            return AST.TBool

                    Nothing -> do
                        addStaticError . SE.SymbolNotInScope $ unionNm
                        return AST.TypeError

                    _       -> do
                        addStaticError (SE.UnmatchingTypes [AST.CustomType "<union_type>" (-1)] (AST.expType _union))
                        return AST.TypeError

    let unionTrying' = unionTrying{AST.expType = unionType}

    return unionTrying'

--  Check Union access 
checkExpr unionUsing@AST.UnionUsing {AST.union=_union, AST.tag=_tag} = do

    -- check _union is an union
    -- check that _tag is part of union _union

    -- Get union name
    let (unionNm, scope) = case AST.expType _union of
                    AST.CustomType s scope' -> (s,scope')
                    _ -> ("$", -1)

    -- Get type of the union trying
    unionType <- if unionNm == "$"
        then
            do
               addStaticError (SE.UnmatchingTypes [AST.CustomType "<union_type>" (-1)] (AST.expType _union))
               return AST.TypeError
        else
            do

                -- Get current state
                State{symTable = st} <- RWS.get

                let unionSym = ST.findSymbolInScope unionNm scope st

                -- Check if struct symbol exists and it's a struct type.
                case unionSym of
                    Just ST.Symbol{ST.symType = ST.UnionType{ST.fields=_fields}} -> do

                        -- get fields that has name tag
                        let ltag = filter (\(s, _)-> s ==_tag) _fields

                        if null ltag then do
                            addStaticError . SE.SymbolNotInScope $ _tag
                            return AST.TypeError
                        else
                            return . snd . head $ ltag 

                    Nothing -> do
                        addStaticError . SE.SymbolNotInScope $ unionNm
                        return AST.TypeError

                    _       -> do
                        addStaticError (SE.UnmatchingTypes [AST.CustomType "<union_type>" (-1)] (AST.expType _union))
                        return AST.TypeError

    let unionUsing' = unionUsing{AST.expType = unionType}

    return unionUsing'

--  Check New Expression
checkExpr n@AST.New {AST.typeName=_type} =
    if typeMatch _type AST.TypeError
        then
            return n{AST.expType = AST.TypeError }
        else
            return n{AST.expType = AST.TPtr _type }

--  Check Delete
checkExpr d@AST.Delete {AST.ptrExpr=_ptrExpr} = do
    --check that _ptrExpr is a pointer
    case AST.expType _ptrExpr of
        AST.TPtr t -> case t of
            AST.TypeError -> do
                addStaticError $ SE.UnmatchingTypes [AST.TPtr AST.TVoid] (AST.TPtr t)
            _             -> return ()
        t -> addStaticError $ SE.UnmatchingTypes [AST.TPtr AST.TVoid] t

    return d {AST.expType = AST.TUnit}

-- Check Deref and assign to pointed 
checkExpr p@AST.DerefAssign { AST.ptrExpr=_ptrExpr, AST.value=_value} = do
    -- check that _ptrExpr is a pointer of the same type of value
    newType <- case AST.expType _ptrExpr of
        
        AST.TPtr t -> case t of

            AST.TypeError -> do
                
                addStaticError $ SE.UnmatchingTypes [AST.TPtr AST.TVoid] (AST.TPtr t)
                return AST.TypeError
            _             -> do
                ok <- _checkTypeMatch [t] _value
                if ok then return t
                    else return AST.TypeError

        t -> do
 
            addStaticError $ SE.UnmatchingTypes [AST.TPtr AST.TVoid] t
            return AST.TypeError

    return p {AST.expType = newType}

--  Check array index access
checkExpr a@AST.ArrayIndexing {AST.index=_index, AST.expr=_expr} = do
    -- check index is integer type
    _ <- _checkTypeMatch'' AST.TInt _index

    let arrExprType = AST.expType _expr
    -- check _expr is array type
    aType <- case arrExprType of
        AST.TArray {AST.arrType=_arrType} -> return _arrType
        _ -> do
            addStaticError $ SE.NonArrayExpr arrExprType
            return AST.TypeError

    -- Set expression type
    return a{AST.expType = aType}


checkExpr a@AST.ArrayAssign {AST.index=_index, AST.arrayExpr=_arrayExpr, AST.value=_value} = do
    -- check index is integer type
    _ <- _checkTypeMatch'' AST.TInt _index

    let arrExprType = AST.expType _arrayExpr
    -- check _expr is array type
    aType <- case arrExprType of
        AST.TArray {AST.arrType=_arrType} -> return _arrType
        _ -> do
            addStaticError $ SE.NonArrayExpr arrExprType
            return AST.TypeError

    -- check value match type of array
    match <- _checkTypeMatch'' aType _value

    if match
        then return a{ AST.expType = aType }
        else return a{ AST.expType = AST.TypeError }


-- Check Struct Literal
checkExpr c@AST.LiteralStruct {AST.structName=_structName, AST.list=_list} = do

    State{symTable = st} <- RWS.get

    -- Check struct name of literal struct
    mbSym <- checkSymbolDefined _structName

    -- Check if symbol is a valid struct name 
    cStructType <- case mbSym of
        Just sym -> do
            if not $ ST.isStruct sym
                then do
                    addStaticError $ SE.NotAValidStruct {
                        SE.symName=_structName,
                        SE.actualSymType=ST.symType sym
                    }
                    return AST.TypeError
                else
                    return $ AST.CustomType _structName (ST.scope sym)

        _ -> return AST.TypeError

    -- get fields types
    let fieldsTypes = case mbSym of
            Just ST.Symbol{ST.symType = ST.StructType{ST.fields=_fields}} ->
                map snd _fields
            _ -> []

        expNumOfArgs = length fieldsTypes
        actNumOfArgs = length _list

    M.when (actNumOfArgs > expNumOfArgs) $ do
        addStaticError SE.TooManyArguments{
            SE.refTo = _structName,
            SE.expectedNumOfArgs=expNumOfArgs,
            SE.actualNumOfArgs=actNumOfArgs
        }

    M.when (actNumOfArgs < expNumOfArgs) $ do
        addStaticError SE.FewArguments{
            SE.refTo = _structName,
            SE.expectedNumOfArgs=expNumOfArgs,
            SE.actualNumOfArgs=actNumOfArgs
        }

    -- Check that the types in _list match
    expListOk <- checkExprList fieldsTypes _list

    let cStructType' = if expListOk
        then cStructType
        else AST.TypeError

    -- calculate offset
        width = ST.getTypeSize st cStructType'
        align = ST.getTypeAlign st cStructType'
    o <- updateOffset align width

    let c' = c{AST.expType = cStructType', AST.offset = o}

    return c'

-- Check Union Literal
checkExpr c@AST.LiteralUnion {AST.unionName=_unionName, AST.value=_value, AST.tag=_tag} = do

    State{symTable = st} <- RWS.get

    -- Check union name of literal union
    mbSym <- checkSymbolDefined _unionName

     -- Check if symbol is a valid union name 
    cUnionType <- case mbSym of
        Just sym -> do
            if not $ ST.isUnion sym
                then do
                    addStaticError $ SE.NotAValidUnion {
                        SE.symName=_unionName,
                        SE.actualSymType=ST.symType sym
                    }
                    return AST.TypeError
                else
                    return $ AST.CustomType _unionName (ST.scope sym)

        _ -> return AST.TypeError

    let tagType = case mbSym of
            Just ST.Symbol{ST.symType = ST.UnionType{ST.fields=_fields}} ->
                map snd $ filter (\(str, _)->str==_tag) _fields
            _ -> []

    -- Check that the types in _value match the type of the tag
    tagTypeOk <- _checkTypeMatch' (concatMap getCastClass tagType) (AST.expType _value)

    let cUnionType' = if tagTypeOk && not (null tagType)
        then cUnionType
        else AST.TypeError

    -- calculate offset
        width = ST.getTypeSize st cUnionType'
        align = ST.getTypeAlign st cUnionType'
    _ <- updateOffset align width

    let c' = c{AST.expType = cUnionType'}

    return c'


-- Check String Literal
checkExpr s@AST.LiteralString {AST.expType = t} = do

    State{symTable = st} <- RWS.get

    -- calculate offset
    let  width = ST.getTypeSize st t
         align = ST.getTypeAlign st t
    o <- updateOffset align width

    return s{AST.offset = o}


    -- Check a return expression 
checkExpr r@AST.Return {AST.expr = _expr} = do

    -- Get expected type from expected types stack
    expectedType <- topType
    -- check type of expression matches the expected one
    let retType = AST.expType _expr

    matching <- _checkTypeMatch' [expectedType] retType

    -- Replace expected type when typematch found
    M.when matching $ replaceType retType

    return r

--  Check break 
checkExpr b@AST.Break {AST.expr=_expr} = do

    -- Get expected type from expected types stack
    expectedType <- topLoopType
    -- check type of expression matches the expected one
    let retType = AST.expType _expr

    matching <- _checkTypeMatch' [expectedType] retType

    -- Replace expected type when typematch found
    M.when matching $ replaceLoopType retType

    return b

--  Check continue 
checkExpr c@AST.Continue {AST.expr=_expr} = do

    -- Get expected type from expected types stack
    expectedType <- topLoopType
    -- check type of expression matches the expected one
    let retType = AST.expType _expr

    matching <- _checkTypeMatch' [expectedType] retType

    -- Replace expected type when typematch found
    M.when matching $ replaceLoopType retType

    return c

checkExpr x = return x

-- | Checks if a given type is a valid one 
checkType :: AST.Type -> ParserState AST.Type
checkType t@AST.CustomType {AST.tName=_tName, AST.scope = _scope} = do -- When it is a custom type
    State{symTable=symTb} <- RWS.get

    -- try to find symbol
    let symbol =  ST.findSymbolInScope _tName _scope symTb

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


checkType arrType@(AST.TArray _ sz) = do

    -- check type of array elements
    -- checkType t

    -- check that size of array is int
    isInt <- _checkTypeMatch'' AST.TInt sz

    if isInt
        then return arrType
        else do
            addStaticError $ SE.UnmatchingTypes{SE.expectedTypes=[AST.TInt], SE.actualType=(AST.expType sz)}
            return AST.TypeError

checkType ptr@(AST.TPtr _) =
    return ptr

checkType ref@(AST.TReference _) =
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
                ST.isConst=_isConst,
                ST.offset = 0,
                ST.staticLabel = Nothing
            }

        declToSymType AST.Reference {AST.refName=_refName} = 
            ST.Reference {
                ST.refName=_refName, 
                ST.refType=AST.TypeError, 
                ST.refScope=0, 
                ST.offset=0, 
                ST.staticLabel=Nothing 
            }


        declToSymType AST.Union {AST.fields=_fields} = ST.UnionType {ST.fields=_fields, ST.width = 0, ST.align = 0, ST.fieldScope=0}

        declToSymType AST.Struct {AST.fields=_fields} = ST.StructType {ST.fields=_fields, ST.width = 0, ST.align = 0, ST.fieldScope=0}

        declToSymType AST.Func {AST.args=_args, AST.retType=_retType, AST.body=_body, AST.baseStackSize=_fSz} =
            ST.Function {
                ST.args=_args,
                ST.retType=_retType,
                ST.body=_body,
                ST.funcSize=_fSz
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

-- | Get current scope
topScope :: ParserState ST.Scope 
topScope = RWS.get <&> (ST.stCurrScope . symTable)

-- | Utility function to check if a given name corresponds to a valid symbol, and return it if exists
checkSymbolDefined :: U.Name -> ParserState (Maybe ST.Symbol)
checkSymbolDefined name = do
    State{symTable = symTb} <- RWS.get

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

-- | Return type of given id if a valid variable or reference, return type error if none of them
getTypeOfId :: U.Name -> ParserState AST.Type
getTypeOfId name = do
    State{symTable = symTb} <- RWS.get

    case ST.findSymbol name symTb of
        Just ST.Symbol { ST.symType=ST.Variable {ST.varType=_varType} } -> return _varType
        Just ST.Symbol { ST.symType=ST.Reference { ST.refType=_refType } } -> return _refType
        _ -> return AST.TypeError


getOperationTypes :: AST.Opr2 -> [AST.Type]
getOperationTypes AST.Sum = [AST.TFloat, AST.TInt]
getOperationTypes AST.Sub = [AST.TFloat, AST.TInt]
getOperationTypes AST.Mult = [AST.TFloat, AST.TInt]
getOperationTypes AST.Div = [AST.TFloat, AST.TInt]
getOperationTypes AST.Mod = [AST.TInt]
getOperationTypes AST.Lt = [AST.TFloat, AST.TInt, AST.TBool]
getOperationTypes AST.LtEq = [AST.TFloat, AST.TInt, AST.TBool]
getOperationTypes AST.Gt = [AST.TFloat, AST.TInt, AST.TBool]
getOperationTypes AST.GtEq = [AST.TFloat, AST.TInt, AST.TBool]
getOperationTypes AST.Eq = [AST.TFloat, AST.TInt, AST.TBool, AST.TChar, AST.TPtr AST.TVoid]
getOperationTypes AST.NotEq = [AST.TFloat, AST.TInt, AST.TBool, AST.TChar, AST.TPtr AST.TVoid]
getOperationTypes AST.And = [AST.TBool]
getOperationTypes AST.Or = [AST.TBool]


-----------------------------------------------------------------------------------------------------
-- < Utility functions to check matching types > ---------------------------------------------------- 


-- Check if two types match
typeMatch :: AST.Type -> AST.Type -> Bool
typeMatch (AST.TPtr t1) (AST.TPtr t2) = typeMatch t1 t2
typeMatch (AST.TArray t1 _) (AST.TArray t2 _) = typeMatch t1 t2
typeMatch t1 t2 = t2 `elem` getCastClass t1

-- return the list of cast-able types with each other
-- that contains the given type
getCastClass :: AST.Type -> [AST.Type] 
getCastClass AST.TInt           = [AST.TInt, AST.TFloat, AST.TReference AST.TInt]
getCastClass AST.TFloat         = [AST.TFloat, AST.TInt, AST.TReference AST.TFloat]
getCastClass (AST.TReference t) = (AST.TReference t):(getCastClass t)
getCastClass t                  = [t, AST.TReference t]

-- check that the given types match the given expressions
checkExprList :: [AST.Type] -> [AST.Expr] -> ParserState Bool
checkExprList ts = _checkTypeMatchesArgs (map getCastClass ts)


-- | Check if a given expr typematchs an expected set of types, and report error if they don't
_checkTypeMatch :: [AST.Type] -> AST.Expr -> ParserState Bool
_checkTypeMatch expected  = _checkTypeMatch' expected . AST.expType

-- | Check if a given type matches some of the expected ones. Report error if not 
_checkTypeMatch' :: [AST.Type] -> AST.Type -> ParserState Bool

_checkTypeMatch' [] _                                    = return False
_checkTypeMatch' ((AST.TPtr _):_) (AST.TPtr AST.TVoid) = return True
_checkTypeMatch' ((AST.TPtr t1):_) (AST.TPtr t2)        = _checkTypeMatch' [t1] t2
_checkTypeMatch' (_:ts) (AST.TPtr t)                    = _checkTypeMatch' ts (AST.TPtr t)

_checkTypeMatch' ((AST.TArray t1 _):_) (AST.TArray t2 _) = _checkTypeMatch' [t1] t2
_checkTypeMatch' (_:ts) (AST.TArray t sz)                = _checkTypeMatch' ts (AST.TArray t sz)

_checkTypeMatch' expected exprType
    | exprType == AST.TypeError        = return False -- nothing matches TypeError
    | AST.TypeError `elem` expected    = return False -- nothing matches TypeError
    | AST.TVoid `elem` expected        = return True  -- void typematches enything
    | exprType == (AST.TPtr AST.TVoid) = return (hasTPtr expected)
    | otherwise =
    case exprType of
        AST.TypeError -> return False
        t             -> if t `elem` expected'
                            then return True
                            else do
                                addStaticError $ SE.UnmatchingTypes expected exprType
                                return False
    where
        expected' = concatMap getCastClass expected
        hasTPtr [] = False
        hasTPtr ((AST.TPtr{}):_) = True
        hasTPtr (_:_) = True

_checkTypeMatch'' :: AST.Type -> AST.Expr -> ParserState Bool
_checkTypeMatch'' t e = _checkTypeMatch [t] e

-- | Check if a sorted list of expression matches a sorted list of types
_checkTypeMatchesArgs :: [[AST.Type]] -> [AST.Expr] -> ParserState Bool
_checkTypeMatchesArgs expected args = M.zipWithM _checkTypeMatch expected args <&> and

-- | utility function to perform some operations needed before checking a function
_functionCheckerHelper  :: U.Name               -- ^ Function ID
                        -> Maybe AST.Type       -- ^ Function return type if provided 
                        -> [AST.FuncArg]        -- ^ Function args
                        -> AST.Expr             -- ^ Function Body
                        -> Int                  -- ^ Function base stack size
                        -> ParserState AST.Declaration
_functionCheckerHelper fid _ args body stackSize = do

    -- Get current type 
    inferedType <- topType
    popType

    let inferedType' = if inferedType == AST.TVoid 
                            then AST.expType body
                            else inferedType

    -- Check if body type matches expected type 
    _ <- _checkTypeMatch' [inferedType] (AST.expType body)

    checkDecls $ AST.Func fid (reverse args) inferedType' body 0 stackSize

-- | utility function to perform some operations needed before checking a for loop
_forCheckerHelper :: AST.Declaration  
                  -> AST.Expr
                  -> AST.Expr
                  -> AST.Expr
                  -> AST.Expr
                  -> AST.Type
                  -> ParserState AST.Expr
_forCheckerHelper itSym step start end body _ = do
    inferedType <- topLoopType
    popLoopType

    let inferedType' = if inferedType == AST.TVoid 
                            then AST.expType body
                            else inferedType

    -- Check if body type matches expected type 
    _ <- _checkTypeMatch' [inferedType] (AST.expType body)

    checkExpr $ AST.For itSym step start end body inferedType'

-- | utility function to perform some operations needed before checking a while loop
_whileCheckerHelper :: AST.Expr
                    -> AST.Expr
                    -> AST.Type
                    -> ParserState AST.Expr
_whileCheckerHelper cond body _ = do
    inferedType <- topLoopType
    popLoopType

    let inferedType' = if inferedType == AST.TVoid 
                            then AST.expType body
                            else inferedType

    -- Check if body type matches expected type 
    _ <- _checkTypeMatch' [inferedType] (AST.expType body)

    checkExpr $ AST.While cond body inferedType'


-- OFFSET THINGS
getOffset :: Int -> Int -> Int
getOffset currOffset align = newOffset
    where
        newOffset = ((currOffset + align - 1) `div` align) * align

getNextOffset :: Int -> Int -> Int -> Int
getNextOffset currOffset align width = newOffset
    where
        newOffset = getOffset currOffset align + width

pushOffset :: Int -> ParserState ()
pushOffset o = do
    s@State{symTable = st} <- RWS.get
    RWS.put s{symTable = ST.pushOffset st o}

popOffset :: ParserState Int 
popOffset = do
    s@State{symTable = st} <- RWS.get
    let currOffset = ST.getCurrentOffset st
    RWS.put s{symTable = ST.popOffset st}
    return currOffset

getCurrentOffset :: ParserState Int
getCurrentOffset = do
    State{symTable = st} <- RWS.get
    return $ ST.getCurrentOffset st

-- esta te devuelve el offset de la variable y actualiza el tope de la pila
updateOffset :: Int -> Int -> ParserState Int
updateOffset align width = do 
    State{symTable = st} <- RWS.get
    let currOffset = ST.getCurrentOffset st
    let ret = getOffset currOffset align
    _ <- popOffset
    pushOffset $ getNextOffset currOffset align width
    return ret