{- 
    The following file contains all the functions to convert the AST data type into a TAC
    code. The TAC data types can be found in src/BackEnd/TACTypes/TAC.hs
-}
{-# OPTIONS_GHC -Wall #-}
module BackEnd.TacGenerator where

-- <Language Bender Imports> ------------------------------------
import qualified FrontEnd.AST      as AST
import qualified FrontEnd.SymTable as ST
import qualified TACTypes.TAC      as TAC
import qualified Utils.Constants   as C
import qualified FrontEnd.Utils    as U
-- <Utility Data types> -----------------------------------------
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad     as M
import Data.Maybe(fromJust)
import Data.Functor((<&>))
import Data.List(elemIndex, zip4)

-- >> Data ------------------------------------------------------

type Id = String
type Label = String
type Scope = Int
type Size = Int

-- | Data representing relevant information about the current iterator 
--   on context, such as where to go in case of a break, or in case of 
--   continue
data IterData   = IterData
                    { breakLabel :: Label           -- ^ Where to go in case of a break
                    , continueLabel :: Label        -- ^ Where to go in case of continue
                    , iterReturnId  :: Maybe Id     -- ^ Where to store the result of the loop if it returns something, 
                                                    --   some loops doesn't return anything, in which case the value is Nothing
                    } deriving(Eq, Show)

type IterDataStack = [IterData]

-- | Data representing relevant information about the current function on context
data FuncData = FuncData
                { startLabel :: Label       -- ^ Start label for this function
                , endLabel   :: Label       -- ^ End label for this function
                }

type FuncDataStack = [FuncData]

-- | Stateful information required by the conversion process
data GeneratorState = State
                    { nextTemporal :: Int              -- ^ Available id' for the next temporal symbol name (Int)
                    , nextLabelTemporal :: Int         -- ^ Available id' for the next temporal label
                    , nextFloatTemporal :: Int         -- ^ Available id' for the next temporal float
                    , currentIterData :: IterDataStack    -- ^ Stack of data for the currently running iterator
                    , currentFuncData :: FuncDataStack    -- ^ Stack of data for the currently running iterator
                    , symT :: ST.SymTable              -- ^ The symbol table
                    }

-- | Monad used to keep a state when traversing the AST to generate the code
type GeneratorMonad = RWS.RWST () ([TAC.TACCode], [TAC.TACCode]) GeneratorState IO


-- >> Handling Monad --------------------------------------------

-- | Initial Generator State
initialGenState :: ST.SymTable -> GeneratorState
initialGenState st = State{ nextTemporal = 0
                          , nextLabelTemporal = 0
                          , nextFloatTemporal = 0
                          , currentIterData = []
                          , currentFuncData = []
                          , symT = st
                          }

-- Generate ID's and Labels

-- separator
separator :: String
separator = "__"
-- separator = "@"

-- | get next temporal variable
getNextTemp :: GeneratorMonad Id
getNextTemp = do
    s@State{nextTemporal = n, nextLabelTemporal = _} <- RWS.get
    RWS.put s{nextTemporal = n+1}
    return $ "T" ++ show n

getNextTemp' :: String -> GeneratorMonad Id
getNextTemp' prefix = do
    t <- getNextTemp
    return $ "_" ++ prefix ++ separator ++ t

-- | get next label temporal variable
getNextLabelTemp :: GeneratorMonad Label
getNextLabelTemp = do
    s@State{nextTemporal = _, nextLabelTemporal = n} <- RWS.get
    RWS.put s{nextLabelTemporal = n+1}
    return $ "L" ++ show n

-- | Generate the next label with a prefix
getNextLabelTemp' :: String -> GeneratorMonad Label
getNextLabelTemp' prefix = do
    l <- getNextLabelTemp
    return $ "_" ++ prefix ++ separator ++ l

-- | get next temporal float variable
getNextFloatTemp :: GeneratorMonad Id
getNextFloatTemp = do
    s@State{nextFloatTemporal = n} <- RWS.get
    RWS.put s{nextFloatTemporal = n+1}
    return $ "f" ++ show n

getNextFloatTemp' :: String -> GeneratorMonad Id
getNextFloatTemp' prefix = do
    f <- getNextFloatTemp
    return $ "f_" ++ prefix ++ separator ++ f

-- get the correspondent kind of temporal variable
-- acording to the given type. 
getNextTypedTemp :: AST.Type -> GeneratorMonad Id
getNextTypedTemp t = do
    case t of
        AST.TFloat -> getNextFloatTemp
        _          -> getNextTemp

getNextTypedTemp' :: String -> AST.Type -> GeneratorMonad Id
getNextTypedTemp' s t = do
    case t of
        AST.TFloat -> getNextFloatTemp' s
        _          -> getNextTemp' s


-- | Get the static label of an static variable
-- | Set it if the variable does not have one already
getVarStaticLabel :: Id -> Scope -> GeneratorMonad Label
getVarStaticLabel id' scope = do
    s@State{symT=st} <- RWS.get

    let mbLabel = ST.getVarStaticLabel st id' scope

    case mbLabel of
        Just label -> return label
        Nothing    -> do
            label <- getNextLabelTemp' $ getTacId id' scope
            let st' = ST.setVarStaticLabel st id' scope label
                varType = ST.getVarType st id' scope
                size = ST.getTypeSize st varType
            RWS.put s{symT=st'}
            writeStatic $ TAC.newTAC TAC.MetaStaticv (TAC.Label label) [TAC.Constant (TAC.Int size)]
            return label

-- | Get static variable address in an TAC Id
getVarStaticAddressId :: Id -> Scope -> GeneratorMonad Id
getVarStaticAddressId id' scope = do
    --s@State{symT=st} <- RWS.get
    --let var_type = ST.getVarType st id' scope
    label <- getVarStaticLabel id' scope
    --var_address <- getNextTypedTemp' id' var_type
    var_address <- getNextTemp' id'
    writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String (var_address++" := "++label++" # address of variable "++id')) []
    writeTac $ TAC.newTAC TAC.Assign (TAC.Id var_address) [
        TAC.Label label
        ]
    return var_address

-- | Get variable stack address in an TAC Id
getVarStackAddressId :: Id -> Scope -> GeneratorMonad Id
getVarStackAddressId id' scope = do
    State{symT=st} <- RWS.get
    let offset = ST.getVarOffset st id' scope
        -- var_type = ST.getVarType st id' scope
    --var_address <- getNextTypedTemp' id' var_type
    var_address <- getNextTemp' id'
    writeTac $ TAC.newTAC TAC.Add (TAC.Id var_address) [
        TAC.Id base,
        TAC.Constant $ TAC.Int offset
        ]
    return var_address

getVarAddressId :: Id -> Scope -> GeneratorMonad Id
getVarAddressId id' scope = do
    State{symT=st} <- RWS.get
    let isConst = ST.getVarIsConst st id' scope
    if isConst || scope == 0
        then getVarStaticAddressId id' scope
        else getVarStackAddressId id' scope

-- Write TAC

-- | write TAC instruccion
writeTac :: TAC.TACCode -> GeneratorMonad ()
writeTac tacInst = do
    RWS.tell ([], [tacInst])

-- | write Static instruccion
writeStatic :: TAC.TACCode -> GeneratorMonad ()
writeStatic tacStatic = do
    RWS.tell ([tacStatic], [])

-- IterData Stack

-- | Get the id' of the next expected return value
topCurrentIterData :: GeneratorMonad IterData
topCurrentIterData = RWS.get <&> head . currentIterData

-- | Push the next expected return id'
pushNextIterData :: IterData -> GeneratorMonad ()
pushNextIterData lb = do
    s@State{ currentIterData = stk } <- RWS.get
    RWS.put s{currentIterData = lb:stk}

-- | Remove and retrieve the next id' for return
popNextIterData ::  GeneratorMonad IterData
popNextIterData = do
    s@State {  currentIterData = (x:xs) } <- RWS.get
    RWS.put s{ currentIterData = xs }
    return x

-- FuncData Stack

-- | Get the id' of the next expected return value
topCurrentFuncData :: GeneratorMonad FuncData
topCurrentFuncData = RWS.get <&> head . currentFuncData

-- | Push the next expected return id'
pushNextFuncData :: FuncData -> GeneratorMonad ()
pushNextFuncData lb = do
    s@State{ currentFuncData = stk } <- RWS.get
    RWS.put s{currentFuncData = lb:stk}

-- | Remove and retrieve the next id' for return
popNextFuncData ::  GeneratorMonad FuncData
popNextFuncData = do
    s@State {  currentFuncData = (x:xs) } <- RWS.get
    RWS.put s{ currentFuncData = xs }
    return x

-- >> Auxiliar Functions ----------------------------------------

-- | Get the current BASE id'. 
-- | BASE value is updated implicitly.
-- | Will be implemented in target code.
base :: Id
base = TAC.base

-- | Get the current STACK value
stack :: Id
stack = TAC.stack

-- | generate an unique tac id' from a language bender id'
getTacId :: Id -> Scope -> Id
getTacId id' scope = id' ++ separator ++ show scope

-- | Copy data from one address to another
makeCopy :: Id -> Id -> AST.Type -> GeneratorMonad ()
makeCopy dest_address source_value value_type = do
    State{symT=st} <- RWS.get
    writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String ("Copy from "++ show source_value ++" to " ++ show dest_address)) []
    case value_type of 
        AST.TInt            ->
            writeTac $ TAC.newTAC TAC.LDeref (TAC.Id dest_address) [TAC.Constant $ TAC.Int 0, TAC.Id source_value]
        AST.TFloat          ->
            writeTac $ TAC.newTAC TAC.LDeref (TAC.Id dest_address) [TAC.Constant $ TAC.Int 0, TAC.Id source_value]
        AST.TChar           ->
            writeTac $ TAC.newTAC TAC.LDerefb (TAC.Id dest_address) [TAC.Constant $ TAC.Int 0, TAC.Id source_value]
        AST.TBool           ->
            writeTac $ TAC.newTAC TAC.LDerefb (TAC.Id dest_address) [TAC.Constant $ TAC.Int 0, TAC.Id source_value]
        AST.TPtr _          ->
            writeTac $ TAC.newTAC TAC.LDeref (TAC.Id dest_address) [TAC.Constant $ TAC.Int 0, TAC.Id source_value]
        AST.TReference t    ->
            makeCopy dest_address source_value t
        AST.TArray _ _      ->
            writeTac $ TAC.newTAC TAC.MemCopy (TAC.Id dest_address) [TAC.Id source_value, TAC.Constant $ TAC.Int 8] -- la opcion menos mala
        AST.CustomType name scope -> do
            let Just symb = ST.findSymbolInScope' name scope st
            writeTac $ TAC.newTAC TAC.MemCopy (TAC.Id dest_address) [TAC.Id source_value, TAC.Constant $ TAC.Int $ ST.width (ST.symType symb)]
        _                   ->
            return()

isVarDecl :: AST.Declaration -> Bool
isVarDecl AST.Variable{} = True
isVarDecl _              = False

-- >> Main Function ---------------------------------------------

-- | Generate a Tac program using the symbol table and the AST object, returning the 
-- | resulting state and the generated program
generateTac :: ST.SymTable  -> AST.Program  -> IO (GeneratorState, TAC.TACProgram)
generateTac symbolTable program = do
    (genState, (tacStatic, tacCode)) <- RWS.execRWST (generateTac' program) () (initialGenState symbolTable)
    return (genState, TAC.TACProgram (tacStatic ++ tacCode))

-- | Utility function to generate the actual Tac Program.
generateTac' :: AST.Program  -> GeneratorMonad ()
generateTac' program = do
    genErrorStrings
    genTacDecls $  filter isVarDecl (AST.decls program)
    hasMain <- findMain
    if hasMain 
        then do
            writeTac $ TAC.newTAC TAC.Call (TAC.Id "___main") [TAC.Label $ getTacId "main" 0]
            writeTac $ TAC.newTAC TAC.Exit  (TAC.Constant . TAC.Int $ 0) [] 
        else 
            writeTac $ TAC.newTAC TAC.Goto (TAC.Label $ "endProgram") []
    genTacDecls $  filter (not .isVarDecl) (AST.decls program)
    writeTac $ TAC.newTAC TAC.MetaLabel (TAC.Label "endProgram") []
    --genTacStd

genErrorStrings :: GeneratorMonad()
genErrorStrings = do
    writeStatic $ TAC.newTAC TAC.MetaStaticStr (TAC.Id "___OUT_OF_BOUNDS") [TAC.Constant $ TAC.String $ "\"Avatar doesn't allow using non existing disciples" ++ "\\" ++ "n\""]
    writeStatic $ TAC.newTAC TAC.MetaStaticStr (TAC.Id "___DIVSION_BY_0") [TAC.Constant $ TAC.String $ "\"Avatar doesn't allow doing besides 0" ++ "\\" ++ "n\""]
    writeStatic $ TAC.newTAC TAC.MetaStaticStr (TAC.Id "___UNACTIVE_UNION_FIELD") [TAC.Constant $ TAC.String $ "\"Avatar doesn't allow using an energy's non-active technique" ++ "\\" ++ "n\""]

findMain :: GeneratorMonad Bool
findMain = do
    State{symT=st} <- RWS.get
    -- search in st if there is a "main" id' that is a procedure
    let foundSym = ST.findSymbolInScope' "main" 0 st

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
genTacDecl AST.Variable{AST.decName=varId, AST.initVal=val, AST.declScope=scope, AST.varType=_varType, AST.isConst=_isConst} = do

    let varId' = getTacId varId scope
    State{symT=st} <- RWS.get

    -- If the type of the declared variable is an union type
    -- then add 4 bytes of memory to keep track of 
    -- the current active field in this variable.
    case _varType of
        AST.CustomType tname tscope -> do

            let foundSym = ST.findSymbolInScope' tname tscope st

            case foundSym of
                Just ST.Symbol{ST.symType=ST.UnionType{}} -> do
                    -- Store the field active on the union
                    -- initially it value is 0 (no field is active)
                    let union_size = ST.getTypeSize st _varType

                    if _isConst || scope == 0
                        then do
                            -- static memory case
                            -- Ti := LABEL # address of union
                            union_address <- getVarStaticAddressId varId scope

                            -- Ti[union_size - 4] := 0
                            let actFieldOffset = union_size - 4
                            writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String "Ti[union_size - 4] := 0") []
                            writeTac $ TAC.newTAC TAC.LDeref (TAC.Id union_address) [
                                TAC.Constant (TAC.Int actFieldOffset),
                                TAC.Constant (TAC.Int 0)
                                ]
                        else do
                            -- stack memory case
                            -- base [offset_union + union_size - 4] := 0
                            let offset_union = ST.getVarOffset st varId scope
                            let offset_ = offset_union + union_size - 4
                            writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String ("base [offset_union + union_size - 4] := 0 -- union "++varId')) []
                            writeTac $ TAC.newTAC TAC.LDeref (TAC.Id base) [
                                TAC.Constant (TAC.Int offset_) ,
                                TAC.Constant (TAC.Int 0)
                                ]
                            return ()

                _ -> return ()

        AST.TArray t exprSz -> do
            dopeVecAddress <- getVarAddressId varId scope
            let size = ST.getTypeSize st t
            -- dopeVec[0] = stack
            writeTac $ TAC.newTAC TAC.LDeref (TAC.Id dopeVecAddress) [TAC.Constant (TAC.Int 0), TAC.Id stack]
            Just sz <- genTacExpr exprSz
            -- dopeVec[4] = sz
            writeTac $ TAC.newTAC TAC.LDeref (TAC.Id dopeVecAddress) [TAC.Constant (TAC.Int 4), TAC.Id sz]
            -- stack = stack + 4*sz
            writeTac $ TAC.newTAC TAC.Mult (TAC.Id sz) [TAC.Id sz, TAC.Constant (TAC.Int size)]
            writeTac $ TAC.newTAC TAC.Add (TAC.Id stack) [TAC.Id stack, TAC.Id sz]
        
        _ -> return ()

    case val of
        -- if there is no value to assign in the declaration do nothing
        Nothing   -> return()
        -- else create the code for the rvalue and assign it
        Just expr -> do
            maybeValId <- genTacExpr expr
            case maybeValId of
                Nothing -> return ()
                Just valId -> do

                    -- ti := var_address
                    var_address <- getVarAddressId varId scope
                    -- ti [0] := valId # copy by value
                    makeCopy var_address valId _varType
                    return ()



-- | gen tac for references decl
genTacDecl AST.Reference {AST.decName=_decName, AST.refName=_refName, AST.declScope=_declScope} = do
    {-
    Reference template:
        given a code like:
        let x = &y;
        if (x,y) is in stack, the code goes like:
            t0 = base + y_offset
            ldefer base x_offset t0
        if x in stacx, but y in .data, the code goes like:
            ldefer base x_offset y_id
        if neither x, y in stack
            ref t0 y
            assign x t0
    -}
    -- Get symbol table

    State {symT=_symT} <- RWS.get
    -- Generate required temporal 
    temp <- getNextTemp

    -- Get scope for referenced symbol
    let refcdScope = case ST.findSymbolInScope' _decName _declScope _symT of
                        Nothing -> error $ "Inconsistent AST: Variable " ++ _decName ++ " should be available in scope " ++ show _declScope
                        Just ST.Symbol {ST.symType=ST.Reference {ST.refScope=_refScope}} -> _refScope
                        _ -> error $ "Inconsistent AST: Variable " ++ _decName ++ " is expected to be a ref type"

    -- Get offset for each variable
    let refVarOffset   = ST.getVarOffset _symT _decName _declScope
        refcdVarOffset = ST.getVarOffset _symT _refName refcdScope

    -- Get position of static variables

    if _declScope == 0 && refcdScope == 0 then do
        refVarStaticAddr <- getVarStaticAddressId _decName _declScope
        refcdVarStaticAddr <- getVarStaticAddressId _refName refcdScope
        -- x[0] := &y
        writeTac $ TAC.newTAC TAC.LDeref (TAC.Id refVarStaticAddr) [TAC.Constant . TAC.Int $ 0, TAC.Id refcdVarStaticAddr]

    else if _declScope == 0 then do -- if our variable is in static memory but the referenced value isn't
        refVarStaticAddr <- getVarStaticAddressId _decName _declScope
        -- add t0 BASE y_offset
        writeTac $ TAC.newTAC TAC.Add (TAC.Id temp) [TAC.Id base, TAC.Constant . TAC.Int $ refcdVarOffset]
        -- x[0] := t0
        writeTac $ TAC.newTAC TAC.LDeref (TAC.Id refVarStaticAddr) [TAC.Constant . TAC.Int $ 0, TAC.Id temp]
    else if refcdScope == 0 then do  -- if referenced variable is in static memory
        refcdVarStaticAddr <- getVarStaticAddressId _refName refcdScope
        -- BASE[x_offset] = &y
        writeTac $ TAC.newTAC TAC.LDeref (TAC.Id TAC.base) [TAC.Constant . TAC.Int $ refVarOffset, TAC.Id refcdVarStaticAddr]
    else do -- if both are in stack 
        writeTac $ TAC.newTAC TAC.Add (TAC.Id temp) [TAC.Id base, TAC.Constant . TAC.Int $ refcdVarOffset]
        -- BASE[x_offset] = t0
        writeTac $ TAC.newTAC TAC.LDeref (TAC.Id base) [TAC.Constant . TAC.Int $ refVarOffset, TAC.Id temp]

-- | gen tac for unions decl
genTacDecl AST.Union{} = return()

-- | gen tac for structs decl
genTacDecl AST.Struct{} = return()

-- | gen tac for functions and procedure decls
genTacDecl AST.Func{AST.decName=name, AST.body=body, AST.declScope=scope, AST.baseStackSize=stackSize, AST.retType = _retType} = do
    {-
        Function definition template
        @label func_name@l0
        @function func_name@l0

        (tac code for function body)

        @label end_func_name@l1
        @endfunction func_name_end@l1
    -}
    -- fuction name label
    let startFuncLabel = getTacId name scope
        stackSize' = TAC.Constant . TAC.Int $ stackSize
    endFuncLabel <- getNextLabelTemp' $ name ++ "_end"

    let funcData = FuncData {startLabel=startFuncLabel, endLabel=endFuncLabel}

    -- Write tack for beginFunc
    writeTac $ TAC.newTAC TAC.MetaBeginFunc  (TAC.Id startFuncLabel) [stackSize']

    -- push this function as the current function in scope
    pushNextFuncData funcData

    -- generate tac code for function body
    mbReturn <- genTacExpr body

    -- Write return if any
    case (mbReturn, _retType) of
        (Nothing, AST.TUnit) -> return ()
        (Just retId, _)      -> do
            writeTac $ TAC.newTAC TAC.Return  (TAC.Id retId) []
        _ -> error "Programming error: Inconsistent AST, return addres provided on unit expression or not provided on non-unit expression"

    -- Pop current function data
    _ <- popNextFuncData

    -- generate tac label for function end

    writeTac $ TAC.newTAC TAC.MetaLabel  (TAC.Label endFuncLabel) []
    writeTac $ TAC.newTAC TAC.MetaEndFunc stackSize' []
    -- falta un return? agarrar lo que devuelva el cuerpo de la func y retornar eso?

    return ()

-------------------------------------------------------
-- | generate tac for expressions ---------------------
-- | returns the id' where the result is stored ? ------
genTacExpr :: AST.Expr -> GeneratorMonad (Maybe String)
genTacExpr AST.ConstChar{AST.cVal=val} = do
    -- get next temporal id' and save the const char in it
    currId <- getNextTemp
    writeTac  $ TAC.newTAC TAC.Assignb (TAC.Id currId)  [TAC.Constant (TAC.Char (head val))] -- revisar esto por (head val)
    return (Just currId)

genTacExpr AST.LiteralString{AST.sVal=str, AST.offset=_offset} = do

    let _list    = [(AST.ConstChar [s_i] AST.TChar) | s_i <- str]
        sz       = length str
        _expType = AST.TArray AST.TChar (AST.ConstInt sz AST.TInt)

    genTacExpr AST.Array {AST.list=_list, AST.expType=_expType, AST.offset=_offset}


genTacExpr AST.ConstInt{AST.iVal=val} = do
    -- get next temporal id' and save the const int in it
    currId <- getNextTemp
    writeTac  $ TAC.newTAC TAC.Assign (TAC.Id currId)  [TAC.Constant (TAC.Int val)]
    return (Just currId)

genTacExpr AST.ConstFloat{AST.fVal=val} = do
    -- get next temporal id' and save the const float in it
    currId <- getNextFloatTemp
    writeTac $ TAC.newTAC TAC.Assign (TAC.Id currId) [TAC.Constant (TAC.Float val)]
    return (Just currId)

genTacExpr AST.ConstTrue{} = do
    -- get next temporal id' and save true in it
    currId <- getNextTemp
    writeTac $ TAC.newTAC TAC.Assignb (TAC.Id currId) [TAC.Constant (TAC.Bool True)]
    return (Just currId)

genTacExpr AST.ConstFalse{} = do
    -- get next temporal id' and save false in it
    currId <- getNextTemp
    writeTac $ TAC.newTAC TAC.Assignb  (TAC.Id currId)  [TAC.Constant (TAC.Bool False)]
    return (Just currId)

genTacExpr AST.LiteralStruct{AST.structName=name, AST.expType=_expType, AST.list=_list, AST.offset=_offset} = do

    State{symT=st} <- RWS.get
    
    let AST.CustomType _ scope = _expType
        Just symStruct = ST.findSymbolInScope' name scope st
        fields_names = map fst $ (ST.fields . ST.symType) symStruct
        field_scope  = (ST.fieldScope . ST.symType) symStruct
    currId <- getNextTemp


    -- currId := base + _offset # Addres of struct
    writeTac $ TAC.newTAC TAC.Add (TAC.Id currId) [
        TAC.Id base,
        TAC.Constant $ TAC.Int _offset
        ]
    
    -- iterar por fields asignando los valores a las posiciones de memoria correspondientes.
    M.forM_ (zip _list fields_names) (\ (field_expr, field_name) -> do
             
            let Just tagSymb = ST.findSymbolInScope' field_name field_scope st
                fieldOffset = (ST.offset . ST.symType) tagSymb
                fieldType = (ST.varType . ST.symType) tagSymb

            Just from <- genTacExpr field_expr
            
            fieldAddress <- getNextTemp

            -- fieldAddress := struct_address + fieldOffset # Addres of struct
            writeTac $ TAC.newTAC TAC.Add (TAC.Id fieldAddress) [
                TAC.Id currId,
                TAC.Constant $ TAC.Int fieldOffset
                ]    


            makeCopy fieldAddress from fieldType
        ) 

    return (Just currId)

genTacExpr AST.LiteralUnion{AST.unionName=name, AST.tag=_tag,AST.value=_value,AST.expType=t,AST.offset=_offset} = do
    State{symT=st} <- RWS.get

    -- gen tac for value
    Just from <- genTacExpr _value

    let AST.CustomType _ scope = t
        Just symUnion = ST.findSymbolInScope' name scope st
        field_scope  = (ST.fieldScope . ST.symType) symUnion

    union_address <- getNextTemp

    writeTac $ TAC.newTAC TAC.Add (TAC.Id union_address) [
        TAC.Id base,
        TAC.Constant $ TAC.Int _offset
        ]

    let tagType = ST.getVarType st _tag field_scope

    -- base [offset] := _value
    makeCopy union_address from tagType

    return (Just union_address)

genTacExpr AST.ConstUnit{} = return Nothing 

genTacExpr AST.ConstNull{} = do
    currId <- getNextTemp
    writeTac $ TAC.newTAC TAC.Assign (TAC.Id currId) [
            TAC.Constant $ TAC.Int 0
        ]
    return (Just currId)

genTacExpr AST.Id{AST.name=name, AST.declScope_=scope, AST.expType=_expType} = do
    State{symT=st} <- RWS.get

    let typeSize = ST.getTypeSize st _expType
        Just sym = ST.findSymbolInScope' name scope st
        symType_ = ST.symType sym

    case symType_ of
        ST.Reference{ST.refName=_refName,ST.refScope=_refScope,ST.refType=_refType} -> 
            genTacExpr AST.Id{AST.name=_refName, AST.declScope_=_refScope, AST.expType=_refType, AST.position=(U.Position 0 0)}

        ST.Variable{} -> do

            let _isConst = ST.getVarIsConst st name scope
            
            case _expType of

                AST.CustomType _ _ -> do
                    -- get var address
                    var_address <- getVarAddressId name scope
                    return (Just var_address)

                AST.TArray _ _ ->  do
                    -- get var address
                    var_address <- getVarAddressId name scope
                    return (Just var_address)

                -- AST.TPtr _ -> do
                --     var_address <- getVarAddressId name scope
                --     return (Just var_address)

                t -> do 
                    -- var_address # address of variable
                    var_address <- getVarAddressId name scope
                    -- t1 := var_address [0]
                    currId <- getNextTypedTemp t

                    writeTac $ case typeSize of
                        -- assign b 
                        1 -> TAC.newTAC TAC.RDerefb (TAC.Id currId) [
                                TAC.Id var_address,
                                TAC.Constant $ TAC.Int 0
                            ]

                        -- assign w
                        4 -> TAC.newTAC TAC.RDeref (TAC.Id currId) [
                                TAC.Id var_address,
                                TAC.Constant $ TAC.Int 0
                            ]

                        _ -> error "Should not happen: Error in size of types"

                    return (Just currId) 

        _ -> error "Inconsistent AST: Expected Variable or Reference to a Variable"                           
    


genTacExpr AST.Assign{AST.variable=name, AST.value=val, AST.declScope_=scope, AST.expType=_expType} = do
    State{symT=st} <- RWS.get

    let Just sym = ST.findSymbolInScope' name scope st
        symType_ = ST.symType sym
    
    case symType_ of

        ST.Reference{ST.refName=_refName,ST.refScope=_refScope,ST.refType=_refType} -> 
            genTacExpr AST.Assign{AST.variable=_refName, AST.declScope_=_refScope, AST.expType=_refType, AST.value=val}

        ST.Variable{} -> do

            -- generate tac code for the expr
            Just valId <- genTacExpr val

            -- ti := var_address
            var_address <- getVarAddressId name scope
            -- ti [0] := valId # copy by value
            makeCopy var_address valId _expType
            return (Just valId)

        _ -> error "Inconsistent AST: Expected Variable or Reference to a Variable"


genTacExpr AST.StructAssign{AST.struct=struct, AST.tag=tag, AST.value=value, AST.expType=_expType} = do
    State{symT=st} <- RWS.get
    case AST.expType struct of
        AST.CustomType name scope -> do

            let Just symStruct= ST.findSymbolInScope' name scope st
                field_scope  = (ST.fieldScope . ST.symType) symStruct

            -- get symbols for struct and tag, maybe struct not needed?
            let maybeStruct = ST.findSymbolInScope' name scope st
                maybeTag    = ST.findSymbolInScope' tag field_scope st

            case (maybeStruct, maybeTag) of
                (Just _, Just tagSymb) -> do
                    -- generate code for struct expr and value expr
                    maybeStructId <- genTacExpr struct
                    maybeValId    <- genTacExpr value

                    case (maybeStructId, maybeValId) of
                        (Just structId, Just valId) -> do
                            -- structId[offset tag] = valId
                            tag_address <- getNextTemp
                            let offset_ = (ST.offset . ST.symType) tagSymb
                            --writeTac $ TAC.newTAC TAC.LDeref (TAC.Id structId) [ TAC.Constant (TAC.Int offset_), TAC.Id valId]
                            writeTac $ TAC.newTAC TAC.Add (TAC.Id tag_address) [
                                TAC.Id structId,
                                TAC.Constant (TAC.Int offset_)
                                ]
                            makeCopy tag_address valId _expType
                            return (Just structId)

                        -- if there is no id' with the struct or value, error
                        _             ->
                            error "Deberia darme el id' donde esta el struct"
                -- if symbols for struct or tag dont exist, error
                _ ->
                    error "Deberia existir el struct"
        _              ->
        -- if the type of the struct is not a custom type, error
            error "Aqui deberia haber un struct"


genTacExpr AST.StructAccess{AST.struct=struct, AST.tag=tag} = do
    State{symT=st} <- RWS.get
    case AST.expType struct of
        AST.CustomType name scope -> do

            let Just symStruct= ST.findSymbolInScope' name scope st
                field_scope  = (ST.fieldScope . ST.symType) symStruct

            -- get symbols for struct and tag, maybe struct not needed?
            let maybeStruct = ST.findSymbolInScope' name scope st
                maybeTag    = ST.findSymbolInScope' tag field_scope st

            case (maybeStruct, maybeTag) of
                (Just _, Just tagSymb) -> do
                    -- generate code for struct expr and value expr
                    maybeStructId <- genTacExpr struct

                    case maybeStructId of
                        Just structId -> do
                            -- t0 := structId[offset tag]
                            tag_address <- getNextTemp
                            let offset_  = (ST.offset . ST.symType) tagSymb
                                tag_type = (ST.varType . ST.symType) tagSymb 

                            writeTac $ TAC.newTAC TAC.Add (TAC.Id tag_address) [ 
                                TAC.Id structId, 
                                TAC.Constant (TAC.Int offset_)
                                ]

                            case tag_type of
                                AST.CustomType _ _ -> return (Just tag_address)

                                AST.TArray _ _ -> return (Just tag_address)

                                AST.TFloat -> do
                                    -- fi := tag_address[ 0 ]
                                    currId <- getNextFloatTemp
                                    writeTac $ TAC.newTAC TAC.RDeref (TAC.Id currId) [
                                        TAC.Id tag_address,
                                        TAC.Constant (TAC.Int 0)
                                        ]
                                    -- return fi
                                    return (Just currId)

                                AST.TInt -> do
                                    -- Ti := tag_address[ 0 ]
                                    currId <- getNextTemp
                                    writeTac $ TAC.newTAC TAC.RDeref (TAC.Id currId) [
                                        TAC.Id tag_address,
                                        TAC.Constant (TAC.Int 0)
                                        ]
                                    -- return Ti
                                    return (Just currId)

                                _ -> do -- char or bool, byte assignment
                                    -- Ti :=b tag_address[ 0 ]
                                    currId <- getNextTemp
                                    writeTac $ TAC.newTAC TAC.RDerefb (TAC.Id currId) [
                                        TAC.Id tag_address,
                                        TAC.Constant (TAC.Int 0)
                                        ]
                                    -- return Ti
                                    return (Just currId)


                        -- if there is no id' with the struct or value, error
                        _             ->
                            error "Deberia darme el id' donde esta el struct"
                -- if symbols for struct or tag dont exist, error
                _ ->
                    error "Deberia existir el struct"
        _              ->
        -- if the type of the struct is not a custom type, error
            error "Aqui deberia haber un struct"

genTacExpr AST.FunCall {AST.fname=_fname, AST.actualArgs=_actualArgs, AST.expType=_expType, AST.declScope_=_declScope_}
    | isIO _fname = genIO _fname _actualArgs
    | otherwise   = do
        State {symT=st} <- RWS.get
        let Just _func = ST.findSymbolInScope' _fname 0 st
            func       = ST.symType _func
            args       = ST.args func
            argsIdsScp = map (\f -> (AST.argName f, AST._declScope f)) args
            getOffsets c (a, b) = ST.offset $ ST.symType $ fromJust $ ST.findSymbolInScope' a b c
            argsOffs   = map (getOffsets st) argsIdsScp 
            argsTypes  = map AST.argType args

        writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String ("Code to calculate parameters")) []

        -- Compute each argument expressions and retrieve its return labels
        mbReturnIds <- M.mapM genTacExpr _actualArgs

        -- Might crash here if some expression has no return position, this is expected. 
        -- Every expression should return something, otherwise we have an inconsistent AST
        let returnIds = map fromJust mbReturnIds

        -- Generate return position for this function call if it does returns something
        fcallRetPos <- getNextTypedTemp' "freturn" _expType

        -- Generate tmp vars to save the address given by param
        varAddresses <- getNTemps $ length argsTypes

        writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String ("Push parameters")) []

        M.forM_ (zip4 returnIds argsOffs argsTypes varAddresses) (\(retId, offset, typ, varAddress) -> do 
            writeTac $ TAC.newTAC TAC.Param (TAC.Id varAddress) [TAC.Constant $ TAC.Int offset]
            makeCopy varAddress retId typ)

        -- Call function 
        writeTac $ TAC.newTAC TAC.Call (TAC.Id fcallRetPos) [TAC.Label $ getTacId _fname 0]

        M.forM_ (zip3 varAddresses argsTypes _actualArgs) (\(varAddress, argT, arg) -> do
            case argT of 
                AST.TReference t -> do
                    case arg of 
                        AST.Id name _ _ scope -> do
                            writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String ("Assign reference: " ++ name)) []
                            argAd <- getVarAddressId name scope
                            case t of 
                                AST.CustomType{} ->
                                    makeCopy argAd varAddress t
                                AST.TArray{} ->
                                    makeCopy argAd varAddress t
                                _ -> do
                                    writeTac $ TAC.newTAC TAC.RDeref (TAC.Id varAddress) [TAC.Id varAddress, TAC.Constant (TAC.Int 0)]
                                    makeCopy argAd varAddress t

                        AST.ArrayIndexing index array t2 -> do
                            writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String ("Assign reference of array access")) []
                            Just arrayAd <- genTacExpr array
                            Just ind <- genTacExpr index
                            writeTac $ TAC.newTAC TAC.RDeref (TAC.Id arrayAd) [TAC.Id arrayAd, TAC.Constant (TAC.Int 0)]
                            let typeSz = ST.getTypeSize st t
                            writeTac $ TAC.newTAC TAC.Mult (TAC.Id ind) [TAC.Id ind, TAC.Constant (TAC.Int typeSz)]
                            writeTac $ TAC.newTAC TAC.Add (TAC.Id arrayAd) [TAC.Id arrayAd, TAC.Id ind]             
                            case t2 of 
                                AST.CustomType{} ->
                                    makeCopy arrayAd varAddress t2
                                AST.TArray{} ->
                                    makeCopy arrayAd varAddress t2
                                _ -> do
                                    writeTac $ TAC.newTAC TAC.RDeref (TAC.Id varAddress) [TAC.Id varAddress, TAC.Constant (TAC.Int 0)]
                                    makeCopy arrayAd varAddress t2
                        _ ->
                            error "Expected an id to reference but got something else"
                _ -> return())

        case _expType of
            AST.TUnit -> return Nothing
            _         -> return $ Just fcallRetPos



genTacExpr AST.For {AST.iteratorSym=_iteratorSym, AST.step=_step, AST.start=_start, AST.end=_end, AST.cicBody=_cicBody, AST.expType=_expType} = do
    {-
        For template:
            # Generate code for start, end, and step expressions
            for_start@t0 := start_expr_return_id
            for_end@t1   := end_expr_return_id
            for_step@t2  := step_expr_return_id
        @label for_start@l0
            # Check that the for can start
            t0 := for_start@t0 < for_end@t0
            t1 := ! t0
            goif for_end@l1 t1

            # Generate code for iterator body
            for_result@t3 := body_result_id # only needed when the for returns something
            for_start@t0 := for_start@t0 + for_step@t2
            goto for_start@l0

        @label for_end@l1
   -}
    -- Generate needed labels
    forStartLabel <- getNextLabelTemp' "for_start"
    forStepLabel  <- getNextLabelTemp' "for_step"
    forEndLabel   <- getNextLabelTemp' "for_end"

    -- Generate needed temporals
    forResultId     <- getNextTypedTemp' "for_result" _expType
    genVoidVal forResultId _expType

    mbStartResultId <- genTacExpr _start
    mbEndResultId   <- genTacExpr _end
    mbStepResultId  <- genTacExpr _step

    State {symT=_symT} <- RWS.get

    -- Consistency checking
    let (forIterName, scope) = case _iteratorSym of
                                    AST.Variable {AST.decName=name', AST.declScope=scope'} -> (name',scope')
                                    _ -> error "For iterator should be a variable declaration"

    let forIterOffset = ST.getVarOffset _symT forIterName scope

    let startResultId = case mbStartResultId of
            Just startResultId' -> startResultId'
            _                  -> error "Inconsistent AST: Start expression should return some numeric value, and it's returning nothing"

    let endResultId = case mbEndResultId of
            Just endResultId'   -> endResultId'
            _                  -> error "Inconsistent AST: End expression should return some numeric value, and it's returning nothing"
    let stepResultId = case mbStepResultId of
            Just stepResultId' -> stepResultId'
            _                  -> error "Inconsistent AST: Step expression should return some numeric value, and it's returning nothing"


    let forIterData = IterData {
            breakLabel=forEndLabel,
            continueLabel=forStepLabel,
            iterReturnId= case _expType of
                            AST.TUnit -> Just forResultId
                            _         -> Nothing
                }
    writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String ("FOR BEGIN")) []

    -- Move initial value to for iterator
    writeTac $ TAC.newTAC TAC.LDeref (TAC.Id TAC.base) [TAC.Constant . TAC.Int $ forIterOffset, TAC.Id startResultId]

    -- Put start label
    writeTac $ TAC.newTAC TAC.MetaLabel  (TAC.Label forStartLabel) []

    -- Check if should keep iterating
    tempForCheck <- getNextTemp
    tempForIterValue <- getNextTemp' forIterName
    writeTac $ TAC.newTAC TAC.RDeref (TAC.Id tempForIterValue) [TAC.Id TAC.base, TAC.Constant . TAC.Int $ forIterOffset]
    writeTac $ TAC.newTAC TAC.Geq (TAC.Id tempForCheck) [TAC.Id tempForIterValue, TAC.Id endResultId]
    writeTac $ TAC.newTAC TAC.Goif  (TAC.Label forEndLabel) [TAC.Id tempForCheck]

    -- Add this data as iterator data
    pushNextIterData forIterData

    writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String ("FOR BODY BEGIN")) []
    -- Put result value
    mbOutResultId <- genTacExpr _cicBody
    writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String ("FOR BODY END")) []

    -- Remove iter data
    _ <- popNextIterData

    -- Put Step label 
    writeTac $ TAC.newTAC TAC.MetaLabel (TAC.Label forStepLabel) []

    -- Put Step code
    writeTac $ TAC.newTAC TAC.Add  (TAC.Id tempForIterValue) [TAC.Id tempForIterValue, TAC.Id stepResultId] -- in reverse for should be (-stepResultId)
    writeTac $ TAC.newTAC TAC.LDeref (TAC.Id TAC.base) [TAC.Constant . TAC.Int $ forIterOffset, TAC.Id tempForIterValue]

    -- Return consistency checking
    case (mbOutResultId, _expType) of
        (Nothing, AST.TUnit)   -> return ()
        (Just outResultId, _)  -> writeTac $ TAC.newTAC TAC.Assign (TAC.Id forResultId) [TAC.Id outResultId]
        _                      -> error "Inconsistent TAC Generator: should provide a return id when body expression returns something different from Unit"
    writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String ("FOR END")) []

    -- Go back to start
    writeTac $ TAC.newTAC TAC.Goto (TAC.Label forStartLabel) []

    -- Put End Label
    writeTac $ TAC.newTAC TAC.MetaLabel  (TAC.Label forEndLabel) []

    case _expType of
        AST.TUnit -> return Nothing
        _         -> return $ Just forResultId

genTacExpr AST.While {AST.cond=_cond, AST.cicBody=_cicBody, AST.expType=_expType} = do
    {-
        While template:
        @label while_start@l0:
            # code for _cond
            t0 := _cond
            t0 := ! t0
            goif while_out@l1 t0

            # code for _cicBody

            goto while_start@l0 
        @label while_out@l1:
    -}
    -- Get needed labels to select where to go
    startLabel'   <- getNextLabelTemp' "while_start"
    outLabel      <- getNextLabelTemp' "while_out"
    whileResultId <- getNextTypedTemp _expType
    genVoidVal whileResultId _expType

    -- Don't add a return addres if returns nothing
    let mbWhileResultId
            | _expType /= AST.TUnit = Just whileResultId
            | otherwise             = Nothing

    let whileData = IterData {breakLabel=outLabel, continueLabel=startLabel', iterReturnId = mbWhileResultId }

    -- Put current iterator data in the state
    pushNextIterData whileData

    -- Add label marking the while start
    writeTac $ TAC.newTAC TAC.MetaLabel (TAC.Label startLabel') []

    -- Generate code for condition checking 
    Just condId <- genTacExpr _cond -- may raise error when returns Nothing, this is intended

    -- Generate code to jump to the end when condition is not met
    writeTac $ TAC.newTAC TAC.GoifNot (TAC.Label outLabel) [TAC.Id condId]

    -- Generate code for body
    mbBodyResultId <- genTacExpr _cicBody

    State {symT=st} <- RWS.get
    let type_size = ST.getTypeSize st _expType

    -- Update result if while loop has return type
    M.when (_expType /= AST.TUnit) $
        case mbBodyResultId of
            Just bodyResultId -> case type_size of 
                1 -> writeTac $ TAC.newTAC TAC.Assignb (TAC.Id whileResultId) [TAC.Id bodyResultId]
                _ -> writeTac $ TAC.newTAC TAC.Assign (TAC.Id whileResultId) [TAC.Id bodyResultId]
            Nothing -> error $ "Inconsistent AST: Body of while returning nothing when type of while is not Unit. \n\t" ++ "Expected return type: " ++ show _expType

    -- Add code for going to the start
    writeTac $ TAC.newTAC TAC.Goto (TAC.Label startLabel') []

    -- Add goto label
    writeTac $ TAC.newTAC TAC.MetaLabel (TAC.Label outLabel) []

    -- Remove data for this iterator instruction
    _ <- popNextIterData

    -- Depending on the expression type, check what should return
    case _expType of
        AST.TUnit -> return Nothing
        _         -> return . Just $ whileResultId


genTacExpr AST.If{AST.cond=cond, AST.accExpr=accExpr, AST.failExpr=failExpr, AST.expType=_expType} = do
    -- get needed labels to select where to go
    elseLabel <- getNextLabelTemp' "if_else"
    outLabel <- getNextLabelTemp'  "if_out"
    resultId <- getNextTypedTemp _expType

    -- get conditional and negate it, so we can choose to go to the else
    Just condId <- genTacExpr cond

    -- if negated cond is true, jump to else code
    writeTac (TAC.newTAC TAC.GoifNot (TAC.Label elseLabel) [TAC.Id condId])

    writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String ("INICIO IF")) []
    -- gen code for if 
    ifResultId <- genTacExpr accExpr

    State {symT=st} <- RWS.get
    let type_size = ST.getTypeSize st _expType

    -- if the type of the if-else its not unit, update the return type
    M.when (_expType /= AST.TUnit) $
        case ifResultId of
            Just _ifResultId -> case type_size of
                1 -> writeTac $ TAC.newTAC TAC.Assignb (TAC.Id resultId) [TAC.Id _ifResultId]
                _ -> writeTac $ TAC.newTAC TAC.Assign (TAC.Id resultId) [TAC.Id _ifResultId]
            Nothing ->
                error "Inconsistent AST: no result for if when it does have a return type diferent from unit"

    -- jump to the out label
    writeTac (TAC.newTAC TAC.Goto (TAC.Label outLabel) [])

    -- create else label
    writeTac (TAC.newTAC TAC.MetaLabel (TAC.Label elseLabel) [])

    writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String ("INICIO ELSE")) []
    -- gen code for else
    elseResultId <- genTacExpr failExpr
    -- if the type of the if-else its not unit, assign the return type
    M.when (_expType /= AST.TUnit) $
        case elseResultId of
            Just _elseResultId -> case type_size of
                1 -> writeTac (TAC.newTAC TAC.Assignb (TAC.Id resultId) [TAC.Id _elseResultId])
                _ -> writeTac (TAC.newTAC TAC.Assign (TAC.Id resultId) [TAC.Id _elseResultId])
            Nothing ->
                error "Inconsistent AST: no result for if when it does have a return type diferent from unit"

    -- create out label
    writeTac (TAC.newTAC TAC.MetaLabel (TAC.Label outLabel) [])

    -- return result of if-else if its type its not unit
    if _expType /= AST.TUnit then
        return (Just resultId)
    else
        return Nothing


genTacExpr AST.ExprBlock{AST.exprs=exprs, AST.expType=_expType} = do
    -- generate code for exprs in block, if type is unit return nothing
    -- else return whatever the last expr in block returns
    maybeId <- genTacBlock exprs
    if _expType == AST.TUnit then
        return Nothing
    else
        return maybeId

genTacExpr AST.Return {AST.expr=_expr, AST.expType=_expType} = do
    {-
        Return template
        <generate code for expression>
        t0 = value, moved by value since it is just a scalar
        return t0
        goto endfunc@label
    -}
    FuncData {endLabel=_endLabel} <- topCurrentFuncData
    mbReturnVal <- genTacExpr _expr

    let tacGoToFunEnd = TAC.newTAC TAC.Goto (TAC.Label _endLabel) []

    case (mbReturnVal, _expType) of
        (Nothing, AST.TUnit) -> do
            -- If nothing to return, just go to the end of the function
            writeTac tacGoToFunEnd
            return Nothing
        (Just returnVal, _) -> do
            -- add return value
            writeTac $ TAC.newTAC TAC.Return (TAC.Id returnVal) []
            return (Just returnVal)
        _ -> error "Inconsistent TAC generation: Either expression has no return value and type is non-unit, or type is unit and expression provides value"


genTacExpr AST.Break {AST.expr=_expr, AST.expType=_expType} = do
    {-
        Template for break:
            # Code for break expresion evaluation if necessary
            iter_return_id := expr_return_id # only if type != unit
            goto iter_out@l0
    -}
    -- Get data for the current iteration to break
    IterData {breakLabel=_breakLabel, iterReturnId=_iterReturnId} <- topCurrentIterData

    case _expType of
        AST.TUnit -> return ()
        _         ->  do
            -- Generate code for expression & get the return value id'
            Just expResultId <- genTacExpr _expr    -- May crash when expression return is Nothing, this is intended

            let Just _iterReturnId' = _iterReturnId -- May crash when this iter is Nothing, this is intended

            -- Save the expression value in the iterator return value
            writeTac $ TAC.newTAC TAC.Assign (TAC.Id _iterReturnId') [TAC.Id expResultId]

    -- Go to the out label
    writeTac $ TAC.newTAC TAC.Goto  (TAC.Label _breakLabel) []

    -- Return value id'
    case (_expType, _iterReturnId) of
        (AST.TUnit, Nothing ) -> return Nothing
        (_, Just x)           -> return $ Just x
        _                     -> error "Inconsistent AST"


genTacExpr AST.Continue {AST.expr=_expr, AST.expType=_expType} = do
    {-
        Template for break:
            # Code for break expresion evaluation if necessary
            iter_return_id := expr_return_id # only if type != unit
            goto iter_start@l0
    -}
    -- Get data for the current iteration to break
    IterData {continueLabel=_continueLabel, iterReturnId=_iterReturnId} <- topCurrentIterData

    case _expType of
        AST.TUnit -> return ()
        _         ->  do
            -- Generate code for expression & get the return value id'
            Just expResultId <- genTacExpr _expr    -- May crash when expression return is Nothing, this is intended

            let Just _iterReturnId' = _iterReturnId -- May crash when this iter is Nothing, this is intended

            -- Save the expression value in the iterator return value
            writeTac $ TAC.newTAC TAC.Assign (TAC.Id _iterReturnId') [TAC.Id expResultId]

    -- Go to the out label
    writeTac $ TAC.newTAC TAC.Goto  (TAC.Label _continueLabel) []

    -- Return value id'
    case (_expType, _iterReturnId) of
        (AST.TUnit, Nothing ) -> return Nothing
        (_, Just x)           -> return $ Just x
        _                     -> error "Inconsistent AST"

genTacExpr AST.Declaration{AST.decl=d} = do
    genTacDecl d
    return Nothing

genTacExpr AST.Op2{AST.op2=op, AST.opr1=l, AST.opr2=r, AST.expType=_expType} = do
    -- generate code for left and right exprs
    Just leftId <- genTacExpr l
    Just rightId <- genTacExpr r
    -- get temp id'
    currId <- getNextTypedTemp _expType
    case op of
        AST.Div -> do
            is0 <- getNextTemp
            label <- getNextLabelTemp
            writeTac $ TAC.newTAC TAC.Eq (TAC.Id is0) [TAC.Id rightId, TAC.Constant (TAC.Int 0)]
            writeTac $ TAC.newTAC TAC.GoifNot (TAC.Label label) [TAC.Id is0]
            _tmp <- getNextTemp
            writeTac $ TAC.newTAC TAC.Assign (TAC.Id _tmp) [TAC.Label "___DIVSION_BY_0"]
            writeTac $ TAC.newTAC TAC.Print (TAC.Id _tmp) []
            writeTac $ TAC.newTAC TAC.Exit  (TAC.Constant . TAC.Int $ 1) [] 
            writeTac $ TAC.newTAC TAC.MetaLabel (TAC.Label label) []
            -- assing the op to the id' and return the id'
            writeTac (TAC.TACCode (mapOp2 op) (Just (TAC.Id currId)) (Just (TAC.Id leftId)) (Just (TAC.Id rightId)))
            return (Just currId)
        _ -> do 
            -- assing the op to the id' and return the id'
            writeTac (TAC.TACCode (mapOp2 op) (Just (TAC.Id currId)) (Just (TAC.Id leftId)) (Just (TAC.Id rightId)))
            return (Just currId)

genTacExpr AST.Op1{AST.op1=op, AST.opr=l, AST.expType=_expType} = do
    
    -- if op is Unit, return nothing
    -- else assing to a temp variable and return it
    case op of
        AST.UnitOperator -> do
            Just _ <- genTacExpr l
            return Nothing
        _                -> do
            -- generate code for expr
            Just opId <- genTacExpr l
            currId <- getNextTypedTemp _expType
            -- writeTac (TAC.TACCode (mapOp1 op) (Just (TAC.Id currId)) (Just (TAC.Id opId)) Nothing)
            writeTac $ TAC.newTAC (mapOp1 op) (TAC.Id currId) [TAC.Id opId]
            return (Just currId)

-- literal array
genTacExpr AST.Array {AST.list=_list, AST.expType=_expType, AST.offset=_offset} = do

    State {symT=st} <- RWS.get
    array_address   <- getNextTemp
    dope_vector     <- getNextTemp
    let array_size      = length _list
        array_type      = AST.arrType _expType
        array_type_size = ST.getTypeSize st array_type

    -- array_address := stack
    writeTac $ TAC.newTAC TAC.Assign (TAC.Id array_address) [TAC.Id stack]
    -- stack := stack + array_size_bytes
    writeTac $ TAC.newTAC TAC.Add (TAC.Id stack) [
        TAC.Id stack,
        TAC.Constant (TAC.Int (array_size * array_type_size))
        ]

    writeTac $ TAC.newTAC TAC.Add (TAC.Id dope_vector) [
        TAC.Id base,
        TAC.Constant (TAC.Int _offset)
        ]
    -- dope_vector[ 0 ] := array_address
    writeTac $ TAC.newTAC TAC.LDeref (TAC.Id dope_vector) [
        TAC.Constant (TAC.Int 0),
        TAC.Id array_address
        ]
    -- dope_vector[ 4 ] := array_size_n
    writeTac $ TAC.newTAC TAC.LDeref (TAC.Id dope_vector) [
        TAC.Constant (TAC.Int 4),
        TAC.Constant (TAC.Int array_size)
        ]

    let positions = [0, array_type_size..]
    -- for (val, position_in_array) in zip list positions
    M.forM_ (zip _list positions) (\ (val, position) -> do
             
            Just valId        <- genTacExpr val
            position_in_array <- getNextTemp

            writeTac $ TAC.newTAC TAC.Add (TAC.Id position_in_array) [
                TAC.Id array_address,
                TAC.Constant (TAC.Int position)
                ]
            
            makeCopy position_in_array valId array_type     
        ) 
    
    -- return dope_vector # address of dope vector
    return (Just dope_vector)


genTacExpr AST.UnionTrying {AST.union=_union, AST.tag=_tag, AST.expType = _expType} = do
    {-
        Union try template
        # Generate code for union expression
        # get union address depending on if it's on stack or in static memory
        assign t0 union_address
        
        # now t3 has the value in BASE[offset + union_szie - 4]
        rderef t3 t0 (union_size - word_size, this is known at compile time)
        eq t4 t3 variant_numer
        # return t4 as return address
    -}

    -- Get symbol table
    State {symT=_symT} <- RWS.get 

    
    -- get union data
    let (tags, unionSize) = case AST.expType _union of 
                    AST.CustomType {AST.tName=_tName, AST.scope=_scope} -> do
                        case ST.findSymbolInScope' _tName _scope _symT of 
                                Nothing -> error $ "Symbol " ++ _tName ++ " in scope " ++ show _scope ++ " should be available"

                                Just ST.Symbol {ST.symType=ST.UnionType {ST.fields=_fields, ST.width=_width}} -> (_fields, _width)

                                _ -> error $ "Symbol '" ++ _tName ++ "' in scope " ++ show _scope ++ " should be of type union, but it isn't"
                                
                    _ -> error "Inconsistent AST: Expected type for union expression should be a custom type"
        
    -- get union requested variant number
        unionVariantNum = _getTagNum _tag (map fst tags)

    -- Generate code for the union expression
    mbUnionReturnId <- genTacExpr _union

    -- get actual value and sanity check
    let unionReturnId = case mbUnionReturnId of 
                                Just id' -> id'
                                _       -> error "Inconsistent TAC generation in union trying expression. Expression with type 'union' should return a value"
    -- get temporals
    t1 <- getNextTemp
    t2 <- getNextTemp

    -- rderef t1 unionReturnId (union_size - word_size)
    writeTac $ TAC.newTAC TAC.RDeref (TAC.Id t1) [TAC.Id unionReturnId, TAC.Constant . TAC.Int $ unionSize - C.wordSize]
    -- eq t2 t1 tac_variant_num
    writeTac $ TAC.newTAC TAC.Eq  (TAC.Id t2) [TAC.Id t1, TAC.Constant . TAC.Int $ unionVariantNum] 

    -- return direction where the result is stored
    return $ Just t2


genTacExpr AST.UnionUsing {AST.union=_union, AST.tag=_tag, AST.expType = _expType} = do 
    {-
        Union Using template:
        # Generate code for union expression
        assign t0 union_expr_result

        # Check if it's a valid variant of the union
        rderef t1 t0 (union_size - word_size, known at compile time)
        eq t2 t1 (tag index in list of tags)
        goif union_sucess t2
        exit 1 # error
        @label union_success
        assign t3 t0 
    -}
    
    -- Get tags and type
    State {symT=_symT} <- RWS.get 
    --RWS.liftIO . print $ "im right before creating a copy"
    let (tags, unionSize) = case AST.expType _union of 
                    AST.CustomType {AST.tName=_tName, AST.scope=_scope} -> do
                        case ST.findSymbolInScope' _tName _scope _symT of 
                                Nothing -> error $ "Symbol " ++ _tName ++ " in scope " ++ show _scope ++ " should be available"

                                Just ST.Symbol {ST.symType=ST.UnionType {ST.fields=_fields, ST.width=_width}} -> (_fields, _width)

                                _ -> error $ "Symbol '" ++ _tName ++ "' in scope " ++ show _scope ++ " should be of type union, but it isn't"
                                
                    _ -> error "Inconsistent AST: Expected type for union expression should be a custom type"
        -- get union requested variant number
        unionVariantNum = _getTagNum _tag (map fst tags)
        unionVariantTypeSize = ST.getTypeSize _symT _expType

    -- Generate code for the union expression
    mbUnionReturnId <- genTacExpr _union

    -- get actual value and sanity check
    let unionReturnId = case mbUnionReturnId of 
                                Just id' -> id'
                                _       -> error "Inconsistent TAC generation in union trying expression. Expression with type 'union' should return a value"

    -- Temporals
    t1 <- getNextTemp
    t2 <- getNextTemp
    t3 <- getNextTypedTemp _expType

    unionSuccessLabel <- getNextLabelTemp' "union_success"

    -- rdefer t1 unionResultId (union_size - word_size)
    writeTac $ TAC.newTAC TAC.RDeref (TAC.Id t1) [TAC.Id unionReturnId, TAC.Constant . TAC.Int $ unionSize - C.wordSize]
    -- eq t2 t1 unionVariantNum
    writeTac $ TAC.newTAC TAC.Eq (TAC.Id t2) [TAC.Id t1, TAC.Constant . TAC.Int $ unionVariantNum]
    -- goif union_success t2
    writeTac $ TAC.newTAC TAC.Goif  (TAC.Label unionSuccessLabel) [TAC.Id t2]
    -- exit 1 (status code error)
    _tmp <- getNextTemp
    writeTac $ TAC.newTAC TAC.Assign (TAC.Id _tmp) [TAC.Label "___UNACTIVE_UNION_FIELD"]
    writeTac $ TAC.newTAC TAC.Print (TAC.Id _tmp) []
    writeTac $ TAC.newTAC TAC.Exit  (TAC.Constant . TAC.Int $ 1) [] 
    -- @label union_success
    writeTac $ TAC.newTAC TAC.MetaLabel (TAC.Label unionSuccessLabel) []

    case _expType of
        AST.CustomType _ _ -> return $ Just unionReturnId
        AST.TArray _ _     -> return $ Just unionReturnId
        _ -> case unionVariantTypeSize of
            4 -> do
                -- t3 := unionReturnId [ 0 ]
                writeTac $ TAC.newTAC TAC.RDeref (TAC.Id t3) [
                    TAC.Id unionReturnId,
                    TAC.Constant (TAC.Int 0)
                    ]
                -- return t3
                return (Just t3)
            _           -> do
                -- t3 := unionReturnId [ 0 ]
                writeTac $ TAC.newTAC TAC.RDerefb (TAC.Id t3) [
                    TAC.Id unionReturnId,
                    TAC.Constant (TAC.Int 0)
                    ]
                -- return t3
                return (Just t3)



genTacExpr AST.New {AST.typeName=_typeName} = do
    {-
        New template:
        malloc t0 size_of_type
    -}
    resultId <- getNextTemp' "new_result"

    -- Get symbol table to get size for given type
    State {symT=_symT} <- RWS.get
    let typeSize = ST.getTypeSize _symT _typeName

    writeTac $ TAC.newTAC TAC.Malloc (TAC.Id resultId) [TAC.Constant . TAC.Int $ typeSize]

    return $ Just resultId

genTacExpr AST.Delete {AST.ptrExpr=_ptrExpr} = do
    {-
        Delete template:
        <generate code for ptrExpr>
        t0 := expression result
        delete t0
    -}
    -- Generate tac for expression
    mbResult <- genTacExpr _ptrExpr
    case mbResult of
        Nothing -> error "Inconsistent AST: ptr expression should provide an id' with the result"
        Just result -> do
            -- Free this address
            writeTac $ TAC.newTAC TAC.Free  (TAC.Id result) []

    return Nothing


genTacExpr AST.DerefAssign {AST.ptrExpr=_ptrExpr, AST.value=_value, AST.expType=_expType } = do
    
    Just valId <- genTacExpr _value
    Just ptrId <- genTacExpr _ptrExpr
    makeCopy ptrId valId _expType

    return (Just ptrId)

genTacExpr AST.ArrayIndexing{AST.index=_index, AST.expr = array_expr, AST.expType=_expType} = do

    -- array indexing : a [ i ]
    -- find (a + i * array_type_size) position

    State {symT=st} <- RWS.get
    let array_type_size = ST.getTypeSize st _expType

    Just dope_vector   <- genTacExpr array_expr
    Just array_index   <- genTacExpr _index
    array_address      <- getNextTemp
    array_size         <- getNextTemp
    array_position     <- getNextTemp
    isNegative         <- getNextTemp
    isOverSize         <- getNextTemp
    isOutOfBounds      <- getNextTemp
    array_index_succes <- getNextLabelTemp

    writeTac $ TAC.newTAC TAC.RDeref (TAC.Id array_address) [
        TAC.Id dope_vector,
        TAC.Constant (TAC.Int 0)
        ]
    writeTac $ TAC.newTAC TAC.RDeref (TAC.Id array_size) [
        TAC.Id dope_vector,
        TAC.Constant (TAC.Int 4)
        ]

    -- check index is whitin the limits of the array
    writeTac $ TAC.newTAC TAC.Lt (TAC.Id isNegative) [TAC.Id array_index, TAC.Constant (TAC.Int 0)]
    writeTac $ TAC.newTAC TAC.Geq (TAC.Id isOverSize) [TAC.Id array_index, TAC.Id array_size]
    writeTac $ TAC.newTAC TAC.Or (TAC.Id isOutOfBounds) [TAC.Id isNegative, TAC.Id isOverSize]
    writeTac $ TAC.newTAC TAC.GoifNot (TAC.Label array_index_succes) [TAC.Id isOutOfBounds]
    writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String ("EXIT POR OUT OF BOUNDS")) []
    _tmp <- getNextTemp
    writeTac $ TAC.newTAC TAC.Assign (TAC.Id _tmp) [TAC.Label "___OUT_OF_BOUNDS"]
    writeTac $ TAC.newTAC TAC.Print (TAC.Id _tmp) []
    writeTac $ TAC.newTAC TAC.Exit (TAC.Constant . TAC.Int $ 1) []
    writeTac $ TAC.newTAC TAC.MetaLabel (TAC.Label array_index_succes) []

    -- i * array_type_size
    writeTac $ TAC.newTAC TAC.Mult (TAC.Id array_position) [
        TAC.Id array_index,
        TAC.Constant (TAC.Int array_type_size)
        ]
    -- (a + i * array_type_size)
    writeTac $ TAC.newTAC TAC.Add (TAC.Id array_position) [
        TAC.Id array_position,
        TAC.Id array_address
        ]

    -- return value at that position
    case _expType of
        AST.TArray{} -> return (Just array_position)
        AST.CustomType{} -> return (Just array_position)
        _ -> case array_type_size of
            4 -> do
                currId <- getNextTemp
                writeTac $ TAC.newTAC TAC.RDeref (TAC.Id currId) [
                    TAC.Id array_position,
                    TAC.Constant (TAC.Int 0)
                    ]
                return (Just currId)
            1 -> do
                currId <- getNextTemp
                writeTac $ TAC.newTAC TAC.RDerefb (TAC.Id currId) [
                    TAC.Id array_position,
                    TAC.Constant (TAC.Int 0)
                    ]
                return (Just currId)
            _ -> error "Should not happen: size of type error"



genTacExpr AST.ArrayAssign {AST.index=_index, AST.arrayExpr=array_expr, AST.value=_value, AST.expType=_expType} = do

    -- array assign: a [ i ] := value
    State {symT=st} <- RWS.get
    let array_type_size = ST.getTypeSize st _expType
    Just dope_vector   <- genTacExpr array_expr
    Just array_index   <- genTacExpr _index
    Just valId         <- genTacExpr _value
    array_address      <- getNextTemp
    array_size         <- getNextTemp
    array_position     <- getNextTemp
    isNegative         <- getNextTemp
    isOverSize         <- getNextTemp
    isOutOfBounds      <- getNextTemp
    array_index_succes <- getNextLabelTemp

    writeTac $ TAC.newTAC TAC.RDeref (TAC.Id array_address) [
        TAC.Id dope_vector,
        TAC.Constant (TAC.Int 0)
        ]
    writeTac $ TAC.newTAC TAC.RDeref (TAC.Id array_size) [
        TAC.Id dope_vector,
        TAC.Constant (TAC.Int 4)
        ]

    -- check index is whitin the limits of the array
    writeTac $ TAC.newTAC TAC.Lt (TAC.Id isNegative) [TAC.Id array_index, TAC.Constant (TAC.Int 0)]
    writeTac $ TAC.newTAC TAC.Geq (TAC.Id isOverSize) [TAC.Id array_index, TAC.Id array_size]
    writeTac $ TAC.newTAC TAC.Or (TAC.Id isOutOfBounds) [TAC.Id isNegative, TAC.Id isOverSize]
    writeTac $ TAC.newTAC TAC.GoifNot (TAC.Label array_index_succes) [TAC.Id isOutOfBounds]
    writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String ("EXIT POR OUT OF BOUNDS")) []
    _tmp <- getNextTemp
    writeTac $ TAC.newTAC TAC.Assign (TAC.Id _tmp) [TAC.Label "___OUT_OF_BOUNDS"]
    writeTac $ TAC.newTAC TAC.Print (TAC.Id _tmp) []
    writeTac $ TAC.newTAC TAC.Exit (TAC.Constant . TAC.Int $ 1) []
    writeTac $ TAC.newTAC TAC.MetaLabel (TAC.Label array_index_succes) []

    -- i * array_type_size
    writeTac $ TAC.newTAC TAC.Mult (TAC.Id array_position) [
        TAC.Id array_index,
        TAC.Constant (TAC.Int array_type_size)
        ]
    -- (a + i * array_type_size)
    writeTac $ TAC.newTAC TAC.Add (TAC.Id array_position) [
        TAC.Id array_position,
        TAC.Id array_address
        ]

    makeCopy array_position valId _expType

    -- return value at that position
    case _expType of
        AST.TArray{} -> return (Just array_position)
        AST.CustomType{} -> return (Just array_position)
        _ -> return (Just valId)
            -- case array_type_size of
            -- 4 -> do
            --     currId <- getNextTemp
            --     writeTac $ TAC.newTAC TAC.RDeref (TAC.Id currId) [
            --         TAC.Id array_position,
            --         TAC.Constant (TAC.Int 0)
            --         ]
            --     return (Just currId)
            -- 1 -> do
            --     currId <- getNextTemp
            --     writeTac $ TAC.newTAC TAC.RDerefb (TAC.Id currId) [
            --         TAC.Id array_position,
            --         TAC.Constant (TAC.Int 0)
            --         ]
            --     return (Just currId)


-- gen code for a block, return the last id' (or Nothing), because
-- the value of a block is the value of the last expr in it
genTacBlock :: [AST.Expr] -> GeneratorMonad (Maybe String)
genTacBlock []  = return Nothing
genTacBlock [e] = genTacExpr e
genTacBlock (e:es) = do
    _ <- genTacExpr e
    genTacBlock es


-- map ast bin ops to tac operations
mapOp2 :: AST.Opr2 -> TAC.Operation
mapOp2 AST.Sum   = TAC.Add
mapOp2 AST.Sub   = TAC.Sub
mapOp2 AST.Mult  = TAC.Mult
mapOp2 AST.Div   = TAC.Div
mapOp2 AST.Mod   = TAC.Mod
mapOp2 AST.Lt    = TAC.Lt
mapOp2 AST.LtEq  = TAC.Leq
mapOp2 AST.Gt    = TAC.Gt
mapOp2 AST.GtEq  = TAC.Geq
mapOp2 AST.Eq    = TAC.Eq
mapOp2 AST.NotEq = TAC.Neq
mapOp2 AST.And   = TAC.And
mapOp2 AST.Or    = TAC.Or

-- map ast unary ops to tac operations
mapOp1 :: AST.Opr1 -> TAC.Operation
mapOp1 AST.Negation       = TAC.Neg
mapOp1 AST.Negative       = TAC.Minus
mapOp1 AST.DerefOperator  = TAC.Deref
mapOp1 AST.UnitOperator   = error "The unit operator has no tac representation. you shouldn't be asking for it"

--------------------------------------------------------
-- >> Utils --------------------------------------------

-- | Util function to get the position of a string in a list of strings, useful to find the position of a union variant
_getTagNum :: String -> [String] -> Int
_getTagNum s ss = case elemIndex s ss of 
                        Nothing -> error $ "Error, name '"++ s ++"' not in list of names"
                        Just x  -> x

getNTemps :: Int -> GeneratorMonad ([Id])
getNTemps 0 = return []
getNTemps n = do
    tmp <- getNextTemp
    others <- getNTemps (n-1)
    return $ tmp:others

isIO :: String -> Bool
isIO "readair" = True
isIO "printair" = True
isIO "readwater" = True
isIO "printwater" = True
isIO "readfire" = True
isIO "printfire" = True
isIO "readearth" = True
isIO "printearth" = True
isIO "readmetal" = True
isIO "printmetal" = True
isIO _ = False

mapIO :: String -> TAC.Operation
mapIO "readair" = TAC.Readi
mapIO "printair" = TAC.Printi
mapIO "readwater" = TAC.Readf
mapIO "printwater" = TAC.Printf
mapIO "readfire" = TAC.Readi
mapIO "printfire" = TAC.Printi
mapIO "readearth" = TAC.Readc
mapIO "printearth" = TAC.Printc
mapIO "readmetal" = TAC.Read
mapIO "printmetal" = TAC.Print
mapIO _ = error $ "Error in mapIO"

genIO :: String -> [AST.Expr] -> GeneratorMonad(Maybe Id)
genIO ('r':_) [AST.StructAccess struct tag _expType] = do
    State{symT=st} <- RWS.get
    case AST.expType struct of
        AST.CustomType name scope -> do

            let Just symStruct= ST.findSymbolInScope' name scope st
                field_scope  = (ST.fieldScope . ST.symType) symStruct

            -- get symbols for struct and tag, maybe struct not needed?
            let maybeStruct = ST.findSymbolInScope' name scope st
                maybeTag    = ST.findSymbolInScope' tag field_scope st

            case (maybeStruct, maybeTag) of
                (Just _, Just tagSymb) -> do
                    -- generate code for struct expr and value expr
                    maybeStructId <- genTacExpr struct

                    case maybeStructId of
                        Just structId -> do
                            -- t0 := structId[offset tag]
                            tag_address <- getNextTemp
                            let offset_  = (ST.offset . ST.symType) tagSymb
                                tag_type = (ST.varType . ST.symType) tagSymb 

                            writeTac $ TAC.newTAC TAC.Add (TAC.Id tag_address) [ 
                                TAC.Id structId, 
                                TAC.Constant (TAC.Int offset_)
                                ]

                            case tag_type of
                                AST.TInt -> do
                                    -- Ti := tag_address[ 0 ]
                                    _tmp <- getNextTypedTemp tag_type
                                    writeTac $ TAC.newTAC TAC.Readi (TAC.Id _tmp) []
                                    makeCopy tag_address _tmp tag_type
                                    return Nothing
                                AST.TFloat -> do
                                    -- Ti := tag_address[ 0 ]
                                    _tmp <- getNextTypedTemp tag_type
                                    writeTac $ TAC.newTAC TAC.Readf (TAC.Id _tmp) []
                                    makeCopy tag_address _tmp tag_type
                                    return Nothing
                                AST.TBool -> do
                                    -- Ti := tag_address[ 0 ]
                                    _tmp <- getNextTypedTemp tag_type
                                    writeTac $ TAC.newTAC TAC.Readi (TAC.Id _tmp) []
                                    makeCopy tag_address _tmp tag_type
                                    return Nothing
                                AST.TChar -> do
                                    -- Ti := tag_address[ 0 ]
                                    _tmp <- getNextTypedTemp tag_type
                                    writeTac $ TAC.newTAC TAC.Readc (TAC.Id _tmp) []
                                    makeCopy tag_address _tmp tag_type
                                    return Nothing
                                _ ->
                                    error $ "no deberia llegar aqui"

                        -- if there is no id' with the struct or value, error
                        _             ->
                            error "Deberia darme el id' donde esta el struct"
                -- if symbols for struct or tag dont exist, error
                _ ->
                    error "Deberia existir el struct"
        _              ->
        -- if the type of the struct is not a custom type, error
            error "Aqui deberia haber un struct"


genIO s@"printmetal" [arg] = do
    Just dopeVecA <- genTacExpr arg
    stringA <- getNextTemp
    writeTac $ TAC.newTAC TAC.RDeref (TAC.Id stringA) [TAC.Id dopeVecA, TAC.Constant $ TAC.Int 0]
    writeTac $ TAC.newTAC (mapIO s) (TAC.Id stringA) []
    return Nothing

genIO s@('p':_) [arg] = do
    Just varId <- genTacExpr arg
    writeTac $ TAC.newTAC (mapIO s) (TAC.Id varId) []
    return Nothing

genIO s@"readmetal" [AST.Id name _ _ scope] = do
    dopeVecA <- getVarAddressId name scope
    stringA <- getNextTemp
    -- direccion del arreglo = direccion del dope vector [0]
    writeTac $ TAC.newTAC TAC.RDeref (TAC.Id stringA) [TAC.Id dopeVecA, TAC.Constant $ TAC.Int 0]
    writeTac $ TAC.newTAC (mapIO s) (TAC.Id stringA) []
    return Nothing

genIO s [AST.Id name _ t scope] = do
    tmp <- getNextTypedTemp t
    writeTac $ TAC.newTAC (mapIO s) (TAC.Id tmp) []
    varAddress <- getVarAddressId name scope
    makeCopy varAddress tmp t
    return Nothing

genIO s [AST.ArrayIndexing _index array_expr _expType] = do
    State {symT=st} <- RWS.get
    tmp <- getNextTypedTemp _expType
    writeTac $ TAC.newTAC (mapIO s) (TAC.Id tmp) []
    let array_type_size = ST.getTypeSize st _expType
    Just dope_vector   <- genTacExpr array_expr
    Just array_index   <- genTacExpr _index
    array_address      <- getNextTemp
    array_size         <- getNextTemp
    array_position     <- getNextTemp
    isNegative         <- getNextTemp
    isOverSize         <- getNextTemp
    isOutOfBounds      <- getNextTemp
    array_index_succes <- getNextLabelTemp

    writeTac $ TAC.newTAC TAC.RDeref (TAC.Id array_address) [
        TAC.Id dope_vector,
        TAC.Constant (TAC.Int 0)
        ]
    writeTac $ TAC.newTAC TAC.RDeref (TAC.Id array_size) [
        TAC.Id dope_vector,
        TAC.Constant (TAC.Int 4)
        ]

    -- check index is whitin the limits of the array
    writeTac $ TAC.newTAC TAC.Lt (TAC.Id isNegative) [TAC.Id array_index, TAC.Constant (TAC.Int 0)]
    writeTac $ TAC.newTAC TAC.Geq (TAC.Id isOverSize) [TAC.Id array_index, TAC.Id array_size]
    writeTac $ TAC.newTAC TAC.Or (TAC.Id isOutOfBounds) [TAC.Id isNegative, TAC.Id isOverSize]
    writeTac $ TAC.newTAC TAC.GoifNot (TAC.Label array_index_succes) [TAC.Id isOutOfBounds]
    writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String ("EXIT POR OUT OF BOUNDS")) []
    _tmp <- getNextTemp
    writeTac $ TAC.newTAC TAC.Assign (TAC.Id _tmp) [TAC.Label "___OUT_OF_BOUNDS"]
    writeTac $ TAC.newTAC TAC.Print (TAC.Id _tmp) []
    writeTac $ TAC.newTAC TAC.Exit (TAC.Constant . TAC.Int $ 1) []
    writeTac $ TAC.newTAC TAC.MetaLabel (TAC.Label array_index_succes) []

    -- i * array_type_size
    writeTac $ TAC.newTAC TAC.Mult (TAC.Id array_position) [
        TAC.Id array_index,
        TAC.Constant (TAC.Int array_type_size)
        ]
    -- (a + i * array_type_size)
    writeTac $ TAC.newTAC TAC.Add (TAC.Id array_position) [
        TAC.Id array_position,
        TAC.Id array_address
        ]             
    makeCopy array_position tmp _expType
    return Nothing

genIO _ _ = do
    error "Error in genIO"


genVoidVal :: Id -> AST.Type -> GeneratorMonad()
genVoidVal resId expType =
    case expType of 
        AST.TFloat ->
            writeTac $ TAC.newTAC TAC.Assign (TAC.Id resId) [TAC.Constant $ TAC.Float 0.0]
        AST.TInt   ->
            writeTac $ TAC.newTAC TAC.Assign (TAC.Id resId) [TAC.Constant $ TAC.Int 0]
        AST.TChar   ->
            writeTac $ TAC.newTAC TAC.Assignb (TAC.Id resId) [TAC.Constant $ TAC.Char '$']
        AST.TBool   ->
            writeTac $ TAC.newTAC TAC.Assignb (TAC.Id resId) [TAC.Constant $ TAC.Bool False]
        AST.TArray _ _ -> do
            writeTac $ TAC.newTAC TAC.Assign (TAC.Id resId) [TAC.Id stack]
            writeTac $ TAC.newTAC TAC.Add (TAC.Id stack) [TAC.Id stack, TAC.Constant $ TAC.Int 8]
            writeTac $ TAC.newTAC TAC.RDeref (TAC.Id resId) [TAC.Constant $ TAC.Int 0, TAC.Id resId] -- ponerle una direccion dentro del stack para evitar errores (igual no se usara por el tamano 0)
            writeTac $ TAC.newTAC TAC.RDeref (TAC.Id resId) [TAC.Constant $ TAC.Int 4, TAC.Constant $ TAC.Int 0] -- ponerle tamano 0
        AST.TPtr _ ->
            writeTac $ TAC.newTAC TAC.Assign (TAC.Id resId) [TAC.Constant $ TAC.Int 0]
        AST.TReference t ->
            genVoidVal resId t
        s@(AST.CustomType _ _) -> do
            State {symT=st} <- RWS.get
            let sz = ST.getTypeSize st s
            writeTac $ TAC.newTAC TAC.Assign (TAC.Id resId) [TAC.Id stack]
            writeTac $ TAC.newTAC TAC.Add (TAC.Id stack) [TAC.Id stack, TAC.Constant $ TAC.Int sz]
        _ ->
            return ()