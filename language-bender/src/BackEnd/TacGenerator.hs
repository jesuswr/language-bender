{- 
    The following file contains all the functions to convert the AST data type into a TAC
    code. The TAC data types can be found in src/BackEnd/TACTypes/TAC.hs
-}
{-# OPTIONS_GHC -Wall #-}
module BackEnd.TacGenerator where

-- <Language Bender Imports> ------------------------------------
import qualified FrontEnd.AST as AST
import qualified FrontEnd.SymTable as ST
import qualified TACTypes.TAC as TAC

-- <Utility Data types> -----------------------------------------
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad     as M
import Data.Maybe(fromJust)
import Data.Functor((<&>))

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
                    { nextTemporal :: Int              -- ^ Available id' for the next temporal symbol name
                    , nextLabelTemporal :: Int         -- ^ Available id' for the next temporal label 
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
                          , currentIterData = []
                          , currentFuncData = []
                          , symT = st
                          }

-- Generate ID's and Labels

-- | get next temporal variable
getNextTemp :: GeneratorMonad Id
getNextTemp = do
    s@State{nextTemporal = n, nextLabelTemporal = _} <- RWS.get
    RWS.put s{nextTemporal = n+1}
    return $ "T" ++ show n

getNextTemp' :: String -> GeneratorMonad Id
getNextTemp' prefix = do
    t <- getNextTemp
    return $ prefix ++ "@" ++ t

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

    return $ prefix ++ "@" ++ l

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
            writeTac $ TAC.newTAC TAC.MetaStaticv (TAC.Label label) [TAC.Constant (TAC.Int size)]
            return label

-- | Get static variable address in an TAC Id
getVarStaticAddressId :: Id -> Scope -> GeneratorMonad Id
getVarStaticAddressId id' scope = do
    let varId = getTacId id' scope
    label <- getVarStaticLabel id' scope
    var_address <- getNextTemp' varId
    writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String (var_address++" := "++label++" # address of variable "++varId)) []
    writeTac $ TAC.newTAC TAC.Assign (TAC.Id var_address) [
        TAC.Label label
        ]
    return var_address

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

-- | generate an unique tac id' from a language bender id'
getTacId :: Id -> Scope -> Id
getTacId id' scope = id' ++ "@" ++ show scope

-- | Copy data from one address to another
makeCopy :: AST.Type -> Id -> Id -> Size -> GeneratorMonad ()
makeCopy type_ to from size = do
    writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String ("Copy X bytes from Y to Z")) []


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
    hasMain <- findMain
    M.when hasMain $ writeTac (TAC.newTAC TAC.Goto (TAC.Label $ getTacId "main" 0) [])
    genTacDecls $ AST.decls program

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
                            writeTac $ TAC.newTAC TAC.MetaComment (TAC.Constant $ TAC.String ("Ti[union_size - 4] := 0")) []
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

                    let varTypeSize = ST.getTypeSize st _varType

                    if _isConst || scope == 0
                        then do 
                            -- static memory case
                            -- Ti := LABEL # address of variable
                            var_address <- getVarStaticAddressId varId scope
                            
                            -- Ti[0] := valId # copy the value
                            makeCopy _varType var_address valId varTypeSize
                            return ()
                        else do
                            -- stack memory case
                            -- base [offset_var] := valId
                            let offset_var = ST.getVarOffset st varId scope
                            var_address <- getNextTemp' varId'
                            writeTac $ TAC.newTAC TAC.Add (TAC.Id var_address) [
                                TAC.Id base,
                                TAC.Constant (TAC.Int offset_var)
                                ]
                            makeCopy _varType var_address valId varTypeSize
                            return ()



-- | gen tac for references decl
genTacDecl AST.Reference{} = undefined -- not sure how to do this

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

    -- generate function label
    writeTac (TAC.newTAC TAC.MetaLabel (TAC.Label startFuncLabel) [TAC.Label startFuncLabel])

    -- Write tack for beginFunc
    writeTac $ TAC.newTAC TAC.MetaBeginFunc  stackSize' []

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
    writeTac  $ TAC.newTAC TAC.Assign (TAC.Id currId)  [TAC.Constant (TAC.Char (head val))] -- revisar esto por (head val)
    return (Just currId)

genTacExpr AST.LiteralString{AST.sVal=str} = do
    -- add static string
    strLabel <- getNextLabelTemp
    writeStatic $ TAC.newTAC TAC.MetaStaticStr (TAC.Label strLabel) [TAC.Constant (TAC.String str)]
    -- get next temporal  id' and save the const string in it
    currId <- getNextTemp
    writeTac  $ TAC.newTAC TAC.Assign (TAC.Id currId)  [TAC.Label strLabel]
    return (Just currId)

genTacExpr AST.ConstInt{AST.iVal=val} = do
    -- get next temporal id' and save the const int in it
    currId <- getNextTemp
    writeTac  $ TAC.newTAC TAC.Assign (TAC.Id currId)  [TAC.Constant (TAC.Int val)]
    return (Just currId)

genTacExpr AST.ConstFloat{AST.fVal=val} = do
    -- get next temporal id' and save the const float in it
    currId <- getNextTemp
    writeTac $ TAC.newTAC TAC.Assign (TAC.Id currId) [TAC.Constant (TAC.Float val)]
    return (Just currId)

genTacExpr AST.ConstTrue{} = do
    -- get next temporal id' and save true in it
    currId <- getNextTemp
    writeTac $ TAC.newTAC TAC.Assign (TAC.Id currId) [TAC.Constant (TAC.Bool True)]
    return (Just currId)

genTacExpr AST.ConstFalse{} = do
    -- get next temporal id' and save false in it
    currId <- getNextTemp
    writeTac $ TAC.newTAC TAC.Assign  (TAC.Id currId)  [TAC.Constant (TAC.Bool False)]
    return (Just currId)

genTacExpr AST.LiteralStruct{AST.structName=name, AST.expType=_expType} = do

    State{symT=st} <- RWS.get
    -- create an struct, save its address in temporal and return it
    let _ = ST.getTypeSize st _expType
    currId <- getNextTemp' name
    --structOffset <- getOffset ???

    --writeTac $ TAC.newTAC TAC.Assign (TAC.Id currId) [TAC.Label structLabel]
    -- iterar por fields asignando los valores a las posiciones de memoria correspondientes. TODO
    return (Just currId)

genTacExpr AST.LiteralUnion{} = undefined
genTacExpr AST.ConstUnit{} = return Nothing -- creo que esto iria asi
genTacExpr AST.ConstNull{} = undefined

genTacExpr AST.Id{AST.name=name, AST.declScope_=scope} =
    -- just return the id'@scope
    return $ Just (getTacId name scope)

genTacExpr AST.Assign{AST.variable=var, AST.value=val, AST.declScope_=scope} = do
    -- generate tac code for the expr
    Just valId <- genTacExpr val
    -- get var@scope
    let varId = getTacId var scope
    -- assign value to var and return the id' with the result
    writeTac $ TAC.newTAC TAC.Assign  (TAC.Id varId) [TAC.Id valId]
    return (Just varId)

genTacExpr AST.StructAssign{AST.struct=struct, AST.tag=tag, AST.value=value} = do
    State{symT=st} <- RWS.get
    case AST.expType struct of
        AST.CustomType name scope -> do
            -- get symbols for struct and tag, maybe struct not needed?
            let maybeStruct = ST.findSymbolInScope' name scope st
                maybeTag    = ST.findSymbolInScope' tag (scope + 1) st

            case (maybeStruct, maybeTag) of
                (Just _, Just tagSymb) -> do
                    -- generate code for struct expr and value expr
                    maybeStructId <- genTacExpr struct
                    maybeValId    <- genTacExpr value

                    case (maybeStructId, maybeValId) of
                        (Just structId, Just valId) -> do
                            -- structId[offset tag] = valId
                            let offset_ = (ST.offset . ST.symType) tagSymb
                            writeTac $ TAC.newTAC TAC.LDeref (TAC.Id structId) [ TAC.Constant (TAC.Int offset_), TAC.Id valId] 
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
            -- get symbols for struct and tag, maybe struct not needed?
            let maybeStruct = ST.findSymbolInScope' name scope st
                maybeTag    = ST.findSymbolInScope' tag (scope + 1) st

            case (maybeStruct, maybeTag) of
                (Just _, Just tagSymb) -> do
                    -- generate code for struct expr and value expr
                    maybeStructId <- genTacExpr struct

                    case maybeStructId of
                        Just structId -> do
                            -- t0 := structId[offset tag]
                            currId <- getNextTemp
                            let offset_ = (ST.offset . ST.symType) tagSymb
                            writeTac $ TAC.newTAC TAC.RDeref (TAC.Id currId) [ TAC.Id structId, TAC.Constant (TAC.Int offset_)] 
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

genTacExpr AST.FunCall {AST.fname=_fname, AST.actualArgs=_actualArgs, AST.expType=_expType, AST.declScope_=_declScope_} = do
    {-
        Function template
        # Compute every value corresponding to a function argument

        # Stack parameters
        param arg_0_return_id
        . . .
        param arg_n_return_id

        # call the function 
        ret := call function@label n
    -}

    -- Compute each argument expressions and retrieve its return labels
    mbReturnIds <- M.mapM genTacExpr _actualArgs

    -- Might crash here if some expression has no return position, this is expected. 
    -- Every expression should return something, otherwise we have an inconsistent AST
    let returnIds = map fromJust mbReturnIds 

    -- Generate return position for this function call if it does returns something
    fcallRetPos  <- getNextTemp' "freturn"

    -- Stack parameters
    M.forM_ returnIds (\s -> writeTac $ TAC.newTAC TAC.Param (TAC.Id s) [])

    -- Call function 
    -- @TODO que hago cuando no retorna nada?
    writeTac $ TAC.newTAC TAC.Call (TAC.Id fcallRetPos) [TAC.Label $ getTacId _fname _declScope_, TAC.Constant . TAC.Int . length $ _actualArgs]

    return $ Just fcallRetPos

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
    forEndLabel   <- getNextLabelTemp' "for_end"

    -- Generate needed temporals
    forResultId   <- getNextTemp' "for_result"
    mbStartResultId <- genTacExpr _start
    mbEndResultId   <- genTacExpr _end
    mbStepResultId  <- genTacExpr _step

    -- Consistency checking
    let startResultId = case mbStartResultId of
            Just startResultId' -> startResultId'
            _                  -> error "Inconsistent AST: Start expression should return some numeric value, and it's returning nothing"

    let endResultId = case mbEndResultId of
            Just endResultId'   -> endResultId'
            _                  -> error "Inconsistent AST: End expression should return some numeric value, and it's returning nothing"
    let steptResultId = case mbStepResultId of
            Just steptResultId' -> steptResultId'
            _                  -> error "Inconsistent AST: Step expression should return some numeric value, and it's returning nothing"

    -- Put start label
    writeTac $ TAC.newTAC TAC.MetaLabel  (TAC.Label forStartLabel) []

    -- Put result value
    mbOutResultId <- genTacExpr _cicBody


    -- Put End Label
    writeTac $ TAC.newTAC TAC.MetaLabel  (TAC.Label forEndLabel) []

    -- Return consistency checking
    case (mbOutResultId, _expType) of 
        (Just _, AST.TUnit) -> error "Inconsistent TAC Generator: should return nothing when body expression returns Unit"
        (Nothing, _)                  -> error "Inconsistent TAC Generator: should provide a return id' when body expression returns something different from Unit"
        (Just outResultId, _)         -> writeTac $ TAC.newTAC TAC.Assign (TAC.Id forResultId) [TAC.Id outResultId]

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
    startLabel'    <- getNextLabelTemp' "while_start"
    outLabel      <- getNextLabelTemp' "while_out"
    whileResultId <- getNextTemp

    -- Don't add a return addres if returns nothing
    let mbWhileResultId
            | _expType /= AST.TUnit = Just whileResultId
            | otherwise  = Nothing 

    let whileData = IterData {breakLabel=outLabel, continueLabel=startLabel', iterReturnId = mbWhileResultId }

    -- Put current iterator data in the state
    pushNextIterData whileData

    -- Add label marking the while start
    writeTac $ TAC.newTAC TAC.MetaLabel (TAC.Label startLabel') []

    -- Generate code for condition checking 
    Just condId <- genTacExpr _cond -- may raise error when returns Nothing, this is intended

    -- Generate code to jump to the end when condition is not met
    writeTac $ TAC.newTAC TAC.Neg  (TAC.Id condId)      [TAC.Id condId]
    writeTac $ TAC.newTAC TAC.Goif (TAC.Label outLabel) [TAC.Id condId]

    -- Generate code for body
    mbBodyResultId <- genTacExpr _cicBody 

    -- Update result if while loop has return type
    M.when (_expType /= AST.TUnit) $
        case mbBodyResultId of 
            Just bodyResultId -> writeTac $ TAC.newTAC TAC.Assign (TAC.Id whileResultId) [TAC.Id bodyResultId]
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


genTacExpr AST.If{AST.cond=cond, AST.accExpr=accExpr, AST.failExpr=failExpr, AST.expType=expType} = do
    -- get needed labels to select where to go
    elseLabel <- getNextLabelTemp' "if_else" 
    outLabel <- getNextLabelTemp'  "if_out"
    resultId <- getNextTemp        

    -- get conditional and negate it, so we can choose to go to the else
    Just condId <- genTacExpr cond
    condNegId <- getNextTemp
    writeTac $ TAC.newTAC TAC.Neg (TAC.Id condNegId) [TAC.Id condId]

    -- if negated cond is true, jump to else code
    writeTac (TAC.newTAC TAC.Goif (TAC.Label elseLabel) [TAC.Id condNegId])

    -- gen code for if 
    ifResultId <- genTacExpr accExpr

    -- if the type of the if-else its not unit, update the return type
    M.when (expType /= AST.TUnit) $
        case ifResultId of
            Just _ifResultId ->
                writeTac $ TAC.newTAC TAC.Assign (TAC.Id resultId) [TAC.Id _ifResultId]
            Nothing ->
                error "Inconsistent AST: no result for if when it does have a return type diferent from unit"

    -- jump to the out label
    writeTac (TAC.newTAC TAC.Goto (TAC.Label outLabel) [])

    -- create else label
    writeTac (TAC.newTAC TAC.MetaLabel (TAC.Label elseLabel) [])
    -- gen code for else
    elseResultId <- genTacExpr failExpr
    -- if the type of the if-else its not unit, assign the return type
    M.when (expType /= AST.TUnit) $
        case elseResultId of
            Just _elseResultId ->
                writeTac (TAC.newTAC TAC.Assign (TAC.Id resultId) [TAC.Id _elseResultId])
            Nothing ->
                error "Inconsistent AST: no result for if when it does have a return type diferent from unit"

    -- create out label
    writeTac (TAC.newTAC TAC.MetaLabel (TAC.Label outLabel) [])

    -- return result of if-else if its type its not unit
    if expType /= AST.TUnit then
        return (Just resultId)
    else
        return Nothing


genTacExpr AST.ExprBlock{AST.exprs=exprs, AST.expType=expType} = do
    -- generate code for exprs in block, if type is unit return nothing
    -- else return whatever the last expr in block returns
    maybeId <- genTacBlock exprs
    if expType == AST.TUnit then
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

genTacExpr AST.Op2{AST.op2=op, AST.opr1=l, AST.opr2=r} = do
    -- generate code for left and right exprs
    Just leftId <- genTacExpr l
    Just rightId <- genTacExpr r
    -- get temp id'
    currId <- getNextTemp
    -- assing the op to the id' and return the id'
    writeTac (TAC.TACCode (mapOp2 op) (Just (TAC.Id currId)) (Just (TAC.Id leftId)) (Just (TAC.Id rightId)))
    return (Just currId)

genTacExpr AST.Op1{AST.op1=op, AST.opr=l} = do
    -- generate code for expr
    RWS.liftIO . putStrLn $ "operator is: " ++ show op ++ ", and operand is: " ++ show l
    Just opId <- genTacExpr l
    -- if op is Unit, return nothing
    -- else assing to a temp variable and return it
    case op of
        AST.UnitOperator -> return Nothing
        _                -> do
            currId <- getNextTemp
            writeTac (TAC.TACCode (mapOp1 op) (Just (TAC.Id currId)) (Just (TAC.Id opId)) Nothing)
            return (Just currId)

genTacExpr AST.Array{} = undefined
genTacExpr AST.UnionTrying{} = undefined
genTacExpr AST.UnionUsing{} = undefined
genTacExpr AST.New {AST.typeName=_typeName} = do
    {-
        New template:
        malloc t0 size_of_type
    -}
    resultId <- getNextLabelTemp' "new_result"

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


genTacExpr AST.ArrayIndexing{} = undefined


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
mapOp1 AST.UnitOperator   = undefined -- ???

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
