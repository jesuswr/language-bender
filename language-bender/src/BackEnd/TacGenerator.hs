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
import Data.Maybe(fromJust)
import Data.Functor((<&>))

-- >> Data ------------------------------------------------------

type Id = String
type Label = String

-- | Data representing relevant information about the current iterator 
--   on context, such as where to go in case of a break, or in case of 
--   continue
data IterData   = IterData 
                    { breakLabel :: Label           -- ^ Where to go in case of a break
                    , continueLabel :: Label        -- ^ Where to go in case of continue
                    , iterReturnId  :: Maybe Id     -- ^ Where to store the result of the loop if it returns something, 
                                                    --   some loops doesn't return anything, in which case the value is Nothing
                    }
-- | Stateful information required by the conversion process
data GeneratorState = State
                    { nextTemporal :: Int             -- ^ Available id for the next temporal symbol name
                    , nextLabelTemporal :: Int        -- ^ Available id for the next temporal label 
                    , currentIterData :: [IterData]   -- ^ Stack of data for the currently running iterator
                    , symT :: ST.SymTable
                    }

-- | Monad used to keep a state when traversing the AST to generate the code
type GeneratorMonad = RWS.RWST () [TAC.TACCode] GeneratorState IO


-- >> Handling Monad --------------------------------------------

-- | get next temporal variable
getNextTemp :: GeneratorMonad String
getNextTemp = do
    s@State{nextTemporal = n, nextLabelTemporal = m} <- RWS.get
    RWS.put s{nextTemporal = n+1}
    return $ "T" ++ show n

getNextTemp' :: String -> GeneratorMonad String
getNextTemp' prefix = do
    t <- getNextTemp
    return $ prefix ++ "@" ++ t

-- | get next label temporal variable
getNextLabelTemp :: GeneratorMonad String
getNextLabelTemp = do
    s@State{nextTemporal = m, nextLabelTemporal = n} <- RWS.get
    RWS.put s{nextLabelTemporal = n+1}
    return $ "L" ++ show n

-- | Generate the next label with a prefix
getNextLabelTemp' :: String -> GeneratorMonad String
getNextLabelTemp' prefix = do
    l <- getNextLabelTemp

    return $ prefix ++ "@" ++ l

-- | write TAC instruccion
writeTac :: TAC.TACCode -> GeneratorMonad ()
writeTac tacInst = do
    RWS.tell [tacInst]

-- | Initial Generator State
initialGenState :: ST.SymTable -> GeneratorState
initialGenState st = State{nextTemporal = 0, nextLabelTemporal = 0, currentIterData = [], symT = st}

-- | Get the id of the next expected return value
topCurrentIterData :: GeneratorMonad IterData
topCurrentIterData = RWS.get <&> head . currentIterData 

-- | Push the next expected return id
pushNextIterData :: IterData -> GeneratorMonad ()
pushNextIterData lb = do
    s@State{ currentIterData = stk } <- RWS.get 
    RWS.put s{currentIterData = lb:stk}

-- | Remove and retrieve the next id for return
popNextIterData ::  GeneratorMonad IterData
popNextIterData = do 
    s@State {  currentIterData = (x:xs) } <- RWS.get 
    RWS.put s{ currentIterData = xs }
    return x


-- >> Auxiliar Functions ----------------------------------------

-- | generate an unique tac id from a language bender id
getTacId :: String -> Int -> String
getTacId id scope = id ++ "@" ++ show scope

-- >> Main Function ---------------------------------------------

-- | Generate a Tac program using the symbol table and the AST object, returning the 
-- | resulting state and the generated program
generateTac :: ST.SymTable  -> AST.Program  -> IO (GeneratorState, TAC.TACProgram)
generateTac symbolTable program = do
    (genState, tacCode) <- RWS.execRWST (generateTac' symbolTable program) () (initialGenState symbolTable)
    return (genState, TAC.TACProgram tacCode)

-- | Utility function to generate the actual Tac Program.
generateTac' :: ST.SymTable  -> AST.Program  -> GeneratorMonad ()
generateTac' symT program = do
    hasMain <- findMain symT
    M.when hasMain $ writeTac (TAC.TACCode TAC.Goto (Just (TAC.LVLabel $ getTacId "main" 0)) Nothing Nothing)
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
genTacDecl AST.Variable{AST.decName=varId, AST.initVal=val, AST.declScope=scope} =
    case val of 
        -- if there is no value to assign in the declaration do nothing
        Nothing   -> return()
        -- else create the code for the rvalue and assign it
        Just expr -> do
            maybeValId <- genTacExpr expr
            case maybeValId of
                Nothing -> return ()
                Just valId -> do
                    writeTac $ TAC.newTAC TAC.Assign  (TAC.LVId varId) [TAC.RVId valId]
                    return ()


-- | gen tac for references decl
genTacDecl AST.Reference{} = undefined -- not sure how to do this

-- | gen tac for unions decl
genTacDecl AST.Union{} = return()

-- | gen tac for structs decl
genTacDecl AST.Struct{} = return()

-- | gen tac for functions and procedure decls
genTacDecl AST.Func{AST.decName=name, AST.body=body, AST.declScope=scope} = do
    -- fuction name label
    writeTac (TAC.TACCode TAC.MetaLabel (Just (TAC.LVLabel (getTacId name scope))) Nothing Nothing)
    -- generate tac code for function body
    genTacExpr body
    -- falta un return? agarrar lo que devuelva el cuerpo de la func y retornar eso?
    return()

-------------------------------------------------------
-- | generate tac for expressions ---------------------
-- | returns the id where the result is stored ? ------
genTacExpr :: AST.Expr -> GeneratorMonad (Maybe String)
genTacExpr AST.ConstChar{AST.cVal=val} = do
    -- get next temporal id and save the const char in it
    currId <- getNextTemp
    writeTac  $ TAC.newTAC TAC.Assign (TAC.LVId currId)  [TAC.Constant (TAC.Char (head val))] -- revisar esto por (head val)
    return (Just currId)
genTacExpr AST.ConstString{} = undefined

genTacExpr AST.ConstInt{AST.iVal=val} = do
    -- get next temporal id and save the const int in it
    currId <- getNextTemp
    writeTac  $ TAC.newTAC TAC.Assign (TAC.LVId currId)  [TAC.Constant (TAC.Int val)]
    return (Just currId)

genTacExpr AST.ConstFloat{AST.fVal=val} = do
    -- get next temporal id and save the const float in it
    currId <- getNextTemp
    writeTac $ TAC.newTAC TAC.Assign (TAC.LVId currId) [TAC.Constant (TAC.Float val)]
    return (Just currId)

genTacExpr AST.ConstTrue{} = do
    -- get next temporal id and save true in it
    currId <- getNextTemp
    writeTac $ TAC.newTAC TAC.Assign (TAC.LVId currId) [TAC.Constant (TAC.Bool True)]
    return (Just currId)

genTacExpr AST.ConstFalse{} = do
    -- get next temporal id and save false in it
    currId <- getNextTemp
    writeTac $ TAC.newTAC TAC.Assign  (TAC.LVId currId)  [TAC.Constant (TAC.Bool False)]
    return (Just currId)

genTacExpr AST.ConstStruct{} = undefined
genTacExpr AST.ConstUnion{} = undefined
genTacExpr AST.ConstUnit{} = return Nothing -- creo que esto iria asi
genTacExpr AST.ConstNull{} = undefined
genTacExpr AST.Id{AST.name=name, AST.declScope_=scope} =
    -- just return the id@scope
    return $ Just (getTacId name scope)

genTacExpr AST.Assign{AST.variable=var, AST.value=val, AST.declScope_=scope} = do
    -- generate tac code for the expr
    Just valId <- genTacExpr val
    -- get var@scope
    let varId = getTacId var scope
    -- assign value to var and return the id with the result
    writeTac $ TAC.newTAC TAC.Assign  (TAC.LVId varId) [TAC.RVId valId]
    return (Just varId)

genTacExpr AST.StructAssign{AST.struct=struct, AST.tag=tag, AST.value=value} = do
    s@State{symT=st} <- RWS.get
    case AST.expType struct of
        AST.CustomType name scope -> do
            -- get symbols for struct and tag, maybe struct not needed?
            let maybeStruct = ST.findSymbolInScope name scope st
                maybeTag    = ST.findSymbolInScope tag (scope + 1) st

            case (maybeStruct, maybeTag) of
                (Just structSymb, Just tagSymb) -> do
                    -- generate code for struct expr and value expr
                    maybeStructId <- genTacExpr struct
                    maybeValId    <- genTacExpr value

                    case (maybeStructId, maybeValId) of
                        (Just structId, Just valId) -> do
                            -- aqui habria que hacer structId[offset tag] = valId
                            return Nothing

                        -- if there is no id with the struct or value, error
                        _             ->
                            error "Deberia darme el id donde esta el struct"
                -- if symbols for struct or tag dont exist, error
                _ ->
                    error "Deberia existir el struct"
        _              -> 
        -- if the type of the struct is not a custom type, error
            error "Aqui deberia haber un struct"

            
genTacExpr AST.StructAccess{AST.struct=struct, AST.tag=tag} = do
    s@State{symT=st} <- RWS.get
    case AST.expType struct of
        AST.CustomType name scope -> do
            -- get symbols for struct and tag, maybe struct not needed?
            let maybeStruct = ST.findSymbolInScope name scope st
                maybeTag    = ST.findSymbolInScope tag (scope + 1) st

            case (maybeStruct, maybeTag) of
                (Just structSymb, Just tagSymb) -> do
                    -- generate code for struct expr and value expr
                    maybeStructId <- genTacExpr struct

                    case maybeStructId of
                        Just structId -> do
                            -- aqui habria que hacer return structId[offset tag]
                            return Nothing


                        -- if there is no id with the struct or value, error
                        _             ->
                            error "Deberia darme el id donde esta el struct"
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
    M.forM_ returnIds (\s -> writeTac $ TAC.newTAC TAC.Param (TAC.LVId s) [])

    -- Call function 
    -- @TODO que hago cuando no retorna nada?
    writeTac $ TAC.newTAC TAC.Call (TAC.LVId fcallRetPos) [TAC.RVLabel $ getTacId _fname _declScope_, TAC.Constant . TAC.Int . length $ _actualArgs]

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
            Just startResultId -> startResultId
            _                  -> error "Inconsistent AST: Start expression should return some numeric value, and it's returning nothing"

    let endResultId = case mbEndResultId of
            Just endResultId   -> endResultId
            _                  -> error "Inconsistent AST: End expression should return some numeric value, and it's returning nothing"
    let steptResultId = case mbStepResultId of
            Just steptResultId -> steptResultId
            _                  -> error "Inconsistent AST: Step expression should return some numeric value, and it's returning nothing"

    -- Put start label
    writeTac $ TAC.newTAC TAC.MetaLabel  (TAC.LVLabel forStartLabel) []

    -- Put result value
    mbOutResultId <- genTacExpr _cicBody


    -- Put End Label
    writeTac $ TAC.newTAC TAC.MetaLabel  (TAC.LVLabel forEndLabel) []

    -- Return consistency checking
    case (mbOutResultId, _expType) of 
        (Just outResultId, AST.TUnit) -> error "Inconsistent TAC Generator: should return nothing when body expression returns Unit"
        (Nothing, t)                  -> error "Inconsistent TAC Generator: should provide a return id when body expression returns something different from Unit"
        (Just outResultId, _)         -> writeTac $ TAC.newTAC TAC.Assign (TAC.LVId forResultId) [TAC.RVId outResultId]

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
    startLabel    <- getNextLabelTemp' "while_start"
    outLabel      <- getNextLabelTemp' "while_out"
    whileResultId <- getNextTemp

    -- Don't add a return addres if returns nothing
    let mbWhileResultId
            | _expType /= AST.TUnit = Just whileResultId
            | otherwise  = Nothing 

    let whileData = IterData {breakLabel=outLabel, continueLabel=startLabel, iterReturnId = mbWhileResultId }

    -- Put current iterator data in the state
    pushNextIterData whileData

    -- Add label marking the while start
    writeTac $ TAC.newTAC TAC.MetaLabel (TAC.LVLabel startLabel) []

    -- Generate code for condition checking 
    Just condId <- genTacExpr _cond -- may raise error when returns Nothing, this is intended

    -- Generate code to jump to the end when condition is not met
    writeTac $ TAC.newTAC TAC.Neg  (TAC.LVId condId)      [TAC.RVId condId]
    writeTac $ TAC.newTAC TAC.Goif (TAC.LVLabel outLabel) [TAC.RVId condId]

    -- Generate code for body
    mbBodyResultId <- genTacExpr _cicBody 

    -- Update result if while loop has return type
    M.when (_expType /= AST.TUnit) $
        case mbBodyResultId of 
            Just bodyResultId -> writeTac $ TAC.newTAC TAC.Assign (TAC.LVId whileResultId) [TAC.RVId bodyResultId]
            Nothing -> error $ "Inconsistent AST: Body of while returning nothing when type of while is not Unit. \n\t" ++ "Expected return type: " ++ show _expType
    
    -- Add code for going to the start
    writeTac $ TAC.newTAC TAC.Goto (TAC.LVLabel startLabel) []

    -- Add goto label
    writeTac $ TAC.newTAC TAC.MetaLabel (TAC.LVLabel outLabel) []

    -- Remove data for this iterator instruction
    popNextIterData
    
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
    writeTac $ TAC.newTAC TAC.Neg (TAC.LVId condNegId) [TAC.RVId condId]

    -- if negated cond is true, jump to else code
    writeTac (TAC.TACCode TAC.Goif (Just (TAC.LVLabel elseLabel)) (Just (TAC.RVId condNegId)) Nothing)

    -- gen code for if 
    ifResultId <- genTacExpr accExpr

    -- if the type of the if-else its not unit, update the return type
    M.when (expType /= AST.TUnit) $
        case ifResultId of
            Just _ifResultId ->
                writeTac $ TAC.newTAC TAC.Assign (TAC.LVId resultId) [TAC.RVId _ifResultId]
            Nothing ->
                error "Inconsistent AST: no result for if when it does have a return type diferent from unit"

    -- jump to the out label
    writeTac (TAC.TACCode TAC.Goto (Just (TAC.LVLabel outLabel)) Nothing Nothing)

    -- create else label
    writeTac (TAC.TACCode TAC.MetaLabel (Just (TAC.LVLabel elseLabel)) Nothing Nothing)
    -- gen code for else
    elseResultId <- genTacExpr failExpr
    -- if the type of the if-else its not unit, assign the return type
    M.when (expType /= AST.TUnit) $
        case elseResultId of
            Just _elseResultId ->
                writeTac (TAC.TACCode TAC.Assign (Just (TAC.LVId resultId)) (Just (TAC.RVId _elseResultId)) Nothing)
            Nothing ->
                return ()

    -- create out label
    writeTac (TAC.TACCode TAC.MetaLabel (Just (TAC.LVLabel outLabel)) Nothing Nothing)

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

genTacExpr AST.Return{} = undefined
genTacExpr AST.Break {AST.expr=_expr, AST.expType=_expType} = do
    {-
        Template for break:
            # Code for break expresion evaluation if necessary
            iter_return_id := expr_return_id # only if type != unit
            goto iter_out@l0
    -}
    -- Get data for the current iteration to break
    iterData@IterData {breakLabel=_breakLabel, iterReturnId=_iterReturnId} <- topCurrentIterData

    case _expType of
        AST.TUnit -> return ()
        _         ->  do  
            -- Generate code for expression & get the return value id
            Just expResultId <- genTacExpr _expr    -- May crash when expression return is Nothing, this is intended

            let Just _iterReturnId' = _iterReturnId -- May crash when this iter is Nothing, this is intended

            -- Save the expression value in the iterator return value
            writeTac $ TAC.newTAC TAC.Assign (TAC.LVId _iterReturnId') [TAC.RVId expResultId]

    -- Go to the out label
    writeTac $ TAC.newTAC TAC.Goto  (TAC.LVLabel _breakLabel) []

    -- Return value id
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
    iterData@IterData {continueLabel=_continueLabel, iterReturnId=_iterReturnId} <- topCurrentIterData

    case _expType of
        AST.TUnit -> return ()
        _         ->  do  
            -- Generate code for expression & get the return value id
            Just expResultId <- genTacExpr _expr    -- May crash when expression return is Nothing, this is intended

            let Just _iterReturnId' = _iterReturnId -- May crash when this iter is Nothing, this is intended

            -- Save the expression value in the iterator return value
            writeTac $ TAC.newTAC TAC.Assign (TAC.LVId _iterReturnId') [TAC.RVId expResultId]

    -- Go to the out label
    writeTac $ TAC.newTAC TAC.Goto  (TAC.LVLabel _continueLabel) []

    -- Return value id
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
    -- get temp id
    currId <- getNextTemp
    -- assing the op to the id and return the id
    writeTac (TAC.TACCode (mapOp2 op) (Just (TAC.LVId currId)) (Just (TAC.RVId leftId)) (Just (TAC.RVId rightId)))
    return (Just currId)

genTacExpr AST.Op1{AST.op1=op, AST.opr=l} = do
    -- generate code for expr
    Just opId <- genTacExpr l

    -- if op is Unit, return nothing
    -- else assing to a temp variable and return it
    case op of
        AST.UnitOperator -> return Nothing
        _                -> do
            currId <- getNextTemp
            writeTac (TAC.TACCode (mapOp1 op) (Just (TAC.LVId currId)) (Just (TAC.RVId opId)) Nothing)
            return (Just currId)

genTacExpr AST.Array{} = undefined
genTacExpr AST.UnionTrying{} = undefined
genTacExpr AST.UnionUsing{} = undefined
genTacExpr AST.New{} = undefined

genTacExpr AST.Delete{} = undefined
genTacExpr AST.ArrayIndexing{} = undefined



-- gen code for a block, return the last id (or Nothing), because
-- the value of a block is the value of the last expr in it
genTacBlock :: [AST.Expr] -> GeneratorMonad (Maybe String)
genTacBlock []  = return Nothing
genTacBlock [e] = genTacExpr e
genTacBlock (e:es) = do
    genTacExpr e
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
