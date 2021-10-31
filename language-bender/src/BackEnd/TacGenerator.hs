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

-- >> Data ------------------------------------------------------

-- | Stateful information required by the conversion process
data GeneratorState = State
                    { nextTemporal :: Int
                    , nextLabelTemporal :: Int
                    -- , symTable :: ST.SymTable
                    }

-- | Monad used to keep a state when traversing the AST to generate the code
type GeneratorMonad = RWS.RWST () [TAC.TACCode] GeneratorState IO


-- >> Handling Monad --------------------------------------------

-- | get next temporal variable
getNextTemp :: GeneratorMonad String
getNextTemp = do
    State{nextTemporal = n, nextLabelTemporal = m} <- RWS.get
    RWS.put State{nextTemporal = (n+1), nextLabelTemporal = m}
    return $ "T" ++ (show n) 

-- | get next label temporal variable
getNextLabelTemp :: GeneratorMonad String
getNextLabelTemp = do
    State{nextTemporal = m, nextLabelTemporal = n} <- RWS.get
    RWS.put State{nextTemporal = m, nextLabelTemporal = (n+1)}
    return $ "L" ++ (show n) 

-- | write TAC instruccion
writeTac :: TAC.TACCode -> GeneratorMonad ()
writeTac tacInst = do
    RWS.tell [tacInst]

-- | Initial Generator State
initialGenState :: GeneratorState
initialGenState = State{nextTemporal = 0, nextLabelTemporal = 0}

-- >> Auxiliar Functions ----------------------------------------

-- | generate an unique tac id from a language bender id
getTacId :: String -> Int -> String
getTacId id scope = id ++ "@" ++ (show scope)

-- >> Main Function ---------------------------------------------

-- | Generate a Tac program using the symbol table and the AST object, returning the 
-- | resulting state and the generated program
generateTac :: ST.SymTable  -> AST.Program  -> IO (GeneratorState, TAC.TACProgram)
generateTac symbolTable program = do
    (genState, tacCode) <- RWS.execRWST (generateTac' symbolTable program) () initialGenState
    return (genState, TAC.TACProgram tacCode)    

-- | Utility function to generate the actual Tac Program.
generateTac' :: ST.SymTable  -> AST.Program  -> GeneratorMonad ()
generateTac' symT program = do
    hasMain <- findMain symT
    M.when hasMain $ writeTac (TAC.TACCode TAC.Goto (Just (TAC.LVLabel "main@0")) Nothing Nothing)
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
genTacDecl AST.Variable{} = undefined

-- | gen tac for references decl
genTacDecl AST.Reference{} = undefined

-- | gen tac for unions decl
genTacDecl AST.Union{} = undefined

-- | gen tac for structs decl
genTacDecl AST.Struct{} = undefined

-- | gen tac for functions and procedure decls
genTacDecl AST.Func{AST.decName=name, AST.body=body, AST.declScope=scope} = do
    -- fuction name label
    writeTac (TAC.TACCode TAC.MetaLabel (Just (TAC.LVLabel (getTacId name scope))) Nothing Nothing)
    -- generate tac code for function body
    genTacExpr body
    return()


-------------------------------------------------------
-- | generate tac for expressions ---------------------
-- | returns the id where the result is stored ? ------
genTacExpr :: AST.Expr -> GeneratorMonad (Maybe String)
genTacExpr AST.ConstChar{} = undefined
genTacExpr AST.ConstString{} = undefined

genTacExpr AST.ConstInt{AST.iVal=val} = do
    -- get next temporal id and save the const int in it
    currId <- getNextTemp
    writeTac (TAC.TACCode TAC.Assign (Just (TAC.LVId currId)) (Just (TAC.Constant (TAC.Int val))) Nothing)
    return (Just currId)

genTacExpr AST.ConstFloat{AST.fVal=val} = do
    -- get next temporal id and save the const float in it
    currId <- getNextTemp
    writeTac (TAC.TACCode TAC.Assign (Just (TAC.LVId currId)) (Just (TAC.Constant (TAC.Float val))) Nothing)
    return (Just currId)

genTacExpr AST.ConstTrue{} = do
    -- get next temporal id and save true in it
    currId <- getNextTemp
    writeTac (TAC.TACCode TAC.Assign (Just (TAC.LVId currId)) (Just (TAC.Constant (TAC.Bool True))) Nothing)
    return (Just currId)

genTacExpr AST.ConstFalse{} = do
    -- get next temporal id and save true in it
    currId <- getNextTemp
    writeTac (TAC.TACCode TAC.Assign (Just (TAC.LVId currId)) (Just (TAC.Constant (TAC.Bool False))) Nothing)
    return (Just currId)

genTacExpr AST.ConstStruct{} = undefined
genTacExpr AST.ConstUnion{} = undefined
genTacExpr AST.ConstUnit{} = return Nothing
genTacExpr AST.ConstNull{} = undefined
genTacExpr AST.Id{AST.name=name, AST.declScope_=scope} = 
    -- just return the id@scope
    return (Just (getTacId name scope))
genTacExpr AST.Assign{AST.variable=var, AST.value=val, AST.declScope_=scope} = do
    -- generate tac code for the expr
    Just valId <- genTacExpr val
    -- get var@scope
    let varId = getTacId var scope
    -- assign value to var and return the id with the result
    writeTac (TAC.TACCode TAC.Assign (Just (TAC.LVId varId)) (Just (TAC.RVId valId)) Nothing)
    return (Just varId)

genTacExpr AST.StructAssign{} = undefined
genTacExpr AST.StructAccess{} = undefined
genTacExpr AST.FunCall{} = undefined
genTacExpr AST.For{} = undefined
genTacExpr AST.While{} = undefined

genTacExpr AST.If{AST.cond=cond, AST.accExpr=accExpr, AST.failExpr=failExpr, AST.expType=expType} = do
    -- get needed labels to select where to go
    elseLabel <- getNextLabelTemp
    outLabel <- getNextLabelTemp
    resultId <- getNextTemp

    -- get conditional and negate it
    Just condId <- genTacExpr cond
    condNegId <- getNextTemp
    writeTac (TAC.TACCode TAC.Neg (Just (TAC.LVId condNegId)) (Just (TAC.RVId condId)) Nothing)

    -- if negated cond is true, jump to else code
    writeTac (TAC.TACCode TAC.Goif (Just (TAC.LVLabel elseLabel)) (Just (TAC.RVId condNegId)) Nothing)

    -- gen code for if 
    ifResultId <- genTacExpr accExpr
    -- if the type of the if-else its not unit, update the return type
    M.when (expType /= AST.TUnit) $ 
        case ifResultId of 
            Just _ifResultId ->
                writeTac (TAC.TACCode TAC.Assign (Just (TAC.LVId resultId)) (Just (TAC.RVId _ifResultId)) Nothing)
            Nothing -> 
                return ()
    -- jump to the out label
    writeTac (TAC.TACCode TAC.Goto (Just (TAC.LVLabel outLabel)) Nothing Nothing)

    -- gen code for else
    writeTac (TAC.TACCode TAC.MetaLabel (Just (TAC.LVLabel elseLabel)) Nothing Nothing)
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
    if (expType /= AST.TUnit) then
        return (Just resultId)
    else
        return Nothing


genTacExpr AST.ExprBlock{AST.exprs=exprs, AST.expType=expType} = do
    -- generate code for exprs in block, if type is unit return nothing
    -- else return whatever the last expr in block returns
    maybeId <- genTacBlock exprs
    if (expType == AST.TUnit) then
        return Nothing
    else
        return maybeId

genTacExpr AST.Return{} = undefined
genTacExpr AST.Break{} = undefined
genTacExpr AST.Continue{} = undefined
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
