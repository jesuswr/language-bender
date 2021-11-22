{-
    Data Structures and functions for the symbol table
-}

module FrontEnd.SymTable where


-- <Language Bender Imports> ------------------------------------
import qualified FrontEnd.AST        as AST
import qualified FrontEnd.Utils      as U
-- <Utility Data types> -----------------------------------------
import qualified Control.Monad.Trans as T
import qualified Control.Monad.RWS   as RWS
import qualified Data.Map.Strict     as M
import qualified Data.List           as L
import           Data.Maybe(isNothing, fromJust)
-----------------------------------------------------------------

-- <Type Definitions> ------------------------------------------
-- | Execution context in the program
type Scope = Int 
-- | Stack of currently available scopes 
type ScopeStack = [Scope]
-- | Stack of current offsets 
type OffsetStack = [Int]
-- | name of a symbol  
type Identifier = String                        
-- | Map from ids to its corresponding symbol data. Multiple symbols may have the same name, that's why we keep a 
--   list of Symbols instead of a single symbol
type Dictionary = M.Map Identifier [Symbol] 


-- | Symbol Type with its corresponding data
data SymType   
    = Variable      { varType :: AST.Type, initVal :: Maybe AST.Expr, isConst :: Bool, offset :: Int, staticLabel :: Maybe String }
    | Type          { unType :: AST.Type , width :: Int, align :: Int }
    | StructType    { fields :: [(U.Name, AST.Type)] , width :: Int, align :: Int }
    | UnionType     { fields :: [(U.Name, AST.Type)] , width :: Int, align :: Int }
    | Procedure     { args :: [AST.FuncArg], body :: AST.Expr }
    | Function      { args :: [AST.FuncArg], retType :: AST.Type , body :: AST.Expr }
    | Reference     { refName :: U.Name, refType :: AST.Type, refScope :: Int, offset :: Int, staticLabel :: Maybe String}
    deriving (Eq)

-- | Symbol Data type
data Symbol = Symbol
    { identifier :: Identifier          -- ^ Entry name
    , symType :: SymType                -- ^ Symbol Type of the entry
    , scope :: Scope                    -- ^ current scope at insertion
    , enrtyType :: Maybe SymType        -- ^ pointer to another entry
    } deriving (Eq)

-- | Symbol Table: Le Blanc - Cook based implementation 
data SymTable = SymTable
    { stDict :: Dictionary              -- ^ dict of valid symbols
    , stScopeStk :: ScopeStack          -- ^ stack of scopes
    , stOffsetStk :: OffsetStack        -- ^ stack of offsets 
    , stNextScope :: Int                -- ^ current scope 
    } deriving (Eq)

-- < Utility Functions > ----------------------------------------

-- | Create a new empty table
newTable :: SymTable
newTable = SymTable{
    stDict = initDic,
    stScopeStk = [0],
    stOffsetStk = [0],
    stNextScope = 1
}

initDic :: Dictionary
initDic = M.fromList [("air", [Symbol "air" (Type AST.TInt 4 4) 0 Nothing])
                    , ("water", [Symbol "water" (Type AST.TFloat 4 4) 0 Nothing])
                    , ("fire", [Symbol "fire" (Type AST.TBool 1 1) 0 Nothing])
                    , ("earth", [Symbol "earth" (Type AST.TChar 1 1) 0 Nothing])
                    , ("art", [Symbol "art" (Type (AST.TPtr AST.TVoid) 4 4) 0 Nothing])
                    , ("reincarnation", [Symbol "reincarnation" (Type (AST.TPtr AST.TVoid) 4 4) 0 Nothing])
                    ]

pushOffset :: SymTable -> Int -> SymTable
pushOffset st@SymTable{stOffsetStk = stk} o = st{stOffsetStk = o:stk}

popOffset :: SymTable -> SymTable
popOffset st@SymTable{stOffsetStk = stk} = st{stOffsetStk = tail stk}

getCurrentOffset :: SymTable -> Int
getCurrentOffset st@SymTable{stOffsetStk = stk} = head stk

updateOffset :: SymTable -> Int -> SymTable
updateOffset st@SymTable{stOffsetStk = stk} newOffset = st{stOffsetStk = newOffset:(tail stk)}

getMaxSize :: SymTable -> [(U.Name, AST.Type)] -> Int
getMaxSize st x = maximum $ map (getTypeSize st . snd) x

getTypeSize :: SymTable -> AST.Type -> Int
getTypeSize st AST.TArray{AST.arrType = t, AST.size = s} = 
    case s of
        AST.ConstInt val _ -> val * (getTypeSize st t) -- no dope vector
        _                  -> 8 -- dope vector
getTypeSize st AST.CustomType{AST.tName = n, AST.scope = s} = 
    case findSymbolInScope n s st of
        Nothing  -> 0
        Just sym -> 
            case symType sym of 
                Type _ w _       -> w
                StructType _ w _ -> w
                UnionType _ w _  -> w
                _                -> 0
getTypeSize st t = 
    case findSymbolInScope (AST.getTypeId t) 0 st of
        Nothing  -> 0
        Just sym ->
            case symType sym of 
                Type _ w _ -> w
                _          -> 0

getTypeAlign :: SymTable -> AST.Type -> Int
getTypeAlign st AST.TArray{AST.arrType = t, AST.size = s} = getTypeAlign st t
getTypeAlign st AST.CustomType{AST.tName = n, AST.scope = s} = 
    case findSymbolInScope n s st of
        Nothing  -> 1
        Just sym -> 
            case symType sym of 
                Type _ _ a       -> a
                StructType _ _ a -> a
                UnionType _ _ a  -> a
                _                -> 1
getTypeAlign st t = 
    case findSymbolInScope (AST.getTypeId t) 0 st of
        Nothing  -> 1
        Just sym ->
            case symType sym of 
                Type _ _ a -> a
                _          -> 1

-- Handle Variables 

getVarAtrr :: SymTable -> Identifier -> Int -> (SymType -> a) -> a
getVarAtrr st id scope f =
    case findSymbolInScope' id scope st of
        Nothing -> error (id ++ " in scope "++ show scope ++ " should exist")
        Just sym ->
            case symType sym of
                v@Variable{} -> f v
                r@Reference{} -> f r
                _ -> error "This function only supports variables or references"

getVarOffset :: SymTable -> Identifier -> Int -> Int
getVarOffset st id scope = getVarAtrr st id scope offset

getVarType ::SymTable -> Identifier -> Int -> AST.Type
getVarType st id scope = getVarAtrr st id scope varType

getVarIsConst :: SymTable -> Identifier -> Int -> Bool
getVarIsConst st id scope = getVarAtrr st id scope isConst

getVarStaticLabel :: SymTable -> Identifier -> Int -> Maybe String
getVarStaticLabel st id scope = getVarAtrr st id scope staticLabel

setVarStaticLabel :: SymTable -> Identifier -> Int -> String -> SymTable
setVarStaticLabel st id scope label = 
    case findSymbolInScope' id scope st of
        Nothing -> error (id ++ " in scope "++ show scope ++ " should exist")
        Just sym ->
            case symType sym of
                v@Variable{} -> st{stDict=newDict}
                              where

                                -- look for entry in dict where the symbol is stored
                                dict = stDict st
                                Just list =  M.lookup id dict
                                symToInsert = sym{symType=(v{staticLabel=Just label})}

                                newList = [ if sym == s then symToInsert else s | s <- list ]

                                -- Create new dict
                                newDict = M.insert id newList dict


-- | Push a new scope in the given table
pushEmptyScope :: SymTable -> SymTable
pushEmptyScope st@SymTable{stNextScope = s, stScopeStk = stk} = st{stNextScope = s + 1, stScopeStk = s:stk}

-- | Pop Current scope; If stack is empty, leave it empty
popCurrentScope :: SymTable -> SymTable
popCurrentScope st@SymTable{stScopeStk = [0]} = st
popCurrentScope st@SymTable{stScopeStk = (_:stk)} = st{stScopeStk = stk}

-- | Retrieve current scope from a table
stCurrScope :: SymTable -> Scope
stCurrScope = head . stScopeStk  

-- | Insert a new symbol. Note that the scope attribute for such symbol is going to be overriden.
-- | May Return nothing in case of symbol redefinition
insertSymbol :: Symbol 
             -> SymTable
             -> Maybe SymTable
insertSymbol s st = res
    where
        -- aux data
        symId = identifier s                -- Symbol Name
        currScope = stCurrScope st          -- current scope
        maybeExisting = findSymbol symId st -- find symbol if defined with provided name 
        
        -- add symbol if possible 
        newSym = s{scope = currScope}       -- symbol to be added in case of succesfull insertion.
        symDict = stDict st                 -- current dict of symbols.
        syms    = M.lookup symId symDict    -- find name in symbol dict.
        newSyms = case syms of              -- if no symbol at all, list starts inexistent, create a new one 
            Nothing -> [newSym]             -- with our new symbol. Otherwise, add symbol to current list
            Just ls -> newSym:ls
        newSymDict = M.insert symId newSyms symDict

        newSt = st{stDict=newSymDict}

        res = case maybeExisting of 
            Nothing     -> Just newSt
            Just sym    -> if scope sym == currScope -- could not insert symbol if already in current scope 
                                then Nothing 
                                else Just newSt

-- | Update next symbol in scope named the same as this one, delete the old one and replace it 
-- | for the given symbol. If no symbol was found with the same name, do nothing. The scope will be 
-- | overriden
updateSymbol :: Symbol
             -> SymTable
             -> SymTable
updateSymbol sym st = st'
    where
        sname = identifier sym
        mbSym = findSymbol sname st
        
        -- In case the sym is actually there
        Just sym' = mbSym
        symToInsert = sym{scope=scope sym'}

        -- look for entry in dict where the symbol is stored
        dict = stDict st
        Just list =  M.lookup sname dict -- should not crash as we already looked this symbol and it's available

        newList = [ if sym' == s then symToInsert else s | s <- list ]

        -- Create new dict
        newDict = M.insert sname newList dict

        st' = case mbSym of 
                Nothing   -> st
                Just sym' -> st{stDict=newDict}


-- | Find Symbol by name if available in current scope
findSymbol  :: Identifier   -- ^ Symbol identifier
            -> SymTable     -- ^ Table to look in
            -> Maybe Symbol -- ^ Symbol to return if some was found 
findSymbol id st = findSymbol' id st (const True) 

-- | Find Symbol by name that matches given predicate. 
-- If Symbol occurs multiple times, then we return the one whose scope 
-- is the nearest to the top of the scope stack
findSymbol' :: Identifier       -- ^ Id to search for 
            -> SymTable         -- ^ Table to look in
            -> (Symbol -> Bool) -- ^ Filter function 
            -> Maybe Symbol     -- ^ Symbol to return
findSymbol' id st f = res
    where 
        scopeStk     = stScopeStk st            -- Scope stack
        maybeSyms    = M.lookup id $ stDict st  -- Related Symbols with this name

        findSym  l  =  [s | s <- l, scope s `elem` scopeStk, f s]           -- search for elems in scope that matches given property
        maxInScope l = L.maximumBy (\a b -> compare (scope a) (scope b)) l  -- search element in maximum scope 

        -- Compute result 
        res = case maybeSyms of 
            Nothing -> Nothing 
            Just l  -> case findSym  l of 
                        [] -> Nothing
                        ss -> Just $ maxInScope ss

-- | Find a symbol in a specific scope
findSymbolInScope :: Identifier -> Scope -> SymTable -> Maybe Symbol
findSymbolInScope id scp st = findSymbol' id st ((==scp) . scope)

-- | Find a symbol in a specific scope, second implementation
findSymbolInScope' :: Identifier -> Scope -> SymTable -> Maybe Symbol
findSymbolInScope' id scp st = res
    where 
        maybeSyms    = M.lookup id $ stDict st  -- Related Symbols with this name

        findSym  l  =  [s | s <- l, ((==scp) . scope) s] -- search for elems in scope

        -- Compute result 
        res = case maybeSyms of 
            Nothing -> Nothing 
            Just l  -> case findSym  l of 
                        [] -> Nothing
                        [s] -> Just s

-- | Tells if a symbol is a variable or not:
isVariable :: Symbol -> Bool 
isVariable Symbol {symType = Variable{}} = True
isVariable _ = False 

-- | Tells if a symbol is a reference or not
isReference :: Symbol -> Bool
isReference Symbol {symType = Reference{}} = True
isReference _ = False

-- | Tells if it's function or not:
isFunction :: Symbol -> Bool 
isFunction Symbol {symType = Function{}} = True
isFunction _ = False

-- | Tells if it's procedure or not:
isProc :: Symbol -> Bool 
isProc Symbol {symType = Procedure{}} = True
isProc _ = False

-- | Tells if it's struct or not:
isStruct :: Symbol -> Bool
isStruct Symbol {symType = StructType{}} = True
isStruct _ = False

-- | Tells if it's union or not:
isUnion :: Symbol -> Bool
isUnion Symbol {symType = UnionType{}} = True
isUnion _ = False


getIdType :: U.Name -> SymType -> AST.Type
getIdType _ Variable{varType = t} = t
getIdType _ Type{unType = t} = t
getIdType _ Procedure{} = AST.TUnit
getIdType _ Function{retType = t} = t
getIdType _ Reference{refType = t} = t
getIdType nm _ = AST.CustomType{AST.tName=nm, AST.scope = -1}


-- < Pretty Printing > --------------------------------------------

instance Show SymTable where 

    show SymTable {stDict=_stDict, stScopeStk=_stScopeStk, stNextScope=_stNextScope} = 
        "Next Scope: " ++ show _stNextScope ++ 
        "\nScope Stack: \n" ++ _printScopeStack _stScopeStk ++ 
        "\nSymbol Map:\n" ++ _printStDict _stDict

instance Show Symbol where
    
    show Symbol {identifier=_identifier, symType=_symType, scope=_scope, enrtyType=_enrtyType} = 
        _identifier ++ "[" ++ show _scope ++ "] :: " ++ show _symType 

instance Show SymType where

    show Function {args=_args, retType=_retType} = "Func => " ++ _showSignature _args _retType

    show Procedure {args=_args}                  = "Proc => " ++ _showSignature _args AST.TUnit 

    show Variable {varType=_varType, initVal=_initVal, isConst=_isConst, offset=_o} = 
            (if _isConst then "Const " else "") ++  "Var with offset " ++ show _o ++ " => " ++ (init . tail . show) _varType ++ 
            (if isNothing _initVal then "" else  " = initial value")

    show Type {unType=_unType, width=_w, align=_a} = "Type with width " ++ show _w ++ " and align " ++ show _a ++ " => " ++ show _unType

    show UnionType {fields=_fields, width=_w, align=_a}  = "Union with width " ++ show _w ++ " and align " ++ show _a ++ " => " ++ _showSubFields _fields

    show StructType {fields=_fields, width=_w, align=_a} = "Struct with width " ++ show _w ++ " and align " ++ show _a ++ " => " ++ _showSubFields _fields

    show Reference {refName=_refName, refType=_refType, refScope=_refScope, offset=_o} = 
        "Reference with offset " ++ show _o ++ " => &(" ++ _refName ++ "[" ++ show _refScope ++ "]" ++ " :: " ++ show _refType ++ ")"

-- | Print scope stack 
_printScopeStack :: ScopeStack -> String
_printScopeStack (top:stk) =  unlines $ ("\tCurrent Scope: " ++ show top) : ["\t\t- " ++ show i | i <- stk] 
_printScopeStack [] = "\tNo Scope to print"

-- | Print Name Dictionary
_printStDict :: Dictionary -> String 
_printStDict dict =  
    unlines [ "\t- Related Symbols for name: " ++ name ++ "\n" ++
                unlines ["\t\t" ++ show sym | sym <- symList]

                | (name, symList) <-  M.toList dict
            ]

-- | Return signature as str
_showSignature :: [AST.FuncArg] -> AST.Type -> String
_showSignature _args _retType = "(" ++ (if null _args then "" else concat ((U.trim . show . head) _args : [", " ++ (U.trim . show $ arg) | arg <- tail _args]))++ ") -> " ++ (U.trim . show$ _retType) 

-- | Return struct and union subnames as str
_showSubFields :: [(U.Name , AST.Type)] -> String 
_showSubFields  [] = "{}"
_showSubFields  ((name, t):nts) = "{ " ++ name ++ " :: " ++ (U.trim . show $ t) ++ concat [", " ++ name ++ " :: " ++ (U.trim . show $ t) | (name, t) <- nts] ++ " }"
