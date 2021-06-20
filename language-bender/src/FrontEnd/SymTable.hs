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
-----------------------------------------------------------------

-- <Type Definitions> ------------------------------------------
-- | Execution context in the program
type Scope = Int 
-- | Stack of currently available scopes 
type ScopeStack = [Scope]
-- | name of a symbol  
type Identifier = String                        
-- | Map from ids to its corresponding symbol data. Multiple symbols may have the same name, that's why we keep a 
--   list of Symbols instead of a single symbol
type Dictionary = M.Map Identifier [Symbol] 
    


-- | Symbol Type with its corresponding data
data SymType   
    = Variable      { varType ::  Maybe AST.Type, initVal :: Maybe AST.Expr, isConst :: Bool }
    | Constant      { consType ::  AST.Type, consInitVal :: AST.Expr }
    | Type          { unType :: AST.Type}
    | Procedure     { args :: [AST.FuncArg], body :: AST.Expr }
    | Function      { args :: [AST.FuncArg], retType :: AST.Type , body :: AST.Expr }
    | StructType    { fields :: [(U.Name, AST.Type)] }
    | UnionType     { fields :: [(U.Name, AST.Type)] }
    | Reference     { refName :: U.Name, refType :: Maybe AST.Type } -- maybe in case we don't know its type yet, or it can't be tell 
    deriving (Eq, Show)

-- | Symbol Data type
data Symbol = Symbol
    { identifier :: Identifier          -- ^ Entry name
    , symType :: SymType                -- ^ Symbol Type of the entry
    , scope :: Scope                    -- ^ current scope at insertion
    , enrtyType :: Maybe SymType        -- ^ pointer to another entry
    } deriving (Eq, Show)

-- | Symbol Table: Le Blanc - Cook based implementation 
data SymTable = SymTable
    { stDict :: Dictionary              -- ^ dict of valid symbols
    , stScopeStk :: ScopeStack          -- ^ stack of scopes
    , stNextScope :: Int                -- ^ current scope 
    } deriving (Eq, Show)


-- < Utility Functions > ----------------------------------------

-- | Create a new empty table
newTable :: SymTable
newTable = SymTable{
    stDict = M.empty,
    stScopeStk = [0],
    stNextScope = 1
}

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



-- | Tells if a symbol is a variable or not:
isVariable :: Symbol -> Bool 
isVariable Symbol{symType = Variable{}} = True
isVariable _ = False 

-- | Tells if a symbol is a reference or not
isReference :: Symbol -> Bool
isReference Symbol {symType=Reference{}} = True
isReference _ = False

-- | Tells if it's function 
isFunction :: Symbol -> Bool 
isFunction Symbol {symType=Function{}} = True

-- | Tells if it's function 
isProc :: Symbol -> Bool 
isProc Symbol {symType=Procedure{}} = True

