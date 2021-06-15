{-
    Data Structures and functions for the symbol table
-}

module FrontEnd.SymTable where


-- <Language Bender Imports> ------------------------------------
import qualified FrontEnd.Errors     as E
import qualified FrontEnd.AST        as AST

-- <Utility Data types> -----------------------------------------
import qualified Control.Monad.Trans as T
import qualified Control.Monad.RWS   as RWS
import qualified Data.Map.Strict     as Map
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
type Dictionary = Map.Map Identifier [Symbol] 
-- | State used to simulate an imperative process of type checking 
type LangBendState a = RWS.RWST () [E.Error] SymTable a


-- | Symbol Type with its corresponding data
data SymType   
    = Variable      { varType ::  Maybe AST.Type, initVal :: Maybe AST.Expr }
    | Constant      { consType ::  AST.Type, consInitVal :: AST.Expr }
    | Type          { unType :: AST.Type}
    | Procedure     { args :: [AST.FuncArg], body :: AST.Expr }
    | Function      { args :: [AST.FuncArg], retType :: AST.Type , body :: AST.Expr }
    | StructType    { fields :: [(AST.Name, AST.Type)] }
    | UnionType     { fields :: [(AST.Name, AST.Type)] }
    | Reference     { refName :: AST.Name, refType :: AST.Type }
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



