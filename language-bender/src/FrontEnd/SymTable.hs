module FrontEnd.SymTable where

--import Control.Monad.State
import qualified Control.Monad.Trans as T
import qualified Control.Monad.RWS   as RWS
import qualified FrontEnd.Errors     as E
import qualified FrontEnd.AST        as AST

import qualified Data.Map.Strict     as Map
--import qualified Data.HashMap.Strict as H


type Scope = Int
type ScopeStack = [Scope]
type Identifier = String


data Category
    = Variable      {}
    | Constant      {}
    | Type          {}
    | Procedure     {}
    | Function      {}
    | StructType    {}
    | UnionType     {}
    | ReferenceType {}
    | Parameter     {}
    deriving (Eq, Show)


data SymValue = SymValue
    { identifier :: Identifier          -- Entry name
    , category :: Category              -- Category of the entry
    , scope :: Scope                    -- current scope at insertion
    , enrtyType :: (Maybe String)       -- pointer to another entry
    } deriving (Eq, Show)

type Dictionary = Map.Map Identifier [SymValue]

data SymTable = SymTable
    { stDict :: Dictionary              -- dict of valid symbols
    , stScopeStk :: ScopeStack          -- stack of scopes
    , stCurrScope :: Int                -- curr scope 
    } deriving (Eq, Show)


type LangBendState RWS.RWST () [E.Error] SymTable IO

