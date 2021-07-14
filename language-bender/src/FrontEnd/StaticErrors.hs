module FrontEnd.StaticErrors where

-- < Local Imports > ------------------------------------------------------

import qualified FrontEnd.SymTable  as ST
import qualified FrontEnd.Utils     as U
import qualified FrontEnd.AST       as AST
---------------------------------------------------------------------------

-- | Static Analysis Errors
data StaticError = SymbolNotInScope         { symName :: U.Name } 
                 | SymbolRedefinition       {symName :: U.Name} 
                 | ReferencingNonVariable   { symName :: U.Name } 
                 | NotValidType             { nonTypeName :: U.Name }
                 | NotAValidVariable        { symName :: U.Name, actualSymType :: ST.SymType }
                 | NotAValidFunction        { symName :: U.Name, actualSymType :: ST.SymType  }
                 | NotAValidStruct          { symName :: U.Name, actualSymType :: ST.SymType }
                 | NotAValidUnion           { symName :: U.Name, actualSymType :: ST.SymType }
                 | DuplicateNamesInCompound { symName :: U.Name  }
                 | UnmatchingTypes          { expectedTypes :: [AST.Type], actualType :: AST.Type }
                 | AssignToConst            { symName :: U.Name  } -- you can't assign to const initialized variables
--                 | TypeError {typeError :: TypeError}
                 deriving(Eq, Show)  


