module FrontEnd.StaticErrors where

-- < Local Imports > ------------------------------------------------------

import qualified FrontEnd.SymTable as ST
import qualified FrontEnd.Utils as U
---------------------------------------------------------------------------

-- | Static Analysis Errors
data StaticError = SymbolNotInScope { symName :: U.Name } 
                 | SymbolRedefinition {symName :: U.Name} 
                 | ReferencingNonVariable { symName :: U.Name } 
                 | NotValidType  { nonTypeName :: U.Name }
                 | NotAValidVariable { symName :: U.Name, actualSymType :: ST.SymType }
                 | NotAValidFunction { symName :: U.Name, actualSymType :: ST.SymType  }
                 | NotAValidStruct { symName :: U.Name, actualSymType :: ST.SymType }
                 | NotAValidUnion { symName :: U.Name, actualSymType :: ST.SymType }
                 | UnexpectedEOF
                 | ParseError  
                 deriving(Eq, Show)  
