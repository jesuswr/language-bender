module FrontEnd.StaticErrors where

-- < Local Imports > ------------------------------------------------------

import qualified FrontEnd.SymTable  as ST
import qualified FrontEnd.Utils     as U
import qualified FrontEnd.AST       as AST
import qualified FrontEnd.Tokens    as T
---------------------------------------------------------------------------

-- | Static Analysis Errors
data StaticError = SymbolNotInScope         { symName :: U.Name } 
                 | SymbolRedefinition       { symName :: U.Name} 
                 | ReferencingNonVariable   { symName :: U.Name , refName :: U.Name } 
                 | NotValidType             { nonTypeName :: U.Name }
                 | NotAValidVariable        { symName :: U.Name, actualSymType :: ST.SymType }
                 | NotAValidFunction        { symName :: U.Name, actualSymType :: ST.SymType }
                 | NotAValidStruct          { symName :: U.Name, actualSymType :: ST.SymType }
                 | NotAValidUnion           { symName :: U.Name, actualSymType :: ST.SymType }
                 | NonArrayExpr             { actualType :: AST.Type }
                 | DuplicateNamesInCompound { symName :: U.Name }
                 | UnexpectedEOF
                 | ParseError               { remStream :: [T.Token] }
                 | UnmatchingTypes          { expectedTypes :: [AST.Type], actualType :: AST.Type }
                 | AssignToConst            { symName :: U.Name  } -- you can't assign to const initialized variables
                 | FewArguments             { refTo :: U.Name, expectedNumOfArgs :: Int, actualNumOfArgs :: Int }
                 | TooManyArguments         { refTo :: U.Name, expectedNumOfArgs :: Int, actualNumOfArgs :: Int }
                 | CouldNotInferType        { symName :: U.Name }
                 | NestedFunctions          { symName :: U.Name }
                 deriving(Eq)  


instance Show StaticError where
    show (SymbolNotInScope name)              = "\t~ Error: '" ++ name ++ "' was not declared in this scope"
    show (SymbolRedefinition name)            = "\t~ Error: redeclaration of '" ++ name ++ "'"
    show (ReferencingNonVariable lname rname) = "\t~ Error: can't make '" ++ lname ++ "' a reference of non-variable '" ++ rname ++ "'"
    show (NotValidType tname)                 = "\t~ Error: '" ++ tname ++ "' it's not a valid type" 
    show (NotAValidVariable name actType)     = "\t~ Error: expected a variable and got '" ++ name ++ "' of type '" ++ show actType ++ "'"
    show (NotAValidFunction name actType)     = "\t~ Error: expected a function and got '" ++ name ++ "' of type '" ++ show actType ++ "'"
    show (NotAValidStruct name actType)       = "\t~ Error: expected a struct and got '" ++ name ++ "' of type '" ++ show actType ++ "'"
    show (NotAValidUnion name actType)        = "\t~ Error: expected an union and got '" ++ name ++ "' of type '" ++ show actType ++ "'"
    show (NonArrayExpr actType)               = 
        case actType of
            AST.TypeError   ->
                "\t~ Error: expected an array and didn't get it"
            _               ->      
                "\t~ Error: expected an array and got an expression of type '" ++ AST.simplePrint actType ++ "'"
    show (DuplicateNamesInCompound name)      = "\t~ Error: multiple uses of tag '" ++ name ++ "' in a compound"
    show (UnexpectedEOF)                      = "\t~ Error: unexpected EOF while parsing"
    show (ParseError remainingInput)          = "\t~ Error: parsing error"
    show (UnmatchingTypes expTypes actType)   = 
        case actType of
            AST.TypeError   ->
                "\t~ Error: expected any of '" ++ AST.simpleListPrint expTypes ++ "' but didn't get it"
            _               ->
                "\t~ Error: expected any of '" ++ AST.simpleListPrint expTypes ++ "' but got '" ++ AST.simplePrint actType ++ "'"
    show (AssignToConst name)                 = "\t~ Error: assignment of constant variable " ++ show name
    show (CouldNotInferType name)             = "\t~ Error: Could not infer type of " ++ show name
    show (NestedFunctions name)               = "\t~ Error: Nested function declaration. In declaration of " ++ show name
    show badArgNumbers                        = "\t~ Error: in call to '" ++ show (refTo badArgNumbers) ++ "' expected " ++ show (expectedNumOfArgs badArgNumbers) ++ " arguments but got " ++ show (actualNumOfArgs badArgNumbers)
