{-
    Module defining AST data types and related functions
-}

module FrontEnd.AST where


-- for identifiers mostly
type Name = String

-- Type Definition
data Type = TFloat
          | TInt 
          | TChar 
          | TString
          | TBool
          | TArray { arrType :: Type, size :: Expr }
          | TPtr   { ptrType :: Type }
          | CustomType { tName :: Name }
          deriving(Eq, Show)

-- Declaration of new things
data Declaration    = Variable  { decName :: Name, varType ::  Maybe Type, initVal :: Maybe Expr, isConst :: Bool }
                    | Union     { decName :: Name, fields :: [(Name, Type)] }
                    | Struct    { decName :: Name, fields :: [(Name, Type)] }
                    | Func      { decName :: Name, args :: [(Name, Type)], retType :: Type , body :: Expr }
                    deriving(Eq, Show)

-- Binary operators
data BinOpr  = Mult
             | Div
             | Mod
             | Sum 
             | Sub
             | And
             | Or
             | Eq
             deriving(Eq, Show)

-- Unary operators
data UnOpr  = Negation  
            | Negative
            | Unit
            deriving(Eq, Show)

-- Possible expressions. Remember, everything its an expression
data Expr   = ConstInt        { iVal :: Int}
            | ConstFloat      { fVal :: Float}
            | ConstChar       { cVal :: String}
            | ConstString     { sVal :: String}
            | ConstBool       { bVal :: Bool }
            | ConstUnit
            | Id              { name :: Name}
            | BinOper         { bOpr :: BinOpr, lexpr :: Expr, rexpr :: Expr}
            | UnOper          { uOpr :: UnOpr, expr :: Expr }
            | Assign          { variable :: Name, value :: Expr}
            | FunCall         { fname :: Name, actualArgs :: [Expr]}
            | For             { initDecl :: Declaration, cond :: Expr, step :: Expr, cicBody :: Expr }
            | While           { cond :: Expr, cicBody :: Expr}
            | If              { cond :: Expr, accExpr :: Expr, failExpr :: Expr }
            | ExprBlock       { exprs :: [Expr] }
            | Return          { maybeExpr :: Maybe Expr }
            | Break           { maybeExpr :: Maybe Expr }
            | Continue        { maybeExpr :: Maybe Expr }
            | Declaration     { decl :: Declaration }
            deriving(Eq, Show)

-- Program data type     
newtype Program = Program{ decls :: [Declaration] }deriving(Show, Eq)