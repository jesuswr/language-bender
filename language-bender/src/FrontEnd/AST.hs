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
          | TUnit 
          | TReference { refType :: Type }
          | CustomType { tName :: Name }
          deriving(Eq, Show)

data FuncArg = FuncArg{ argName :: Name, argType :: Type, defaultVal :: Maybe Expr } deriving(Eq, Show)

-- Declaration of new things
data Declaration    = Variable  { decName :: Name, varType ::  Maybe Type, initVal :: Maybe Expr, isConst :: Bool }
                    | Reference { decName :: Name, refName :: Name }
                    | Union     { decName :: Name, fields :: [(Name, Type)] }
                    | Struct    { decName :: Name, fields :: [(Name, Type)] }
                    | Func      { decName :: Name, args :: [FuncArg], retType :: Maybe Type , body :: Expr }
                    deriving(Eq, Show)


data Opr2 = Sum
          | Sub
          | Mult
          | Div
          | Mod
          | Lt
          | LtEq
          | Gt
          | GtEq
          | Eq
          | NotEq
          | And
          | Not
          deriving(Eq, Show)

data Opr1 = Negation
          | Negative

-- Possible expressions. Remember, everything its an expression
data Expr   = ConstChar       { cVal :: String}
            | ConstString     { sVal :: String}
            | ConstInt        { iVal :: Int }
            | ConstFloat        { fVal :: Int }
            | ConstStruct     { structType :: Type, list :: [Expr] }
            | ConstTrue 
            | ConstFalse
            | ConstUnion      { unionType :: Type, tag :: Name, value :: Expr }
            | ConstUnit        -- cambiar const union y struct por instance
            | Id              { name :: Name}
            | Assign          { variable :: Name, value :: Expr}
            | StructAssign    { variable :: Name, tag :: Name, value :: Expr}
            | FunCall         { fname :: Name, actualArgs :: [Expr]}
            | For             { iteratorName :: Name, step :: Expr, start :: Expr, end :: Expr, cicBody :: Expr }
            | While           { cond :: Expr, cicBody :: Expr}
            | If              { cond :: Expr, accExpr :: Expr, failExpr :: Expr }
            | ExprBlock       { exprs :: [Expr] }
            | Return          { maybeExpr :: Maybe Expr }
            | Break           { maybeExpr :: Maybe Expr }
            | Continue        { maybeExpr :: Maybe Expr }
            | Declaration     { decl :: Declaration }
            | Op2             { op2 :: Opr2, opr1 :: Expr, opr2 :: Expr }
            | Op1             { op1 :: Opr1, opr :: Expr }
            | Array           { list :: [Expr] }
            | UnionTrying     { unionName :: Name, tag :: Name }
            | UnionUsing      { unionName :: Name, tag :: Name }
            | New             { typeName :: Type }
            | Delete          { ptrExpr :: Expr }
            deriving(Eq, Show)

-- Program data type     
newtype Program = Program{ decls :: [Declaration] }


---------- < show instances > -----------

{-
--instance Show Type where
--  show (Type) = "xd"

identShowType :: Type -> Int -> String
identShowType type_ ident = undefined

--instance Show FuncArg where
--  show = "xd"

identShowFuncArg :: FuncArg -> Int -> String
identShowFuncArg f ident = undefined

--instance Show Declaration where
--  show = "xd"

identShowDeclaration :: Declaration -> Int -> String
identShowDeclaration d ident = undefined

--instance Show BoolBinOpr where
--  show = "xd"

identShowBoolBinOpr :: BoolBinOpr -> Int -> String
identShowBoolBinOpr d ident = undefined

--instance Show OrdOpr where
--  show = "xd"

identShowOrdOpr :: OrdOpr -> Int -> String
identShowOrdOpr d ident = undefined

--instance Show EqOpr where
--  show = "xd"

identShowEqOpr :: EqOpr -> Int -> String
identShowEqOpr d ident = undefined

--instance Show BoolExpr where
--  show = "xd"

identShowBoolExpr :: BoolExpr -> Int -> String
identShowBoolExpr d ident = undefined

--instance Show NumBinOpr where
--  show = "xd"

identShowNumBinOpr :: NumBinOpr -> Int -> String
identShowNumBinOpr n ident = undefined

--instance Show NumUnOpr where
--  show = "xd"

identShowNumUnOpr :: NumUnOpr -> Int -> String
identShowNumUnOpr n ident = undefined

--instance Show NumExpr where
--  show = "xd"

identShowNumExpr :: NumExpr -> Int -> String
identShowNumExpr n ident = undefined

--instance Show Expr where
--  show = "xd"

identShowExpr :: Expr -> Int -> String
identShowExpr e ident = undefined

instance Show Program where
  show = identShowProgram

identShowProgram :: Program -> String
identShowProgram p = undefined
-}