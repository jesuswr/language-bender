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
          deriving(Eq)

data FuncArg = FuncArg{ argName :: Name, argType :: Type, defaultVal :: Maybe Expr } deriving(Eq)

-- Declaration of new things
data Declaration    = Variable  { decName :: Name, varType ::  Maybe Type, initVal :: Maybe Expr, isConst :: Bool }
                    | Reference { decName :: Name, refName :: Name }
                    | Union     { decName :: Name, fields :: [(Name, Type)] }
                    | Struct    { decName :: Name, fields :: [(Name, Type)] }
                    | Func      { decName :: Name, args :: [FuncArg], retType :: Maybe Type , body :: Expr }
                    deriving(Eq)


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
          | Or
          | Not
          deriving(Eq)

data Opr1 = Negation
          | Negative
          deriving(Eq)

-- Possible expressions. Remember, everything its an expression
data Expr   = ConstChar       { cVal :: String}
            | ConstString     { sVal :: String}
            | ConstInt        { iVal :: Int }
            | ConstFloat      { fVal :: Float }
            | ConstStruct     { structType :: Type, list :: [Expr] }
            | ConstTrue 
            | ConstFalse
            | ConstUnion      { unionType :: Type, tag :: Name, value :: Expr }
            | ConstUnit        -- cambiar const union y struct por instance/literal
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
            deriving(Eq)

-- Program data type     
newtype Program = Program{ decls :: [Declaration] } deriving(Eq)


---------- < show instances > -----------

instance Show Program where
  show = identShowProgram

instance Show Expr where
  show = identShowExpr 0

instance Show Opr1 where
  show = identShowOpr1 0

instance Show Opr2 where
  show = identShowOpr2 0

instance Show Declaration where
  show = identShowDeclaration 0

instance Show FuncArg where
  show = identShowFuncArg 0

instance Show Type where
  show = identShowType 0

-----------------------------------------


identShowOpr1 :: Int -> Opr1 -> String

identShowOpr1 ident Negation = 
  replicate ident ' ' ++ "Negation\n"

identShowOpr1 ident Negative =
  replicate ident ' ' ++ "Negative\n"

identShowOpr2 :: Int -> Opr2 -> String

identShowOpr2 ident Sum =
  replicate ident ' ' ++ "Sum\n"

identShowOpr2 ident Sub =
  replicate ident ' ' ++ "Subtraction\n"

identShowOpr2 ident Mult =
  replicate ident ' ' ++ "Multiplication\n"

identShowOpr2 ident Div =
  replicate ident ' ' ++ "Divition\n"

identShowOpr2 ident Mod =
  replicate ident ' ' ++ "Modulo\n"

identShowOpr2 ident Lt =
  replicate ident ' ' ++ "Less than\n"

identShowOpr2 ident LtEq =
  replicate ident ' ' ++ "Less than or equal\n"

identShowOpr2 ident Gt =
  replicate ident ' ' ++ "Greater than\n"

identShowOpr2 ident GtEq =
  replicate ident ' ' ++ "Greater than or equal\n"

identShowOpr2 ident Eq =
  replicate ident ' ' ++ "Equal\n"

identShowOpr2 ident NotEq =
  replicate ident ' ' ++ "Not equal\n"

identShowOpr2 ident And =
  replicate ident ' ' ++ "And\n"

identShowOpr2 ident Or =
  replicate ident ' ' ++ "Or\n"

identShowOpr2 ident Not =
  replicate ident ' ' ++ "Not\n"


identShowFuncArg :: Int -> FuncArg -> String
identShowFuncArg ident (FuncArg nm t def) =
  replicate ident ' ' ++ "Func Arg: " ++ nm ++ " of type:\n"
  ++ identShowType (ident + 2) t ++ "\n"
  ++ (case def of
    Nothing -> ""
    (Just exp_) ->
      replicate ident ' ' ++ "with default value:\n" 
      ++ identShowExpr (ident + 2) exp_ ++ "\n")

identShowType :: Int -> Type -> String

identShowType ident (TFloat) =
  replicate ident ' ' ++ "Type: Float\n"

identShowType ident (TInt) =
  replicate ident ' ' ++ "Type: Int\n"

identShowType ident (TChar) =
  replicate ident ' ' ++ "Type: Char\n"

identShowType ident (TString) =
  replicate ident ' ' ++ "Type: String\n"

identShowType ident (TBool) =
  replicate ident ' ' ++ "Type: Bool\n"

identShowType ident (TArray arrt sz) =
  replicate ident ' ' ++ "Type: Array of size:\n"
  ++ identShowExpr (ident + 2) sz ++ "\n"
  ++ replicate ident ' ' ++ "Of type:\n"
  ++ identShowType (ident + 2) arrt ++ "\n"

identShowType ident (TPtr t) =
  replicate ident ' ' ++ "Type: Pointer of:\n"
  ++ identShowType (ident + 2) t ++ "\n"

identShowType ident (TUnit) =
  replicate ident ' ' ++ "Type: Unit\n"

identShowType ident (TReference t) =
  replicate ident ' ' ++ "Type: Reference of:\n"
  ++ identShowType (ident + 2) t ++ "\n"

identShowType ident (CustomType nm) =
  replicate ident ' ' ++ "Type: " ++ nm ++ "\n"


identShowExpr :: Int -> Expr -> String

identShowExpr ident (ConstChar c) =
  replicate ident ' ' ++ "Literal Character: '" ++ (show c) ++ "'\n"

identShowExpr ident (ConstString s) =
  replicate ident ' ' ++ "Literal String: '" ++ s ++ "'\n"

identShowExpr ident (ConstInt n) =
  replicate ident ' ' ++ "Literal Int: '" ++ (show n) ++ "'\n"

identShowExpr ident (ConstFloat f) =
  replicate ident ' ' ++ "Literal Float: '" ++ (show f) ++ "'\n"

identShowExpr ident (ConstStruct t exps) =
  replicate ident ' ' ++ "Literal Struct:\n"
  ++ identShowType (ident + 2) t ++ "\n"
  ++ replicate ident ' ' ++ "with fields:\n"
  ++ concatMap (identShowExpr (ident + 2)) exps ++ "\n"

identShowExpr ident (ConstTrue) =
  replicate ident ' ' ++ "Literal Bool: True\n"

identShowExpr ident (ConstFalse) =
  replicate ident ' ' ++ "Literal Bool: False\n"

identShowExpr ident (ConstUnion t tag_ val) =
  replicate ident ' ' ++ "Literal Union:\n"
  ++ identShowType (ident + 2) t ++ "\n"
  ++ replicate ident ' ' ++ "with tag: " ++ tag_ ++ "\n"
  ++ replicate ident ' ' ++ "with fields:\n"
  ++ identShowExpr (ident + 2) val ++ "\n"

identShowExpr ident (ConstUnit) =
  replicate ident ' ' ++ "Unit ()\n"

identShowExpr ident (Id nm) =
  replicate ident ' ' ++ "Identifier: " ++ nm ++ "\n"

identShowExpr ident (Assign nm val) =
  replicate ident ' ' ++ "Assignment: " ++ nm ++ " <- \n"
  ++ identShowExpr (ident + 2) val ++ "\n" 

identShowExpr ident (StructAssign var tag_ val) =
  replicate ident ' ' ++ "Struct Assignment: '" ++ var ++ "' with tag '" ++ tag_ ++ "' <-\n"
  ++ identShowExpr (ident + 2) val ++ "\n" 

identShowExpr ident (FunCall fnm args_) =
  replicate ident ' ' ++ "Function/Procedure call: " ++ fnm ++ "\n"
  ++ replicate ident ' ' ++ "with arguments:\n"
  ++ concatMap (identShowExpr (ident + 2)) args_ ++ "\n"

identShowExpr ident (For it step_ start_ end_ bodyExp) =
  replicate ident ' ' ++ "For loop with iteration variable: " ++ it ++ "\n"
  ++ replicate ident ' ' ++ "with step of:\n"
  ++ identShowExpr (ident + 2) step_ ++ "\n"
  ++ replicate ident ' ' ++ "with start:\n"
  ++ identShowExpr (ident + 2) start_ ++ "\n"
  ++ replicate ident ' ' ++ "with end:\n" 
  ++ identShowExpr (ident + 2) end_ ++ "\n"
  ++ replicate ident ' ' ++ "with body:\n" 
  ++ identShowExpr (ident + 2) bodyExp ++ "\n"

identShowExpr ident (While condition bodyExp) =
  replicate ident ' ' ++ "While loop with condition:\n"
  ++ identShowExpr (ident + 2) condition ++ "\n"
  ++ replicate ident ' ' ++ "with body:\n" 
  ++ identShowExpr (ident + 2) bodyExp ++ "\n"

identShowExpr ident (If condition exp1 exp2) =
  replicate ident ' ' ++ "'If' with condition:\n"
  ++ identShowExpr (ident + 2) condition ++ "\n"
  ++ replicate ident ' ' ++ "with first expression:\n" 
  ++ identShowExpr (ident + 2) exp1 ++ "\n"
  ++ replicate ident ' ' ++ "with second expression:\n" 
  ++ identShowExpr (ident + 2) exp2 ++ "\n"

identShowExpr ident (ExprBlock exps) =
  concatMap (identShowExpr ident) exps ++ "\n"

identShowExpr ident (Return expm) =
  case expm of
    (Just exp_) -> 
      replicate ident ' ' ++ "Return: \n"
      ++ identShowExpr (ident + 2) exp_ ++ "\n"
    Nothing ->
      replicate ident ' ' ++ "Return ()\n"

identShowExpr ident (Break expm) =
  case expm of
    (Just exp_) -> 
      replicate ident ' ' ++ "Break: \n"
      ++ identShowExpr (ident + 2) exp_ ++ "\n"
    Nothing ->
      replicate ident ' ' ++ "Break ()\n"

identShowExpr ident (Continue expm) =
  case expm of
    (Just exp_) -> 
      replicate ident ' ' ++ "Continue: \n"
      ++ identShowExpr (ident + 2) exp_ ++ "\n"
    Nothing ->
      replicate ident ' ' ++ "Continue ()\n"

identShowExpr ident (Declaration decl_) =
  identShowDeclaration ident decl_

identShowExpr ident (Op2 op lhs rhs) =
  replicate ident ' ' ++ "Binary Operator:\n"
  ++ identShowOpr2 (ident + 2) op ++ "\n"
  ++ replicate ident ' ' ++ "with left hand side:\n"
  ++ identShowExpr (ident + 2) lhs ++ "\n" 
  ++ replicate ident ' ' ++ "with right hand side:\n"
  ++ identShowExpr (ident + 2) rhs ++ "\n" 

identShowExpr ident (Op1 op opr_) =
  replicate ident ' ' ++ "Unary Operator:\n"
  ++ identShowOpr1 (ident + 2) op ++ "\n"
  ++ replicate ident ' ' ++ "with operand:\n"
  ++ identShowExpr (ident + 2) opr_ ++ "\n" 

identShowExpr ident (Array l) =
  replicate ident ' ' ++ "Array Literal with elements:\n"
  ++ concatMap (identShowExpr (ident + 2)) l ++ "\n"

identShowExpr ident (UnionTrying nm tag_) =
  replicate ident ' ' ++  "Check Union '" ++ nm ++ "'\n"
  ++ replicate ident ' ' ++ "with tag '" ++ tag_ ++ "'\n"

identShowExpr ident (UnionUsing nm tag_) =
  replicate ident ' ' ++  "Access Union '" ++ nm ++ "'\n"
  ++ replicate ident ' ' ++ "with tag '" ++ tag_ ++ "'\n"

identShowExpr ident (New t) =
  replicate ident ' ' ++ "New statement with type:\n"
  ++ identShowType (ident + 2) t ++ "\n"

identShowExpr ident (Delete pt) =
  replicate ident ' ' ++ "Delete:\n"
  ++ identShowExpr (ident + 2) pt ++ "\n"

identShowField :: Int -> (Name, Type) -> String
identShowField ident (name, t) =
  replicate ident ' ' ++ "field:" ++ name ++ "->\n"
  ++ identShowType (ident + 2) t ++ "\n"


identShowDeclaration :: Int -> Declaration -> String

identShowDeclaration ident (Variable name varT initV isCst) =
  replicate ident ' ' ++ "Declaration of variable '" ++ name ++ "'\n"
  ++ replicate ident ' ' ++ "is Constant: " ++ (show isCst) ++ "\n"
  ++ (case initV of
    (Just v) ->
      replicate ident ' ' ++ "with a value of: \n"
      ++ identShowExpr (ident + 2) v ++ "\n"
    Nothing  -> "")
  ++ (case varT of
    (Just t) ->
      replicate ident ' ' ++ "Of type:\n"
      ++ identShowType (ident + 2) t ++ "\n"
    Nothing  -> "")
  

identShowDeclaration ident (Reference name refNm) =
  replicate ident ' ' ++ "Declaration of Reference with name '" 
  ++ name ++ "', referring '" ++ refNm ++ "'\n" 

identShowDeclaration ident (Union name fs) =
  replicate ident ' ' ++ "Declaration of Union '" ++ name ++ "'\n"
  ++ replicate ident ' ' ++ "with fields: \n"
  ++ concatMap (identShowField (ident + 2)) fs ++ "\n"

identShowDeclaration ident (Struct name fs) = 
  replicate ident ' ' ++ "Declaration of Struct '" ++ name ++ "'\n"
  ++ replicate ident ' ' ++ "with fields: \n"
  ++ concatMap (identShowField (ident + 2)) fs ++ "\n"

identShowDeclaration ident (Func name param retT bodyExp) =
  case retT of
    (Just t)->
      replicate ident ' ' ++ "Declaration of Function '" ++ name ++ "'\n"
      ++ replicate ident ' ' ++ "with return type:\n"
      ++ identShowType (ident + 2) t ++ "\n"
      ++ replicate ident ' ' ++ "with parameters: \n"
      ++ concatMap (identShowFuncArg (ident + 2)) param
      ++ replicate ident ' ' ++ "with body:\n"
      ++ identShowExpr (ident + 2) bodyExp ++ "\n"
    Nothing -> 
      replicate ident ' ' ++ "Declaration of Procedure '" ++ name ++ "'\n"
      ++ replicate ident ' ' ++ "with parameters: \n"
      ++ concatMap (identShowFuncArg (ident + 2)) param
      ++ replicate ident ' ' ++ "with body:\n"
      ++ identShowExpr (ident + 2) bodyExp ++ "\n"


identShowProgram :: Program -> String
identShowProgram program = "~  AST  ~\n"
  ++ concatMap (identShowDeclaration 2) (decls program)
  ++ "\n"

