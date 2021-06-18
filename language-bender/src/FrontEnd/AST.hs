{-
    Module defining AST data types and related functions
-}

module FrontEnd.AST where

import qualified FrontEnd.Utils as U

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
          deriving(Eq)

data Opr1 = Negation
          | Negative
          | UnitOperator
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
            | ConstUnit
            | ConstNull
            | Id              { name :: Name, position :: U.Position}
            | Assign          { variable :: Name, value :: Expr}
            | StructAssign    { variable :: Name, tag :: Name, value :: Expr}
            | StructAccess    { struct :: Expr, tag :: Name }
            | FunCall         { fname :: Name, actualArgs :: [Expr]}
            | For             { iteratorName :: Name, step :: Expr, start :: Expr, end :: Expr, cicBody :: Expr }
            | While           { cond :: Expr, cicBody :: Expr}
            | If              { cond :: Expr, accExpr :: Expr, failExpr :: Expr }
            | ExprBlock       { exprs :: [Expr] }
            | Return          { expr :: Expr }
            | Break           { expr :: Expr }
            | Continue        { expr :: Expr }
            | Declaration     { decl :: Declaration }
            | Op2             { op2 :: Opr2, opr1 :: Expr, opr2 :: Expr }
            | Op1             { op1 :: Opr1, opr :: Expr }
            | Array           { list :: [Expr] }
            | UnionTrying     { union :: Expr, tag :: Name }
            | UnionUsing      { union :: Expr, tag :: Name }
            | New             { typeName :: Type }
            | Delete          { ptrExpr :: Expr }
            | ArrayIndexing   { index :: Expr, arrId :: Name}
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

identShowOpr1 ident UnitOperator =
  replicate ident ' ' ++ "Unit Operator\n"

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


identShowFuncArg :: Int -> FuncArg -> String
identShowFuncArg ident (FuncArg nm t def) = "\n" ++
  replicate ident ' ' ++ "Func Arg: " ++ nm ++ " of type:\n"
  ++ identShowType (ident + 2) t 
  ++ (case def of
    Nothing -> ""
    (Just exp_) ->
      replicate ident ' ' ++ "with default value:\n" 
      ++ identShowExpr (ident + 2) exp_ )

identShowType :: Int -> Type -> String

identShowType ident (TFloat) = "\n" ++
  replicate ident ' ' ++ "Type: Float\n"

identShowType ident (TInt) = "\n" ++
  replicate ident ' ' ++ "Type: Int\n"

identShowType ident (TChar) = "\n" ++
  replicate ident ' ' ++ "Type: Char\n"

identShowType ident (TString) = "\n" ++
  replicate ident ' ' ++ "Type: String\n"

identShowType ident (TBool) = "\n" ++
  replicate ident ' ' ++ "Type: Bool\n"

identShowType ident (TArray arrt sz) = "\n" ++
  replicate ident ' ' ++ "Type: Array of size:\n"
  ++ identShowExpr (ident + 2) sz 
  ++ replicate ident ' ' ++ "Of type:\n"
  ++ identShowType (ident + 2) arrt

identShowType ident (TPtr t) = "\n" ++
  replicate ident ' ' ++ "Type: Pointer of:\n"
  ++ identShowType (ident + 2) t 

identShowType ident (TUnit) = "\n" ++
  replicate ident ' ' ++ "Type: Unit\n"

identShowType ident (TReference t) = "\n" ++
  replicate ident ' ' ++ "Type: Reference of:\n"
  ++ identShowType (ident + 2) t 

identShowType ident (CustomType nm) = "\n" ++
  replicate ident ' ' ++ "Type: " ++ nm ++ "\n"


identShowExpr :: Int -> Expr -> String

identShowExpr ident (ConstChar c) = "\n" ++
  replicate ident ' ' ++ "Literal Character: " ++ (show c) ++ "\n"

identShowExpr ident (ConstString s) = "\n" ++
  replicate ident ' ' ++ "Literal String: '" ++ s ++ "'\n"

identShowExpr ident (ConstInt n) = "\n" ++
  replicate ident ' ' ++ "Literal Int: '" ++ (show n) ++ "'\n"

identShowExpr ident (ConstFloat f) = "\n" ++
  replicate ident ' ' ++ "Literal Float: '" ++ (show f) ++ "'\n"

identShowExpr ident (ConstStruct t exps) = "\n" ++
  replicate ident ' ' ++ "Literal Struct:\n"
  ++ identShowType (ident + 2) t 
  ++ replicate ident ' ' ++ "with fields:\n"
  ++ concatMap (identShowExpr (ident + 2)) exps 

identShowExpr ident ConstTrue = "\n" ++
  replicate ident ' ' ++ "Literal Bool: True\n"

identShowExpr ident ConstFalse = "\n" ++
  replicate ident ' ' ++ "Literal Bool: False\n"

identShowExpr ident (ConstUnion t tag_ val) = "\n" ++
  replicate ident ' ' ++ "Literal Union:\n"
  ++ identShowType (ident + 2) t 
  ++ replicate ident ' ' ++ "with tag: " ++ tag_ ++ "\n"
  ++ replicate ident ' ' ++ "with value:\n"
  ++ identShowExpr (ident + 2) val 
identShowExpr ident ConstUnit = "\n" ++
  replicate ident ' ' ++ "Unit ()\n"

identShowExpr ident ConstNull = "\n" ++
  replicate ident ' ' ++ "Null\n"

identShowExpr ident (Id nm pos_) = "\n" ++
  replicate ident ' ' ++ "Identifier: " ++ nm ++ "\n"

identShowExpr ident (Assign nm val) = "\n" ++
  replicate ident ' ' ++ "Assignment: " ++ nm ++ " <- \n"
  ++ identShowExpr (ident + 2) val

identShowExpr ident (StructAssign var tag_ val) = "\n" ++
  replicate ident ' ' ++ "Struct Assignment: '" ++ var ++ "' with tag '" ++ tag_ ++ "' <-\n"
  ++ identShowExpr (ident + 2) val 

identShowExpr ident (StructAccess stru tag_) = "\n" ++
  replicate ident ' ' ++ "Struct Access: \n"
  ++ identShowExpr (ident + 2) stru
  ++ replicate ident ' ' ++ " with tag '" ++ tag_ ++ "'\n"

identShowExpr ident (FunCall fnm args_) = "\n" ++
  replicate ident ' ' ++ "Function/Procedure call: " ++ fnm ++ "\n"
  ++ replicate ident ' ' ++ "with arguments:\n"
  ++ concatMap (identShowExpr (ident + 2)) args_ 

identShowExpr ident (For it step_ start_ end_ bodyExp) = "\n" ++
  replicate ident ' ' ++ "For loop with iteration variable: " ++ it ++ "\n"
  ++ replicate ident ' ' ++ "with step of:\n"
  ++ identShowExpr (ident + 2) step_ 
  ++ replicate ident ' ' ++ "with start:\n"
  ++ identShowExpr (ident + 2) start_ 
  ++ replicate ident ' ' ++ "with end:\n" 
  ++ identShowExpr (ident + 2) end_ 
  ++ replicate ident ' ' ++ "with body:\n" 
  ++ identShowExpr (ident + 2) bodyExp

identShowExpr ident (While condition bodyExp) = "\n" ++
  replicate ident ' ' ++ "While loop with condition:\n"
  ++ identShowExpr (ident + 2) condition
  ++ replicate ident ' ' ++ "with body:\n" 
  ++ identShowExpr (ident + 2) bodyExp

identShowExpr ident (If condition exp1 exp2) = "\n" ++
  replicate ident ' ' ++ "'If' with condition:\n"
  ++ identShowExpr (ident + 2) condition
  ++ replicate ident ' ' ++ "with first expression:\n" 
  ++ identShowExpr (ident + 2) exp1
  ++ replicate ident ' ' ++ "with second expression:\n" 
  ++ identShowExpr (ident + 2) exp2

identShowExpr ident (ExprBlock exps) =
  concatMap (identShowExpr ident) exps

identShowExpr ident (Return expm) = "\n" ++
  replicate ident ' ' ++ "Return: \n"
  ++ identShowExpr (ident + 2) expm

identShowExpr ident (Break expm) = "\n" ++
  replicate ident ' ' ++ "Break: \n"
  ++ identShowExpr (ident + 2) expm

identShowExpr ident (Continue expm) = "\n" ++
  replicate ident ' ' ++ "Continue: \n"
  ++ identShowExpr (ident + 2) expm

identShowExpr ident (Declaration decl_) = "\n" ++
  identShowDeclaration ident decl_

identShowExpr ident (Op2 op lhs rhs) = "\n" ++
  replicate ident ' ' ++ "Binary Operator:\n"
  ++ identShowOpr2 (ident + 2) op
  ++ replicate ident ' ' ++ "with left hand side:\n"
  ++ identShowExpr (ident + 2) lhs 
  ++ replicate ident ' ' ++ "with right hand side:\n"
  ++ identShowExpr (ident + 2) rhs

identShowExpr ident (Op1 op opr_) = "\n" ++
  replicate ident ' ' ++ "Unary Operator:\n"
  ++ identShowOpr1 (ident + 2) op
  ++ replicate ident ' ' ++ "with operand:\n"
  ++ identShowExpr (ident + 2) opr_ 

identShowExpr ident (Array l) = "\n" ++
  replicate ident ' ' ++ "Array Literal with elements:\n"
  ++ concatMap (identShowExpr (ident + 2)) l

identShowExpr ident (UnionTrying u tag_) = "\n" ++
  replicate ident ' ' ++  "Check Union: \n"
  ++ identShowExpr (ident + 2) u
  ++ replicate ident ' ' ++ "with tag '" ++ tag_ ++ "'\n"

identShowExpr ident (UnionUsing u tag_) = "\n" ++
  replicate ident ' ' ++  "Access Union: \n"
  ++ identShowExpr (ident + 2) u
  ++ replicate ident ' ' ++ "with tag '" ++ tag_ ++ "'\n"

identShowExpr ident (New t) = "\n" ++
  replicate ident ' ' ++ "New statement with type:\n"
  ++ identShowType (ident + 2) t

identShowExpr ident (Delete pt) = "\n" ++
  replicate ident ' ' ++ "Delete:\n"
  ++ identShowExpr (ident + 2) pt

identShowExpr ident (ArrayIndexing exp_ arrNm) = "\n" ++
  replicate ident ' ' ++ "Access Array: "++ arrNm ++", at position:\n"
  ++ identShowExpr (ident + 2) exp_


identShowField :: Int -> (Name, Type) -> String
identShowField ident (name, t) = "\n" ++
  replicate ident ' ' ++ "field:" ++ name ++ "->\n"
  ++ identShowType (ident + 2) t

identShowDeclaration :: Int -> Declaration -> String

identShowDeclaration ident (Variable name varT initV isCst) = "\n" ++
  replicate ident ' ' ++ "Declaration of variable '" ++ name ++ "'\n"
  ++ replicate ident ' ' ++ "is Constant: " ++ (show isCst) ++ "\n"
  ++ (case initV of
    (Just v) ->
      replicate ident ' ' ++ "with a value of: \n"
      ++ identShowExpr (ident + 2) v
    Nothing  -> "")
  ++ (case varT of
    (Just t) ->
      replicate ident ' ' ++ "Of type:\n"
      ++ identShowType (ident + 2) t
    Nothing  -> "")
  

identShowDeclaration ident (Reference name refNm) = "\n" ++
  replicate ident ' ' ++ "Declaration of Reference with name '" 
  ++ name ++ "', referring '" ++ refNm ++ "'\n" 

identShowDeclaration ident (Union name fs) = "\n" ++
  replicate ident ' ' ++ "Declaration of Union '" ++ name ++ "'\n"
  ++ replicate ident ' ' ++ "with fields: \n"
  ++ concatMap (identShowField (ident + 2)) fs

identShowDeclaration ident (Struct name fs) = 
  replicate ident ' ' ++ "Declaration of Struct '" ++ name ++ "'\n"
  ++ replicate ident ' ' ++ "with fields: \n"
  ++ concatMap (identShowField (ident + 2)) fs

identShowDeclaration ident (Func name param retT bodyExp) = "\n" ++
  case retT of    
    (Just TUnit) ->
      replicate ident ' ' ++ "Declaration of Procedure '" ++ name ++ "'\n"
      ++ replicate ident ' ' ++ "with parameters: \n"
      ++ concatMap (identShowFuncArg (ident + 2)) param
      ++ replicate ident ' ' ++ "with body:\n"
      ++ identShowExpr (ident + 2) bodyExp
    (Just t)->
      replicate ident ' ' ++ "Declaration of Function '" ++ name ++ "'\n"
      ++ replicate ident ' ' ++ "with return type:\n"
      ++ identShowType (ident + 2) t ++ "\n"
      ++ replicate ident ' ' ++ "with parameters: \n"
      ++ concatMap (identShowFuncArg (ident + 2)) param
      ++ replicate ident ' ' ++ "with body:\n"
      ++ identShowExpr (ident + 2) bodyExp
    Nothing -> 
      replicate ident ' ' ++ "Declaration of Function '" ++ name ++ "'\n"
      ++ replicate ident ' ' ++ "with return type: Inferred\n"
      ++ replicate ident ' ' ++ "with parameters: \n"
      ++ concatMap (identShowFuncArg (ident + 2)) param
      ++ replicate ident ' ' ++ "with body:\n"
      ++ identShowExpr (ident + 2) bodyExp


identShowProgram :: Program -> String
identShowProgram program = --"~  AST  ~\n"
  concatMap (identShowDeclaration 2) (decls program)

