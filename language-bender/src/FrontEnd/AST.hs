{-
    Module defining AST data types and related functions
-}

module FrontEnd.AST where

import qualified FrontEnd.Utils as U
import Data.List as L

-- | Type Definition
data Type = TFloat --32
          | TInt --32
          | TChar --8
          | TBool --8
          | TArray { arrType :: Type, size :: Expr }
          | TPtr   { ptrType :: Type } --32  
          | TUnit 
          | TReference { refType :: Type } --32 
          | CustomType { tName :: U.Name, scope :: Int }
          | TypeError
          | TVoid
          deriving(Eq)

data FuncArg = FuncArg{ argName :: U.Name, argType :: Type, defaultVal :: Maybe Expr, _declScope :: Int } deriving(Eq)

-- | Declaration of new things
data Declaration    = Variable  { decName :: U.Name, varType :: Type, initVal :: Maybe Expr, isConst :: Bool, declScope :: Int }
                    | Reference { decName :: U.Name, refName :: U.Name, declScope :: Int }
                    | Union     { decName :: U.Name, fields :: [(U.Name, Type)] , width :: Int, align :: Int, declScope :: Int, fieldScope :: Int }
                    | Struct    { decName :: U.Name, fields :: [(U.Name, Type)] , width :: Int, align :: Int, declScope :: Int, fieldScope :: Int }
                    | Func      { decName :: U.Name, args :: [FuncArg], retType :: Type , body :: Expr, declScope :: Int, baseStackSize :: Int}
                    deriving(Eq)

-- | Binary Operators
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

-- | Unary operators
data Opr1 = Negation
          | Negative
          | UnitOperator
          | DerefOperator
          deriving(Eq)

-- | Possible expressions. Remember, everything its an expression
data Expr   = ConstChar       { cVal :: String, expType :: Type }
            | LiteralString   { sVal :: String, expType :: Type, offset :: Int }
            | ConstInt        { iVal :: Int, expType :: Type }
            | ConstFloat      { fVal :: Float, expType :: Type }
            | ConstTrue       { expType :: Type }
            | ConstFalse      { expType :: Type }
            | LiteralStruct   { structName :: U.Name, list :: [Expr], expType :: Type, offset :: Int }
            | LiteralUnion    { unionName :: U.Name, tag :: U.Name, value :: Expr, expType :: Type, offset :: Int }
            | ConstUnit       { expType :: Type }
            | ConstNull       { expType :: Type }
            | Id              { name :: U.Name, position :: U.Position, expType :: Type, declScope_ :: Int}
            | Assign          { variable :: U.Name, value :: Expr, expType :: Type, declScope_ :: Int}
            | StructAssign    { struct :: Expr, tag :: U.Name, value :: Expr, expType :: Type}
            | StructAccess    { struct :: Expr, tag :: U.Name, expType :: Type }
            | FunCall         { fname :: U.Name, actualArgs :: [Expr], expType :: Type, declScope_ :: Int}
            | For             { iteratorSym :: Declaration, step :: Expr, start :: Expr, end :: Expr, cicBody :: Expr, expType :: Type }
            | While           { cond :: Expr, cicBody :: Expr, expType :: Type}
            | If              { cond :: Expr, accExpr :: Expr, failExpr :: Expr, expType :: Type }
            | ExprBlock       { exprs :: [Expr], expType :: Type }
            | Return          { expr :: Expr, expType :: Type }
            | Break           { expr :: Expr, expType :: Type }
            | Continue        { expr :: Expr, expType :: Type }
            | Declaration     { decl :: Declaration, expType :: Type }
            | Op2             { op2 :: Opr2, opr1 :: Expr, opr2 :: Expr, expType :: Type }
            | Op1             { op1 :: Opr1, opr :: Expr, expType :: Type }
            | Array           { list :: [Expr], expType :: Type, offset :: Int }
            | UnionTrying     { union :: Expr, tag :: U.Name, expType :: Type }
            | UnionUsing      { union :: Expr, tag :: U.Name, expType :: Type }
            | New             { typeName :: Type, expType :: Type }
            | Delete          { ptrExpr :: Expr, expType :: Type }
            | DerefAssign     { ptrExpr :: Expr, value :: Expr, expType :: Type }
            | ArrayIndexing   { index :: Expr, expr :: Expr, expType :: Type }
            | ArrayAssign     { index :: Expr, arrayExpr :: Expr, value :: Expr, expType :: Type }
            deriving(Eq)

-- | Program data type     
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

identShowOpr1 ident DerefOperator =
  replicate ident ' ' ++ "Dereference Operator\n"

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
identShowFuncArg ident (FuncArg nm t def _) = "\n" ++
  replicate ident ' ' ++ "Func Arg: " ++ nm ++ " of type: "
  ++ ( init . tail $ identShowType (ident + 2) t) 
  ++ (case def of
    Nothing -> ""
    (Just exp_) ->
      replicate ident ' ' ++ "with default value: " 
      ++ identShowExpr (ident + 2) exp_ )

identShowType :: Int -> Type -> String

identShowType ident TFloat = "\n" ++
  replicate ident ' ' ++ "Type: Float\n"

identShowType ident TInt = "\n" ++
  replicate ident ' ' ++ "Type: Int\n"

identShowType ident TChar = "\n" ++
  replicate ident ' ' ++ "Type: Char\n"

identShowType ident TBool = "\n" ++
  replicate ident ' ' ++ "Type: Bool\n"

identShowType ident (TArray arrt sz) = "\n" ++
  replicate ident ' ' ++ "Type: Array of size:\n"
  ++ identShowExpr (ident + 2) sz 
  ++ "\n" ++ replicate ident ' ' ++ "Of type:\n"
  ++ identShowType (ident + 2) arrt

identShowType ident (TPtr t) = "\n" ++
  replicate ident ' ' ++ "Type: Pointer of:\n"
  ++ identShowType (ident + 2) t 

identShowType ident TUnit = "\n" ++
  replicate ident ' ' ++ "Type: Unit\n"

identShowType ident (TReference t) = "\n" ++
  replicate ident ' ' ++ "Type: Reference of:\n"
  ++ identShowType (ident + 2) t 

identShowType ident (CustomType nm scope) = "\n" ++
  replicate ident ' ' ++ "Type: " ++ nm ++ "[" ++ show scope ++ "]" ++ "\n"

identShowType ident TypeError = "\n" ++
  replicate ident ' ' ++ "Type: Type Error\n"

identShowType ident TVoid = "\n" ++
  replicate ident ' ' ++ "Type: Void\n"


identShowExpr :: Int -> Expr -> String

identShowExpr ident (ConstChar c _) = "\n" ++
  replicate ident ' ' ++ "Literal Character: " ++ show c ++ "\n"

identShowExpr ident (LiteralString s _ _) = "\n" ++
  replicate ident ' ' ++ "Literal String: '" ++ s ++ "'\n"

identShowExpr ident (ConstInt n _) = "\n" ++
  replicate ident ' ' ++ "Literal Int: '" ++ show n ++ "'\n"

identShowExpr ident (ConstFloat f _) = "\n" ++
  replicate ident ' ' ++ "Literal Float: '" ++ show f ++ "'\n"

identShowExpr ident (LiteralStruct stru_id exps _ _) = "\n" ++
  replicate ident ' ' ++ "Literal Struct: '"++ stru_id ++ "'\n"
  ++ replicate ident ' ' ++ "with fields:\n"
  ++ concatMap (identShowExpr (ident + 2)) exps 

identShowExpr ident (ConstTrue _) = "\n" ++
  replicate ident ' ' ++ "Literal Bool: True\n"

identShowExpr ident (ConstFalse _) = "\n" ++
  replicate ident ' ' ++ "Literal Bool: False\n"

identShowExpr ident (LiteralUnion union_id tag_ val _ _) = "\n" ++
  replicate ident ' ' ++ "Literal Union: '" ++ union_id ++ "'\n"
  ++ replicate ident ' ' ++ "with tag: " ++ tag_ ++ "\n"
  ++ "\n" ++ replicate ident ' ' ++ "with value:\n"
  ++ identShowExpr (ident + 2) val 

identShowExpr ident (ConstUnit _) = "\n" ++
  replicate ident ' ' ++ "Unit ()\n"

identShowExpr ident (ConstNull _) = "\n" ++
  replicate ident ' ' ++ "Null\n"

identShowExpr ident (Id nm pos_ _ _) = "\n" ++
  replicate ident ' ' ++ "Identifier: " ++ nm ++ "\n"

identShowExpr ident (Assign nm val _ _) = "\n" ++
  replicate ident ' ' ++ "Assignment: " ++ nm ++ " <- \n"
  ++ identShowExpr (ident + 2) val

identShowExpr ident (StructAssign stru tag_ val _) = "\n" ++
  replicate ident ' ' ++ "Struct Assignment: \n"
  ++ identShowExpr (ident + 2) stru
  ++ "\n" ++ replicate ident ' ' ++ "With tag '" ++ tag_ ++ "' <-\n"
  ++ identShowExpr (ident + 2) val 

identShowExpr ident (StructAccess stru tag_ _) = "\n" ++
  replicate ident ' ' ++ "Struct Access: \n"
  ++ identShowExpr (ident + 2) stru
  ++ "\n" ++ replicate ident ' ' ++ " with tag '" ++ tag_ ++ "'\n"

identShowExpr ident (FunCall fnm args_ _ _) = "\n" ++
  replicate ident ' ' ++ "Function/Procedure call: " ++ fnm ++ "\n"
  ++ "\n" ++ replicate ident ' ' ++ "with arguments:\n"
  ++ concatMap (identShowExpr (ident + 2)) args_ 

identShowExpr ident (For Variable {decName=it, declScope=scope} step_ start_ end_ bodyExp _) = "\n" ++
  replicate ident ' ' ++ "For loop with iteration variable: " ++ it ++ "@" ++  show scope++"\n"
  ++ "\n" ++ replicate ident ' ' ++ "with step of:\n"
  ++ identShowExpr (ident + 2) step_ 
  ++ "\n" ++ replicate ident ' ' ++ "with start:\n"
  ++ identShowExpr (ident + 2) start_ 
  ++ "\n" ++ replicate ident ' ' ++ "with end:\n" 
  ++ identShowExpr (ident + 2) end_ 
  ++ "\n" ++ replicate ident ' ' ++ "with body:\n" 
  ++ identShowExpr (ident + 2) bodyExp

identShowExpr ident (While condition bodyExp _) = "\n" ++
  replicate ident ' ' ++ "While loop with condition:\n"
  ++ identShowExpr (ident + 2) condition
  ++ "\n" ++ replicate ident ' ' ++ "with body:\n" 
  ++ identShowExpr (ident + 2) bodyExp

identShowExpr ident (If condition exp1 exp2 _) = "\n" ++
  replicate ident ' ' ++ "'If' with condition:\n"
  ++ identShowExpr (ident + 2) condition
  ++ "\n" ++ replicate ident ' ' ++ "with first expression:\n" 
  ++ identShowExpr (ident + 2) exp1
  ++ "\n" ++ replicate ident ' ' ++ "with second expression:\n" 
  ++ identShowExpr (ident + 2) exp2

identShowExpr ident (ExprBlock exps _) =
  concatMap (identShowExpr ident) exps

identShowExpr ident (Return expm _) = "\n" ++
  replicate ident ' ' ++ "Return: \n"
  ++ identShowExpr (ident + 2) expm

identShowExpr ident (Break expm _) = "\n" ++
  replicate ident ' ' ++ "Break: \n"
  ++ identShowExpr (ident + 2) expm

identShowExpr ident (Continue expm _) = "\n" ++
  replicate ident ' ' ++ "Continue: \n"
  ++ identShowExpr (ident + 2) expm

identShowExpr ident (Declaration decl_ _) = "\n" ++
  identShowDeclaration ident decl_

identShowExpr ident (Op2 op lhs rhs _) = "\n" ++
  replicate ident ' ' ++ "Binary Operator:\n"
  ++ identShowOpr2 (ident + 2) op
  ++ "\n" ++ replicate ident ' ' ++ "with left hand side:\n"
  ++ identShowExpr (ident + 2) lhs 
  ++ "\n" ++ replicate ident ' ' ++ "with right hand side:\n"
  ++ identShowExpr (ident + 2) rhs

identShowExpr ident (Op1 op opr_ _) = "\n" ++
  replicate ident ' ' ++ "Unary Operator:\n"
  ++ identShowOpr1 (ident + 2) op
  ++ "\n" ++ replicate ident ' ' ++ "with operand:\n"
  ++ identShowExpr (ident + 2) opr_ 

identShowExpr ident (Array l _ _) = "\n" ++
  replicate ident ' ' ++ "Array Literal with elements:\n"
  ++ concatMap (identShowExpr (ident + 2)) l

identShowExpr ident (UnionTrying u tag_ _) = "\n" ++
  replicate ident ' ' ++  "Check Union: \n"
  ++ identShowExpr (ident + 2) u
  ++ "\n" ++ replicate ident ' ' ++ "with tag '" ++ tag_ ++ "'\n"

identShowExpr ident (UnionUsing u tag_ _) = "\n" ++
  replicate ident ' ' ++  "Access Union: \n"
  ++ identShowExpr (ident + 2) u
  ++ "\n" ++ replicate ident ' ' ++ "with tag '" ++ tag_ ++ "'\n"

identShowExpr ident (New t _) = "\n" ++
  replicate ident ' ' ++ "New statement with type:\n"
  ++ identShowType (ident + 2) t

identShowExpr ident (Delete pt _) = "\n" ++
  replicate ident ' ' ++ "Delete:\n"
  ++ identShowExpr (ident + 2) pt

identShowExpr ident (DerefAssign{ptrExpr=pt,value=val}) = "\n" ++
  replicate ident ' ' ++ "Assign to deref pointer:\n"
  ++ identShowExpr (ident + 2) pt
  ++ "\n" ++ replicate ident ' ' ++"the value:\n"
  ++ identShowExpr (ident + 2) val

identShowExpr ident (ArrayIndexing exp_ arrNm _) = "\n" ++
  replicate ident ' ' ++ "Access Array: "
  ++ identShowExpr (ident + 2) arrNm ++ "\n"
  ++ replicate ident ' ' ++"At position:\n"
  ++ identShowExpr (ident + 2) exp_

identShowExpr ident (ArrayAssign exp_ arrNm val _) = "\n" ++
  replicate ident ' ' ++ "Assign Array: "
  ++ identShowExpr (ident + 2) arrNm ++ "\n"
  ++ replicate ident ' ' ++"At position:\n"
  ++ identShowExpr (ident + 2) exp_
  ++ replicate ident ' ' ++ "the value:\n"
  ++ identShowExpr (ident + 2) val

identShowField :: Int -> (U.Name, Type) -> String
identShowField ident (name, t) = "\n" ++
  replicate ident ' ' ++ "field:" ++ name ++ "->\n"
  ++ identShowType (ident + 2) t

identShowDeclaration :: Int -> Declaration -> String

identShowDeclaration ident (Variable name varT initV isCst _) = "\n" ++
  replicate ident ' ' ++ "Declaration of variable '" ++ name ++ "'\n"
  ++ replicate ident ' ' ++ "is Constant: " ++ show isCst ++ "\n"
  ++ (case initV of 
    (Just v) ->
      "\n" ++ replicate ident ' ' ++ "with a value of: \n"
      ++ identShowExpr (ident + 2) v
    Nothing  -> "")
  ++  "\n" ++ replicate ident ' ' ++ "Of type:\n"
      ++ identShowType (ident + 2) varT

identShowDeclaration ident (Reference name refNm _) = "\n" ++
  replicate ident ' ' ++ "Declaration of Reference with name '" 
  ++ name ++ "', referring '" ++ refNm ++ "'\n" 

identShowDeclaration ident (Union name fs _ _ _ _) = "\n" ++
  replicate ident ' ' ++ "Declaration of Union '" ++ name ++ "'\n"
  ++ "\n" ++ replicate ident ' ' ++ "with fields: \n"
  ++ concatMap (identShowField (ident + 2)) fs

identShowDeclaration ident (Struct name fs _ _ _ _) = 
  replicate ident ' ' ++ "Declaration of Struct '" ++ name ++ "'\n"
  ++ "\n" ++ replicate ident ' ' ++ "with fields: \n"
  ++ concatMap (identShowField (ident + 2)) fs

identShowDeclaration ident (Func name param retT bodyExp _ stackSize) = "\n" ++
  case retT of    
    TUnit ->
      replicate ident ' ' ++ "Declaration of Procedure '" ++ name ++ "' with stack size of "++ show stackSize ++ "\n"
      ++ replicate ident ' ' ++ "with parameters: \n"
      ++ concatMap (identShowFuncArg (ident + 2)) param
      ++ "\n" ++ replicate ident ' ' ++ "with body:\n"
      ++ identShowExpr (ident + 2) bodyExp
    t->
      replicate ident ' ' ++ "Declaration of Function '" ++ name ++ "' with stack size of "++ show stackSize ++ "\n"
      ++ replicate ident ' ' ++ "with return type:\n"
      ++ identShowType (ident + 2) t ++ "\n"
      ++ "\n" ++ replicate ident ' ' ++ "with parameters: \n"
      ++ concatMap (identShowFuncArg (ident + 2)) param
      ++ "\n" ++ replicate ident ' ' ++ "with body:\n"
      ++ identShowExpr (ident + 2) bodyExp

identShowProgram :: Program -> String
identShowProgram program = --"~  AST  ~\n"
  concatMap (identShowDeclaration 2) (decls program)


simplePrint :: Type -> String
simplePrint TFloat               = "Float"
simplePrint TInt                 = "Int"
simplePrint TChar                = "Char"
simplePrint TBool                = "Bool"
simplePrint (TArray aType sz)    = "Array of (" ++ simplePrint aType ++ ") of size " ++ show sz
simplePrint (TPtr pType)         = "Pointer of (" ++ simplePrint pType ++ ")"
simplePrint TUnit                = "Unit"
simplePrint (TReference rType)   = "Reference of (" ++ simplePrint rType ++ ")"
simplePrint (CustomType tName _) = "Custom type '" ++ tName ++ "'"
simplePrint TypeError            = "Type error"
simplePrint TVoid                = "Void"

simpleListPrint :: [Type] -> String
simpleListPrint ts = tsPrinted
  where
    tsPrinted = L.intercalate ", " (map simplePrint ts) 

getTypeId :: Type -> U.Name
getTypeId TFloat = "water"
getTypeId TInt = "air"
getTypeId TChar = "earth"
getTypeId TBool = "fire"
getTypeId TPtr{} = "art"
getTypeId TReference{} = "reincarnation"
getTypeId (CustomType name _) = name
getTypeId _ = ""