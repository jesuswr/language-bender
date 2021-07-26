{

module FrontEnd.Parser where

-- <Language Bender Imports> ------------------------------------
import qualified FrontEnd.ParserCheck as P
import qualified FrontEnd.Tokens as TK
import qualified FrontEnd.AST    as AST
import qualified FrontEnd.SymTable      as ST
import qualified FrontEnd.Utils         as U
import qualified FrontEnd.Errors  as E
import qualified FrontEnd.StaticErrors  as SE

-- <Utility Data types> -----------------------------------------
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad     as M
import Data.Maybe(isNothing, maybe, fromMaybe, isJust, fromJust)

}

%name parseTokens
%tokentype { TK.Token }
%error { parseError }
%monad { P.ParserState }
%expect 128

%token
    bender              { TK.Token _ TK.TKbender }
    of                  { TK.Token _ TK.TKof }
    eternal             { TK.Token _ TK.TKeternal }
    '&'                 { TK.Token _ TK.TKReference }
    is                  { TK.Token _ TK.TKis }
    reincarnationOf     { TK.Token _ TK.TKreincarnation }
    art                 { TK.Token _ TK.TKart }
    null                { TK.Token _ TK.TKapprentice }
    born                { TK.Token _ TK.TKborn }
    member              { TK.Token _ TK.TKmember }
    died                { TK.Token _ TK.TKdied }
    air                 { TK.Token _ TK.TKair }
    water               { TK.Token _ TK.TKwater }
    fire                { TK.Token _ TK.TKfire }
    true                { TK.Token _ TK.TKlightning }
    false               { TK.Token _ TK.TKfireMaster }
    earth               { TK.Token _ TK.TKearth }
    metal               { TK.Token _ TK.TKmetal }
    nation              { TK.Token _ TK.TKnation }
    year                { TK.Token _ TK.TKyear }
    masterOf            { TK.Token _ TK.TKmasterOf }
    rightNow            { TK.Token _ TK.TKRightNow }
    disciple            { TK.Token _ TK.TKdisciple }
    element             { TK.Token _ TK.TKelement }
    compoundBy          { TK.Token _ TK.TKcompoundBy }
    skillOf             { TK.Token _ TK.TKskillOf }
    skill               { TK.Token _ TK.TKskill }
    learning            { TK.Token _ TK.TKlearning }
    control             { TK.Token _ TK.TKcontrol }
    energy              { TK.Token _ TK.TKenergy }
    allows              { TK.Token _ TK.TKallows }
    techniqueOf         { TK.Token _ TK.TKtechniqueOf }
    bending             { TK.Token _ TK.TKbending }
    techniqueFrom       { TK.Token _ TK.TKtechniqueFrom }
    using               { TK.Token _ TK.TKusing }
    quotmark_s          { TK.Token _ TK.TKquotmark_s }
    technique           { TK.Token _ TK.TKtechnique }
    trying              { TK.Token _ TK.TKtrying }
    book                { TK.Token _ TK.TKbook }
    about               { TK.Token _ TK.TKabout }
    travel              { TK.Token _ TK.TKtravel }
    madeBy              { TK.Token _ TK.TKmadeBy }
    '+'                 { TK.Token _ TK.TKandThen }
    '-'                 { TK.Token _ TK.TKbut }
    '*'                 { TK.Token _ TK.TKandThus }
    '/'                 { TK.Token _ TK.TKbesides }
    '%'                 { TK.Token _ TK.TKleft }
    and                 { TK.Token _ TK.TKand }
    or                  { TK.Token _ TK.TKor }
    not                 { TK.Token _ TK.TKnot }
    if                  { TK.Token _ TK.TKif }
    otherwise           { TK.Token _ TK.TKotherwise }
    dotOtherwise        { TK.Token _ TK.TKdotOtherwise }
    comma               { TK.Token _ TK.TKcomma }
    colon               { TK.Token _ TK.TKcolon }
    beginBlock          { TK.Token _ TK.TKbeginBlock }
    endBlock            { TK.Token _ TK.TKendBlock }
    dot                 { TK.Token _ TK.TKdot }
    unit                { TK.Token _ TK.TKunit }
    '('                 { TK.Token _ TK.TKopenParent }
    ')'                 { TK.Token _ TK.TKcloseParent }
    in                  { TK.Token _ TK.TKin }
    bookWith            { TK.Token _ TK.TKbookWith }
    travelWith          { TK.Token _ TK.TKtravelWith }
    '<'                 { TK.Token _ TK.TKlessThan }
    '<='                { TK.Token _ TK.TKlessEqThan }
    '>'                 { TK.Token _ TK.TKgreaterThan }
    '>='                { TK.Token _ TK.TKgreaterEqThan }
    '=='                { TK.Token _ TK.TKequal }
    '!='                { TK.Token _ TK.TKnotEqual }
    while               { TK.Token _ TK.TKwhile }
    doing               { TK.Token _ TK.TKdoing }
    opening             { TK.Token _ TK.TKopening }
    chakrasFrom         { TK.Token _ TK.TKchakrasFrom }
    to                  { TK.Token _ TK.TKto }
    elipsis             { TK.Token _ TK.TKelipsis }
    toBeContinued       { TK.Token _ TK.TKtoBeContinued }
    toBeContinuedUnit   { TK.Token _ TK.TKtoBeContinuedUnit }
    burst               { TK.Token _ TK.TKburst }
    burstUnit           { TK.Token _ TK.TKburstUnit }
    return              { TK.Token _ TK.TKreturn }
    returnUnit          { TK.Token _ TK.TKreturnUnit }
    int                 { TK.Token _ (TK.TKint $$) }
    float               { TK.Token _ (TK.TKfloat $$) }
    char                { TK.Token _ (TK.TKchar $$) }
    string              { TK.Token _ (TK.TKstring $$) }
    id                  { TK.Token _ (TK.TKid _) }
 
%right colon otherwise dotOtherwise of
%left unit quotmark_s 

%right toBeContinued burst return 
%left died

%right is 
%left and or

%nonassoc '<' '<=' '>' '>=' 
%left '==' '!='
%left '+' '-'
%left '*' '/' '%'

%right not NEG
%right techniqueFrom


%%
-- Grammar

-- Source Symbol
Program         :: { AST.Program }
    : Declarations                                      { AST.Program (reverse $1) }

-- Program as declaration list
Declarations    :: { [AST.Declaration] }
    : Declaration dot                                   { [$1] }
    | Declarations Declaration dot                      { $2:$1 }

Declaration     :: { AST.Declaration }
    : VarDecl                                                 { $1 }
    | element id compoundBy PushScope StructIdDecls PopScope  {% P.checkDecls $ AST.Struct ((TK.name . TK.tktype) $2) (reverse $5) }
    | energy id allows PushScope UnionIdDecls PopScope        {% P.checkDecls $ AST.Union  ((TK.name . TK.tktype) $2) (reverse $5) }
    | FuncDecl                                                { $1 }
    | ProcDecl                                                { $1 }

ProcDecl        :: { AST.Declaration }
    : ProcDescription madeBy PushScope FuncArg colon PushScope Exprs PopScope PopScope            {% P._functionCheckerHelper $1 (Just AST.TUnit) $4 $7 }
    | ProcDescription PushScope colon PushScope Exprs PopScope PopScope                           {% P._functionCheckerHelper $1 (Just AST.TUnit) [] $5 }

ProcDescription :: { U.Name }
    : travel id                                                                             {% P.pushType AST.TUnit >> return ((TK.name . TK.tktype) $2) }

FuncDecl        :: { AST.Declaration }
    : FuncDescription about PushScope FuncArg colon PushScope Exprs PopScope PopScope       {% P._functionCheckerHelper (fst $1) (snd $1) $4 $7 }
    | FuncDescription PushScope colon PushScope Exprs PopScope PopScope                     {% P._functionCheckerHelper (fst $1) (snd $1) [] $5 }

FuncDescription :: { (U.Name, Maybe AST.Type) }
    : book id of Type                                                                       {% P.pushType $4 >> return ((TK.name . TK.tktype) $2, Just $4) }
    | book id                                                                               {% P.pushType AST.TVoid >> return ((TK.name . TK.tktype) $2, Nothing) }

FuncArg         :: { [AST.FuncArg] }
    : FuncDefArgDecl                                    { $1 }
    | FuncArgDecl                                       { $1 }
    | FuncArgDecl comma FuncDefArgDecl                  { $3 ++ $1 }

FuncDefArgDecl :: { [AST.FuncArg] }
    : SingleDefArgDecl                                  { [$1] }
    | FuncDefArgDecl comma Type bender id Assign        {% (P.checkFunArg $ AST.FuncArg ((TK.name . TK.tktype) $5) $3 (Just $6)) >>= (return . (:$1)) }
    | FuncDefArgDecl comma Type '&' bender id Assign    {% (P.checkFunArg $ AST.FuncArg ((TK.name . TK.tktype) $6) (AST.TReference $3) (Just $7)) >>= (return . (:$1)) }

SingleDefArgDecl :: { AST.FuncArg }
    : Type bender id Assign                             {% P.checkFunArg $ AST.FuncArg ((TK.name . TK.tktype) $3) $1 (Just $4) }
    | Type '&' bender id Assign                         {% P.checkFunArg $ AST.FuncArg ((TK.name . TK.tktype) $4) (AST.TReference $1) (Just $5) }

FuncArgDecl     :: { [AST.FuncArg] }
    : SingleFuncArgDecl                                 { [$1] }
    | FuncArgDecl comma Type bender id                  {% (P.checkFunArg $ AST.FuncArg ((TK.name . TK.tktype) $5) $3 Nothing) >>= (return . (:$1)) }
    | FuncArgDecl comma Type '&' bender id              {% (P.checkFunArg $ AST.FuncArg ((TK.name . TK.tktype) $6) (AST.TReference $3) Nothing) >>= (return . (:$1)) }

SingleFuncArgDecl :: { AST.FuncArg }
    : Type bender id                                    {% P.checkFunArg $ AST.FuncArg ((TK.name . TK.tktype) $3) $1 Nothing }
    | Type '&' bender id                                {% P.checkFunArg $ AST.FuncArg ((TK.name . TK.tktype) $4) (AST.TReference $1) Nothing }

--

StructIdDecls   :: { [(String, AST.Type)] }
    : id skillOf Type                                   {% P.checkField $ [(((TK.name . TK.tktype) $1), $3)] }
    | StructIdDecls comma id skillOf Type               {% P.checkField $ (((TK.name . TK.tktype) $3), $5):$1 }

UnionIdDecls    :: { [(String, AST.Type)] }
   : id techniqueOf Type bending                        {% P.checkField $ [(((TK.name . TK.tktype) $1), $3)] }
   | UnionIdDecls comma id techniqueOf Type bending     {% P.checkField $ (((TK.name . TK.tktype) $3), $5):$1 }

--

VarDecl         :: { AST.Declaration }
    : bender id of Type                                 {% P.checkDecls $ AST.Variable ((TK.name . TK.tktype) $2) $4 Nothing False }
    | bender id of Type Assign                          {% P.checkDecls $ AST.Variable ((TK.name . TK.tktype) $2) $4 (Just $5) False }
    | bender id Assign                                  {% P.checkDecls $ AST.Variable ((TK.name . TK.tktype) $2) (AST.expType $3) (Just $3) False }
    | eternal bender id of Type Assign                  {% P.checkDecls $ AST.Variable ((TK.name . TK.tktype) $3) $5 (Just $6) True }
    | eternal bender id Assign                          {% P.checkDecls $ AST.Variable ((TK.name . TK.tktype) $3) (AST.expType $4) (Just $4) True }
    | bender id is reincarnationOf id                   {% P.checkDecls $ AST.Reference ((TK.name . TK.tktype) $2) ((TK.name . TK.tktype) $5) }
    

    -- >> Expressions --------------------------------------------------------------------------

Expr            :: { AST.Expr }  
    : '(' Expr ')'                                      { $2 }
    | id                                                {% P.checkExpr $ AST.Id ((TK.name . TK.tktype) $1) (TK.pos $1) AST.TypeError }
    | ExprBlock                                         { $1 }
    | id Assign                                         {% P.checkExpr $ AST.Assign ((TK.name . TK.tktype) $1) $2 AST.TypeError }
   
    | Expr quotmark_s id Assign                         {% P.checkExpr $ AST.StructAssign $1 ((TK.name . TK.tktype) $3) $4 AST.TypeError }
    | using Expr quotmark_s id skill                    {% P.checkExpr $ AST.StructAccess $2 ((TK.name . TK.tktype) $4) AST.TypeError }
    | learning id control using
       ExprList rightNow                                {% P.checkExpr $ AST.ConstStruct ((TK.name . TK.tktype) $2) (reverse $5) AST.TypeError }
   
    | trying Expr quotmark_s id technique               {% P.checkExpr $ AST.UnionTrying $2 ((TK.name . TK.tktype) $4) AST.TypeError }
    | using Expr quotmark_s id technique                {% P.checkExpr $ AST.UnionUsing $2 ((TK.name . TK.tktype) $4) AST.TypeError }
    | learning id quotmark_s id 
       techniqueFrom Expr                               {% P.checkExpr $ AST.ConstUnion ((TK.name . TK.tktype) $2) ((TK.name . TK.tktype) $4) $6 AST.TypeError }
   
    | ForDescription colon PushScope Expr PopScope PopScope {% do
                                                                let (_id,_step,_start,_end) = $1
                                                                P.checkExpr $ AST.For _id _step _start _end $4 AST.TypeError
                                                            }
    | while Expr doing colon PushScope Expr PopScope                                   {% P.checkExpr $ AST.While $2 $6 AST.TypeError }
    | if  Expr colon PushScope Expr PopScope otherwise PushScope Expr PopScope         {% P.checkExpr $ AST.If $2 $5 $9 AST.TypeError }
    | if  Expr colon PushScope Expr PopScope dotOtherwise PushScope Expr PopScope      {% P.checkExpr $ AST.If $2 $5 $9 AST.TypeError }
    | if  Expr colon PushScope Expr PopScope                                           {% P.checkExpr $ AST.If $2 $5 (AST.ConstUnit AST.TUnit) AST.TypeError }
    | if  Expr dot colon PushScope Expr PopScope otherwise PushScope Expr PopScope     {% P.checkExpr $ AST.If $2 $6 $10 AST.TypeError }
    | if  Expr dot colon PushScope Expr PopScope dotOtherwise PushScope Expr PopScope  {% P.checkExpr $ AST.If $2 $6 $10 AST.TypeError }
    | if  Expr dot colon PushScope Expr PopScope                                       {% P.checkExpr $ AST.If $2 $6 (AST.ConstUnit AST.TUnit) AST.TypeError }
  
    | in id bookWith ExprList elipsis                   {% P.checkExpr $ AST.FunCall ((TK.name . TK.tktype) $2) (reverse $4) AST.TypeError }
    | in id bookWith elipsis                            {% P.checkExpr $ AST.FunCall ((TK.name . TK.tktype) $2) [] AST.TypeError }
    | id bookWith ExprList elipsis                      {% P.checkExpr $ AST.FunCall ((TK.name . TK.tktype) $1) (reverse $3) AST.TypeError }
    | id bookWith elipsis                               {% P.checkExpr $ AST.FunCall ((TK.name . TK.tktype) $1) [] AST.TypeError }

    | in id travelWith ExprList elipsis                 {% P.checkExpr $ AST.FunCall ((TK.name . TK.tktype) $2) (reverse $4) AST.TypeError }
    | in id travelWith elipsis                          {% P.checkExpr $ AST.FunCall ((TK.name . TK.tktype) $2) [] AST.TypeError }
    | id travelWith ExprList elipsis                    {% P.checkExpr $ AST.FunCall ((TK.name . TK.tktype) $1) (reverse $3) AST.TypeError }
    | id travelWith elipsis                             {% P.checkExpr $ AST.FunCall ((TK.name . TK.tktype) $1) [] AST.TypeError }

    | born Type member                                  {% P.checkExpr $ AST.New $2 AST.TypeError }
    | Expr died                                         {% P.checkExpr $ AST.Delete $1 AST.TUnit }
    | disciple Expr of Expr                             {% P.checkExpr $ AST.ArrayIndexing $2 $4 AST.TypeError }
    | masterOf ExprList rightNow                        {% P.checkExpr $ AST.Array (reverse $2) AST.TypeError }

    -- >> Const Values --------------------------------------------------------------------------------
    | int                                               { AST.ConstInt $1 AST.TInt }
    | float                                             { AST.ConstFloat $1 AST.TFloat }
    | true                                              { AST.ConstTrue AST.TBool }
    | false                                             { AST.ConstFalse AST.TBool }
    | char                                              { AST.ConstChar $1 AST.TChar }
    | string                                            { AST.ConstString $1 AST.TString }
    | null                                              { AST.ConstNull (AST.TPtr AST.TVoid) }

    -- >> Binary Expressions --------------------------------------------------------------------------

    | Expr '+' Expr                                     {% P.checkExpr $ AST.Op2 AST.Sum $1 $3 AST.TypeError }
    | Expr '-' Expr                                     {% P.checkExpr $ AST.Op2 AST.Sub $1 $3 AST.TypeError }
    | Expr '*' Expr                                     {% P.checkExpr $ AST.Op2 AST.Mult $1 $3 AST.TypeError }
    | Expr '/' Expr                                     {% P.checkExpr $ AST.Op2 AST.Div $1 $3 AST.TypeError }
    | Expr '%' Expr                                     {% P.checkExpr $ AST.Op2 AST.Mod $1 $3 AST.TypeError }
    | Expr '<' Expr                                     {% P.checkExpr $ AST.Op2 AST.Lt $1 $3 AST.TypeError }
    | Expr '<=' Expr                                    {% P.checkExpr $ AST.Op2 AST.LtEq $1 $3 AST.TypeError }
    | Expr '>' Expr                                     {% P.checkExpr $ AST.Op2 AST.Gt $1 $3 AST.TypeError }
    | Expr '>=' Expr                                    {% P.checkExpr $ AST.Op2 AST.GtEq $1 $3 AST.TypeError }
    | Expr '==' Expr                                    {% P.checkExpr $ AST.Op2 AST.Eq $1 $3 AST.TypeError }
    | Expr '!=' Expr                                    {% P.checkExpr $ AST.Op2 AST.NotEq $1 $3 AST.TypeError }
    | Expr and Expr                                     {% P.checkExpr $ AST.Op2 AST.And $1 $3 AST.TypeError }
    | Expr or Expr                                      {% P.checkExpr $ AST.Op2 AST.Or $1 $3 AST.TypeError }

    -- >> Unary Expressions ------------------------------------------------------------------------------
    | not Expr                                          {% P.checkExpr $ AST.Op1 AST.Negation $2 AST.TypeError }
    | '-' Expr %prec NEG                                {% P.checkExpr $ AST.Op1 AST.Negative $2 AST.TypeError }
    | Expr unit                                         {% P.checkExpr $ AST.Op1 AST.UnitOperator $1 AST.TypeError }

    -- >> Control Flow -----------------------------------------------------------------------------------
--    | toBeContinued Expr                                { AST.Continue  $2 }
--    | burst Expr                                        { AST.Break     $2 }
    | return Expr                                       {% P.checkExpr $ AST.Return $2 (AST.expType $2) }
--    | toBeContinuedUnit                                 { AST.Continue  AST.ConstUnit }
--    | burstUnit                                         { AST.Break     AST.ConstUnit }
    | returnUnit                                        { AST.Return  (AST.ConstUnit AST.TUnit) AST.TUnit }
    

    -- >> For description 

ForDescription  ::  { (String, AST.Expr, AST.Expr, AST.Expr) }
    : opening Expr of id chakrasFrom Expr to Expr       {% do
                                                            P.pushEmptyScope
                                                            P.checkDecls $ AST.Variable ((TK.name . TK.tktype) $4) (AST.expType $6) (Just $6) False
                                                            return (((TK.name . TK.tktype) $4), $2, $6, $8)
                                                        }

    -- >> Evaluable and none evaluable expressions > -----------------------------------------------------
Exprs           ::  { AST.Expr }
    : Expr                                              { $1 }
    | Declaration                                       { AST.Declaration $1 AST.TUnit } 

    -- >> Assigment ---------------------------------------------------------------------------------------
Assign          :: { AST.Expr }
    : is Expr                                           { $2 } 


-- < Expression block Grammar > --------------------------------------------------------------------------  
ExprBlock       ::  { AST.Expr }
    : beginBlock PushScope ExprSeq PopScope endBlock    { AST.ExprBlock (reverse $3) (AST.expType . head $ $3)}
    | beginBlock PushScope PopScope endBlock            { AST.ExprBlock [] AST.TUnit }

ExprSeq         ::  { [AST.Expr] }
    : LastInBlock                                       { [$1] }
    | Seq LastInBlock                                   { $2:$1 }

Seq             :: { [AST.Expr] }
    : Exprs Dots                                        { [$1] }
    | Seq Exprs Dots                                    { $2:$1 }
    | Dots                                              { [] }
    
LastInBlock     :: { AST.Expr }
    : Exprs                                             { $1 }
    | Exprs Dots                                        { $1 }

Dots            :: { [AST.Expr] }
    : dot                                               { [] }
    | Dots dot                                          { [] }

ExprList        :: { [AST.Expr] }
   : Expr                                              { [$1] }
   | ExprList comma Expr                               { $3:$1 }

    -- >> Types -------------------------------------------------------------------------------------
Type            :: { AST.Type }
    : water                                             { AST.TFloat }
    | air                                               { AST.TInt }
    | earth                                             { AST.TChar }
    | metal                                             { AST.TString }   
    | fire                                              { AST.TBool }
    | id                                                {% P.getCustomType ((TK.name . TK.tktype) $1) }
    | Type nation Expr year                             {% P.checkType $ AST.TArray $1 $3}
    | Type art                                          { AST.TPtr $1 }


    -- >> Auxiliar Rules ----------------------------------------------------------------------------

PushScope 
    : {- empty -}                                       {% P.pushEmptyScope }

PopScope
    : {- empty -}                                       {% P.popEmptyScope }


{

-- Error function
-- parseError :: [TK.Token] -> a
parseError []       = P.addStaticError SE.UnexpectedEOF >> (fail . show) SE.UnexpectedEOF 
parseError rem@(tk:tks) = P.addStaticError (SE.ParseError rem)    >> (fail . show . SE.ParseError) rem

-- Run the parser and return the results
runParse :: [TK.Token] -> P.ParsingState -> IO (AST.Program, P.ParsingState, P.ErrorLog)
runParse tks preState = do
    (ast, s, e) <- RWS.runRWST (parseTokens tks) () preState
    return (ast, s, e)

}