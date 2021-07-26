{

module FrontEnd.PreParser where

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
import Data.Functor((<&>))

}

%name preParseTokens
%tokentype { TK.Token }
%error { parseError }
%monad { P.ParserState }

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
Program         -- :: { AST.Program }
    : Declarations                                      { }

-- Program as declaration list
Declarations    -- :: { [AST.Declaration] }
    : Declaration dot                                   { }
    | Declarations Declaration dot                      { }

Declaration    -- :: { () }--{ AST.Declaration }
    : element id compoundBy StructIdDecls               { }
    | energy id allows UnionIdDecls                     { }
    | VarDecl                                           { }
    | FuncDecl                                          { }
    | ProcDecl                                          { }

ProcDecl        :: { () }-- { AST.Declaration }
    : travel id madeBy PushScope FuncArg colon PushScope Exprs PopScope PopScope              {% M.void . P.preCheckDecls $ AST.Func ((TK.name . TK.tktype) $2) (reverse $5) AST.TUnit (AST.ConstUnit AST.TUnit) }
    | travel id PushScope colon PushScope Exprs PopScope PopScope                             {% M.void . P.preCheckDecls $ AST.Func ((TK.name . TK.tktype) $2) [] AST.TUnit (AST.ConstUnit AST.TUnit) }

FuncDecl        :: { () }-- { AST.Declaration }
    : book id of Type about PushScope FuncArg colon PushScope Exprs PopScope PopScope        {% M.void . P.preCheckDecls $ AST.Func ((TK.name . TK.tktype) $2) (reverse $7) $4 (AST.ConstUnit AST.TUnit) }
    | book id of Type PushScope colon PushScope Exprs PopScope PopScope                      {% M.void . P.preCheckDecls $ AST.Func ((TK.name . TK.tktype) $2) [] $4 (AST.ConstUnit AST.TUnit) }
    | book id about PushScope FuncArg colon PushScope Exprs PopScope PopScope                {% M.void . P.preCheckDecls $ AST.Func ((TK.name . TK.tktype) $2) (reverse $5) AST.TUnit (AST.ConstUnit AST.TUnit) }
    | book id PushScope colon PushScope Exprs PopScope PopScope                              {% M.void . P.preCheckDecls $ AST.Func ((TK.name . TK.tktype) $2) [] AST.TUnit (AST.ConstUnit AST.TUnit) }

FuncArg         :: { [AST.FuncArg] }
    : FuncDefArgDecl                                    { $1 }
    | FuncArgDecl                                       { $1 }
    | FuncArgDecl comma FuncDefArgDecl                  { ($3 ++ $1) }

FuncDefArgDecl :: { [AST.FuncArg] }
    : Type bender id Assign                             { [AST.FuncArg ((TK.name . TK.tktype) $3) $1 (Nothing)] }
    | Type '&' bender id Assign                         { [AST.FuncArg ((TK.name . TK.tktype) $4) (AST.TReference $1) (Nothing)] }
    | FuncDefArgDecl comma Type bender id Assign        { (AST.FuncArg ((TK.name . TK.tktype) $5) $3 (Nothing)):$1 }
    | FuncDefArgDecl comma Type '&' bender id Assign    { (AST.FuncArg ((TK.name . TK.tktype) $6) (AST.TReference $3) (Nothing)):$1 }

FuncArgDecl     :: { [AST.FuncArg] }                    
    : Type bender id                                    { [AST.FuncArg ((TK.name . TK.tktype) $3) $1 Nothing] }
    | Type '&' bender id                                { [AST.FuncArg ((TK.name . TK.tktype) $4) (AST.TReference $1) Nothing] }
    | FuncArgDecl comma Type bender id                  { (AST.FuncArg ((TK.name . TK.tktype) $5) $3 Nothing):$1 }
    | FuncArgDecl comma Type '&' bender id              { (AST.FuncArg ((TK.name . TK.tktype) $6) (AST.TReference $3) Nothing):$1 }

StructIdDecls   -- :: { [(String, AST.Type)] }
    : id skillOf Type                                   {}-- { [(((TK.name . TK.tktype) $1), $3)] }
    | StructIdDecls comma id skillOf Type               {}-- { (((TK.name . TK.tktype) $3), $5):$1 }

UnionIdDecls    -- :: { [(String, AST.Type)] }
    : id techniqueOf Type bending                       {}-- { [(((TK.name . TK.tktype) $1), $3)] }
    | UnionIdDecls comma id techniqueOf Type bending    {}-- { (((TK.name . TK.tktype) $3), $5):$1 }

VarDecl         -- ::{ () }--{ AST.Declaration }
    : bender id of Type                                 {}-- {% P.checkDecls $ AST.Variable ((TK.name . TK.tktype) $2) (Just $4) Nothing False }
    | bender id of Type Assign                          {}-- {% P.checkDecls $ AST.Variable ((TK.name . TK.tktype) $2) (Just $4) (Just $5) False }
    | bender id Assign                                  {}-- {% P.checkDecls $ AST.Variable ((TK.name . TK.tktype) $2) Nothing (Just $3) False }
    | eternal bender id of Type Assign                  {}-- {% P.checkDecls $ AST.Variable ((TK.name . TK.tktype) $3) (Just $5) (Just $6) True }
    | eternal bender id Assign                          {}-- {% P.checkDecls $ AST.Variable ((TK.name . TK.tktype) $3) Nothing (Just $4) True }
    | bender id is reincarnationOf id                   {}-- {% P.checkDecls $ AST.Reference ((TK.name . TK.tktype) $2) ((TK.name . TK.tktype) $5) }
    

    -- >> Expressions --------------------------------------------------------------------------

Expr            --:: { AST.Expr }  
    : '(' Expr ')'                                      {}-- { $2 }
    | id                                                {}-- { AST.Id ((TK.name . TK.tktype) $1) (TK.pos $1) }
    | ExprBlock                                         {}-- { $1 }
    | id Assign                                         {}-- { AST.Assign ((TK.name . TK.tktype) $1) $2 }
    
    | Expr quotmark_s id Assign                         {}-- { AST.StructAssign $1 ((TK.name . TK.tktype) $3) $4 }
    | using Expr quotmark_s id skill                    {}-- { AST.StructAccess $2 ((TK.name . TK.tktype) $4) }
    | learning id control using
        ExprList rightNow                               {}-- { AST.ConstStruct ((TK.name . TK.tktype) $2) (reverse $5) }
    
    | trying Expr quotmark_s id technique               {}-- { AST.UnionTrying $2 ((TK.name . TK.tktype) $4) }
    | using Expr quotmark_s id technique                {}-- { AST.UnionUsing $2 ((TK.name . TK.tktype) $4) }
    | learning id quotmark_s id 
        techniqueFrom Expr                              {}-- { AST.ConstUnion ((TK.name . TK.tktype) $2) ((TK.name . TK.tktype) $4) $6}
    
    | opening Expr of id chakrasFrom 
        Expr to Expr colon PushScope PushScope Expr PopScope PopScope       {}-- { AST.For ((TK.name . TK.tktype) $4) $2 $6 $8 $10 }
    | while Expr doing colon PushScope Expr PopScope              {}-- { AST.While $2 $5 }
    | if  Expr colon PushScope Expr PopScope otherwise PushScope Expr PopScope   {}-- { AST.If $2 $4 $6 }
    | if  Expr colon PushScope Expr PopScope dotOtherwise PushScope Expr PopScope             {}-- { AST.If $2 $4 $6 }
    | if  Expr colon PushScope Expr PopScope                              {}-- { AST.If $2 $4 AST.(AST.ConstUnit AST.TUnit) }
    | if  Expr dot colon PushScope Expr PopScope otherwise PushScope Expr PopScope     {}-- { AST.If $2 $5 $7 }
    | if  Expr dot colon PushScope Expr PopScope dotOtherwise PushScope Expr PopScope  {}-- { AST.If $2 $5 $7 }
    | if  Expr dot colon PushScope Expr PopScope                          {}-- { AST.If $2 $5 AST.(AST.ConstUnit AST.TUnit) }
   
    | in id bookWith ExprList elipsis                   {}-- { AST.FunCall ((TK.name . TK.tktype) $2) (reverse $4) }
    | in id bookWith elipsis                            {}-- { AST.FunCall ((TK.name . TK.tktype) $2) [] }
    | id bookWith ExprList elipsis                      {}-- { AST.FunCall ((TK.name . TK.tktype) $1) (reverse $3) }
    | id bookWith elipsis                               {}-- { AST.FunCall ((TK.name . TK.tktype) $1) [] }

    | in id travelWith ExprList elipsis                 {}-- { AST.FunCall ((TK.name . TK.tktype) $2) (reverse $4) }
    | in id travelWith elipsis                          {}-- { AST.FunCall ((TK.name . TK.tktype) $2) [] }
    | id travelWith ExprList elipsis                    {}-- { AST.FunCall ((TK.name . TK.tktype) $1) (reverse $3) }
    | id travelWith elipsis                             {}-- { AST.FunCall ((TK.name . TK.tktype) $1) [] }

    | born Type member                                  {}-- { AST.New $2 }
    | Expr died                                         {}-- { AST.Delete $1 }
    | disciple Expr of Expr                             {}-- { AST.ArrayIndexing $2 $4 }
    | masterOf ExprList rightNow                        {}-- { AST.Array (reverse $2) }

    -- >> Const Values --------------------------------------------------------------------------------
    | int                                               {}-- { AST.ConstInt $1 }
    | float                                             {}-- { AST.ConstFloat $1 }
    | true                                              {}-- { AST.ConstTrue }
    | false                                             {}-- { AST.ConstFalse }
    | char                                              {}-- { AST.ConstChar $1 }
    | string                                            {}-- { AST.ConstString $1 }
    | null                                              {}-- { AST.ConstNull }

    -- >> Binary Expressions --------------------------------------------------------------------------

    | Expr '+' Expr                                     {}-- { AST.Op2 AST.Sum $1 $3 }
    | Expr '-' Expr                                     {}-- { AST.Op2 AST.Sub $1 $3 }
    | Expr '*' Expr                                     {}-- { AST.Op2 AST.Mult $1 $3 }
    | Expr '/' Expr                                     {}-- { AST.Op2 AST.Div $1 $3 }
    | Expr '%' Expr                                     {}-- { AST.Op2 AST.Mod $1 $3 }
    | Expr '<' Expr                                     {}-- { AST.Op2 AST.Lt $1 $3 }
    | Expr '<=' Expr                                    {}-- { AST.Op2 AST.LtEq $1 $3 }
    | Expr '>' Expr                                     {}-- { AST.Op2 AST.Gt $1 $3 }
    | Expr '>=' Expr                                    {}-- { AST.Op2 AST.GtEq $1 $3 }
    | Expr '==' Expr                                    {}-- { AST.Op2 AST.Eq $1 $3 }
    | Expr '!=' Expr                                    {}-- { AST.Op2 AST.NotEq $1 $3 }
    | Expr and Expr                                     {}-- { AST.Op2 AST.And $1 $3 }
    | Expr or Expr                                      {}-- { AST.Op2 AST.Or $1 $3 }

    -- >> Unary Expressions ------------------------------------------------------------------------------
    | not Expr                                          {}-- { AST.Op1 AST.Negation $2 }
    | '-' Expr %prec NEG                                {}-- { AST.Op1 AST.Negative $2 }
    | Expr unit                                         {}-- { AST.Op1 AST.UnitOperator $1 }

    -- >> Control Flow -----------------------------------------------------------------------------------
    | toBeContinued Expr                                {}-- { AST.Continue  $2 }
    | burst Expr                                        {}-- { AST.Break     $2 }
    | return Expr                                       {}-- { AST.Return    $2 }
    | toBeContinuedUnit                                 {}-- { AST.Continue  AST.(AST.ConstUnit AST.TUnit) }
    | burstUnit                                         {}-- { AST.Break     AST.(AST.ConstUnit AST.TUnit) }
    | returnUnit                                        {}-- { AST.Return    AST.(AST.ConstUnit AST.TUnit) }
    
    -- >> Evaluable and none evaluable expressions > -----------------------------------------------------
Exprs           --::  { AST.Expr }
    : Expr                                              {}-- { $1 }
    | Declaration                                       {}-- { AST.Declaration $1 } 

    -- >> Assigment ---------------------------------------------------------------------------------------
Assign          --:: { AST.Expr }
    : is Expr                                           {}-- { $2 } 


-- < Expression block Grammar > --------------------------------------------------------------------------  
ExprBlock       --::  { AST.Expr }
    : beginBlock PushScope ExprSeq PopScope endBlock    {}
    | beginBlock PushScope PopScope endBlock            {}

ExprSeq         --::  { [AST.Expr] }
    : LastInBlock                                       {}-- { [$1] }
    | Seq LastInBlock                                   {}-- { $2:$1 }

Seq             --:: { [AST.Expr] }
    : Exprs Dots                                        {}-- { [$1] }
    | Seq Exprs Dots                                    {}-- { $2:$1 }
    | Dots                                              {}-- { [] }
    
LastInBlock     --:: { AST.Expr }
    : Exprs                                             {}-- { $1 }
    | Exprs Dots                                        {}-- { $1 }

Dots            --:: { [AST.Expr] }
    : dot                                               {}-- { [] }
    | Dots dot                                          {}-- { [] }

ExprList        -- {}-- :: { [AST.Expr] }
    : Expr                                              {}-- { [$1] }
    | ExprList comma Expr                               {}-- { $3:$1 }

    -- >> Types -------------------------------------------------------------------------------------
Type            :: { AST.Type }
    : water                                             { AST.TFloat }
    | air                                               { AST.TInt }
    | earth                                             { AST.TChar }
    | metal                                             { AST.TString }   
    | fire                                              { AST.TBool }
    | id                                                {% P.getCustomType ((TK.name . TK.tktype) $1) }
    | Type nation Expr year                             { AST.TArray $1 (AST.ConstInt 0 AST.TInt)}
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
parseError rem@(tk:tks) = P.addStaticError (SE.ParseError rem)    >> (fail $ "parse error in "++ (show tk)) -- (fail . show) (SE.ParseError rem)


-- could use execRWST instead of runRWST
runPreParse :: [TK.Token] -> IO (P.ParsingState, P.ErrorLog)
runPreParse tks = do
    (_, s, e) <- RWS.runRWST (preParseTokens tks) () P.startingState
    return (s, e)

}