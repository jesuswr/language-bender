{

module FrontEnd.Parser where

import qualified FrontEnd.Tokens as TK
import qualified FrontEnd.AST    as AST
import qualified FrontEnd.Errors  as E

}

%name parseTokens
%tokentype { TK.Token }
%error { parseError }

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
 
--%right ')' otherwise
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

%right not
--%right colon
%right techniqueFrom


%%
-- Grammar

-- Source Symbol
Program         :: { AST.Program }
    : Declarations                                      { AST.Program (reverse $1) }

-- Program as declaration list
Declarations    :: { [AST.Declaration] }
    : Declaration                                       { [$1] }
    | Declarations Declaration                          { $2:$1 }

Declaration     :: { AST.Declaration }
    : element id compoundBy StructIdDecls               { AST.Struct ((TK.name . TK.tktype) $2) (reverse $4) }
    | energy id allows UnionIdDecls                     { AST.Union  ((TK.name . TK.tktype) $2) (reverse $4) }
    | VarDecl                                           { $1 }
    | FuncDecl                                          { $1 }
    | ProcDecl                                          { $1 }

ProcDecl        :: { AST.Declaration }
    : travel id madeBy FuncArg colon Exprs              { AST.Func ((TK.name . TK.tktype) $2) (reverse $4) (Just AST.TUnit) $6 }
    | travel id colon Exprs                             { AST.Func ((TK.name . TK.tktype) $2) [] (Just AST.TUnit) $4 }

FuncDecl        :: { AST.Declaration }
    : book id of Type about FuncArg colon Exprs         { AST.Func ((TK.name . TK.tktype) $2) (reverse $6) (Just $4) $8 }
    | book id of Type colon Exprs                       { AST.Func ((TK.name . TK.tktype) $2) [] (Just $4) $6 }
    | book id about FuncArg colon Exprs                 { AST.Func ((TK.name . TK.tktype) $2) (reverse $4) Nothing $6 }
    | book id colon Exprs                               { AST.Func ((TK.name . TK.tktype) $2) [] Nothing $4 }

FuncArg         :: { [AST.FuncArg] }
    : FuncDefArgDecl                                    { $1 }
    | FuncArgDecl                                       { $1 }
    | FuncArgDecl comma FuncDefArgDecl                  { $3 ++ $1 }

FuncDefArgDecl :: { [AST.FuncArg] }
    : Type bender id Assign                             { [AST.FuncArg ((TK.name . TK.tktype) $3) $1 (Just $4)] }
    | Type '&' bender id Assign                         { [AST.FuncArg ((TK.name . TK.tktype) $4) (AST.TReference $1) (Just $5)] }
    | FuncDefArgDecl comma Type bender id Assign        { (AST.FuncArg ((TK.name . TK.tktype) $5) $3 (Just $6)):$1 }
    | FuncDefArgDecl comma Type '&' bender id Assign    { (AST.FuncArg ((TK.name . TK.tktype) $6) (AST.TReference $3) (Just $7)):$1 }

FuncArgDecl     :: { [AST.FuncArg] }
    : Type bender id                                    { [AST.FuncArg ((TK.name . TK.tktype) $3) $1 Nothing] }
    | Type '&' bender id                                { [AST.FuncArg ((TK.name . TK.tktype) $4) (AST.TReference $1) Nothing] }
    | FuncArgDecl comma Type bender id                  { (AST.FuncArg ((TK.name . TK.tktype) $5) $3 Nothing):$1 }
    | FuncArgDecl comma Type '&' bender id              { (AST.FuncArg ((TK.name . TK.tktype) $6) (AST.TReference $3) Nothing):$1 }

StructIdDecls   :: { [(String, AST.Type)] }
    : id skillOf Type                                   { [(((TK.name . TK.tktype) $1), $3)] }
    | StructIdDecls comma id skillOf Type               { (((TK.name . TK.tktype) $3), $5):$1 }

UnionIdDecls    :: { [(String, AST.Type)] }
    : id techniqueOf Type bending                       { [(((TK.name . TK.tktype) $1), $3)] }
    | UnionIdDecls comma id techniqueOf Type bending    { (((TK.name . TK.tktype) $3), $5):$1 }

VarDecl         :: { AST.Declaration }
    : bender id of Type                                 { AST.Variable ((TK.name . TK.tktype) $2) (Just $4) Nothing False } -- @TODO AÑADIR EL RESTO DE DECLARACIONES
    | bender id of Type Assign                          { AST.Variable ((TK.name . TK.tktype) $2) (Just $4) (Just $5) False }
    | bender id Assign                                  { AST.Variable ((TK.name . TK.tktype) $2) Nothing (Just $3) False }
    | eternal bender id of Type Assign                  { AST.Variable ((TK.name . TK.tktype) $3) (Just $5) (Just $6) True }
    | eternal bender id Assign                          { AST.Variable ((TK.name . TK.tktype) $3) Nothing (Just $4) True }
    | bender id is reincarnationOf id                   { AST.Reference ((TK.name . TK.tktype) $2) ((TK.name . TK.tktype) $5) }
    

Expr            :: { AST.Expr } 
    : char                                              { AST.ConstChar $1 }
    | '(' Expr ')'                                      { $2 }
    | id                                                { AST.Id ((TK.name . TK.tktype) $1) (TK.pos $1) }
    | ExprBlock                                         { $1 }
    | id Assign                                         { AST.Assign ((TK.name . TK.tktype) $1) $2 }
    | Expr quotmark_s id Assign                           { AST.StructAssign ((TK.name . TK.tktype) $1) ((TK.name . TK.tktype) $3) $4 }
    | using Expr quotmark_s id skill                    { AST.StructAccess $2 ((TK.name . TK.tktype) $4) }
    | opening Expr of id chakrasFrom 
        Expr to Expr colon Expr                         { AST.For ((TK.name . TK.tktype) $4) $2 $6 $8 $10 }
    | while Expr doing colon Expr                       { AST.While $2 $5 }
    | trying Expr quotmark_s id technique               { AST.UnionTrying $2 ((TK.name . TK.tktype) $4) }
    | using Expr quotmark_s id technique                { AST.UnionUsing $2 ((TK.name . TK.tktype) $4) }
    | if  Expr colon Expr otherwise Expr                { AST.If $2 $4 $6 }
    | if  Expr colon Expr dotOtherwise Expr             { AST.If $2 $4 $6 }
    | if  Expr colon Expr                               { AST.If $2 $4 AST.ConstUnit }
   
    | in id bookWith ExprList elipsis                   { AST.FunCall ((TK.name . TK.tktype) $2) (reverse $4) }
    | in id bookWith elipsis                            { AST.FunCall ((TK.name . TK.tktype) $2) [] }
    | id bookWith ExprList elipsis                      { AST.FunCall ((TK.name . TK.tktype) $1) (reverse $3) }
    | id bookWith elipsis                               { AST.FunCall ((TK.name . TK.tktype) $1) [] }

    | in id travelWith ExprList elipsis                 { AST.FunCall ((TK.name . TK.tktype) $2) (reverse $4) }
    | in id travelWith elipsis                          { AST.FunCall ((TK.name . TK.tktype) $2) [] }
    | id travelWith ExprList elipsis                    { AST.FunCall ((TK.name . TK.tktype) $1) (reverse $3) }
    | id travelWith elipsis                             { AST.FunCall ((TK.name . TK.tktype) $1) [] }

    | born Type member                                  { AST.New $2 }
    | Expr died                                         { AST.Delete $1 }
    | disciple Expr of Expr                             { AST.ArrayIndexing $2 ((TK.name . TK.tktype) $4) }

    -- >> Const Values --------------------------------------------------------------------------------
    | int                                               { AST.ConstInt $1 }
    | float                                             { AST.ConstFloat $1 }
    | true                                              { AST.ConstTrue }
    | false                                             { AST.ConstFalse }
    | string                                            { AST.ConstString $1 }
    | null                                              { AST.ConstNull }

    -- >> Binary Expressions --------------------------------------------------------------------------

    | Expr '+' Expr                                     { AST.Op2 AST.Sum $1 $3 }
    | Expr '-' Expr                                     { AST.Op2 AST.Sub $1 $3 }
    | Expr '*' Expr                                     { AST.Op2 AST.Mult $1 $3 }
    | Expr '/' Expr                                     { AST.Op2 AST.Div $1 $3 }
    | Expr '%' Expr                                     { AST.Op2 AST.Mod $1 $3 }
    | Expr '<' Expr                                     { AST.Op2 AST.Lt $1 $3 }
    | Expr '<=' Expr                                    { AST.Op2 AST.LtEq $1 $3 }
    | Expr '>' Expr                                     { AST.Op2 AST.Gt $1 $3 }
    | Expr '>=' Expr                                    { AST.Op2 AST.GtEq $1 $3 }
    | Expr '==' Expr                                    { AST.Op2 AST.Eq $1 $3 }
    | Expr '!=' Expr                                    { AST.Op2 AST.NotEq $1 $3 }
    | Expr and Expr                                     { AST.Op2 AST.And $1 $3 }
    | Expr or Expr                                      { AST.Op2 AST.Or $1 $3 }

    -- >> Unary Expressions ------------------------------------------------------------------------------
    | not Expr                                          { AST.Op1 AST.Negation $2 }
    | '-' Expr                                          { AST.Op1 AST.Negative $2 }
    | Expr unit                                         { AST.Op1 AST.UnitOperator $1 }

    -- >> Control Flow -----------------------------------------------------------------------------------
    | toBeContinued Expr                                { AST.Continue  $2 }
    | burst Expr                                        { AST.Break     $2 }
    | return Expr                                       { AST.Return    $2 }
    | toBeContinuedUnit                                 { AST.Continue  AST.ConstUnit }
    | burstUnit                                         { AST.Break     AST.ConstUnit }
    | returnUnit                                        { AST.Return    AST.ConstUnit }
    
    -- >> Evaluable and none evaluable expressions > -----------------------------------------------------
Exprs           ::  { AST.Expr }
    : Expr                                              { $1 }
    | Declaration                                       { AST.Declaration $1 } 

    -- >> Assigment ---------------------------------------------------------------------------------------
Assign          :: { AST.Expr }
    : is Expr                                           { $2 } 
    | is AssignStruct                                   { $2 }
    | is AssignUnion                                    { $2 }
    | is AssignArray                                    { $2 }

AssignStruct    :: { AST.Expr }
    : learning Type control using
        ExprList rightNow                               { AST.ConstStruct $2 (reverse $5) }

AssignUnion     :: { AST.Expr }
    : learning Type quotmark_s id 
        techniqueFrom Expr                              { AST.ConstUnion $2 ((TK.name . TK.tktype) $4) $6}

AssignArray     :: { AST.Expr }
    : masterOf ExprList rightNow                        { AST.Array (reverse $2) }

-- < Expression block Grammar > --------------------------------------------------------------------------  
ExprBlock       ::  { AST.Expr }
    : beginBlock ExprSeq endBlock                       { AST.ExprBlock (reverse $2) }
    | beginBlock endBlock                               { AST.ExprBlock [] }

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
    | id                                                { AST.CustomType ((TK.name . TK.tktype) $1) }
    | Type nation Expr year                             { AST.TArray $1 $3}
    | Type art                                          { AST.TPtr $1 }

-- < Expression > ---------------------------------------------------------------------------



{

parseError :: [TK.Token] -> a
parseError ls = error $ "por " ++ show ls

}