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
%expect 145

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
    purity              { TK.Token _ TK.TKpurity}
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
    deref               { TK.Token _ TK.TKDeref }
    is_                 { TK.Token _ TK.TKDerefAssign }
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
%right deref
%left and or

%nonassoc '<' '<=' '>' '>=' 
%left '==' '!='
%left '+' '-'
%left '*' '/' '%'

%right not NEG DEREF 
%right techniqueFrom


%%
-- Grammar

-- Source Symbol
Program        
    : Declarations                                                                       { }

-- Program as declaration list
Declarations   
    : Declaration dot                                                                    { }
    | Declarations Declaration dot                                                       { }

Declaration     :: { () } 
    : element id compoundBy PushScope StructIdDecls PopScope                             {% P.preCheckDecls $ AST.Struct ((TK.name . TK.tktype) $2) [] 0 0 0 0 }
    | energy id allows PushScope UnionIdDecls PopScope                                   {% P.preCheckDecls $ AST.Union  ((TK.name . TK.tktype) $2) [] 0 0 0 0 }
    | VarDecl                                                                            { () }
    | FuncDecl                                                                           { () }
    | ProcDecl                                                                           { () }

ProcDecl        :: { () }
    : travel id madeBy PushScope FuncArg colon PushScope Exprs PopScope PopScope         {% P.preCheckDecls $ AST.Func ((TK.name . TK.tktype) $2) (reverse $5) AST.TUnit (AST.ConstUnit AST.TUnit) 0 0 }
    | travel id PushScope colon PushScope Exprs PopScope PopScope                        {% P.preCheckDecls $ AST.Func ((TK.name . TK.tktype) $2) [] AST.TUnit (AST.ConstUnit AST.TUnit) 0 0 }

FuncDecl        :: { () }
    : book id of Type about PushScope FuncArg colon PushScope Exprs PopScope PopScope    {% P.preCheckDecls $ AST.Func ((TK.name . TK.tktype) $2) (reverse $7) $4 (AST.ConstUnit AST.TUnit) 0 0 }
    | book id of Type PushScope colon PushScope Exprs PopScope PopScope                  {% P.preCheckDecls $ AST.Func ((TK.name . TK.tktype) $2) [] $4 (AST.ConstUnit AST.TUnit) 0 0 }
    | book id about PushScope FuncArg colon PushScope Exprs PopScope PopScope            {% P.preCheckDecls $ AST.Func ((TK.name . TK.tktype) $2) (reverse $5) AST.TVoid (AST.ConstUnit AST.TUnit) 0 0 }
    | book id PushScope colon PushScope Exprs PopScope PopScope                          {% P.preCheckDecls $ AST.Func ((TK.name . TK.tktype) $2) [] AST.TVoid (AST.ConstUnit AST.TUnit) 0 0 }

FuncArg         :: { [AST.FuncArg] }
    : FuncDefArgDecl                                                                     { $1 }
    | FuncArgDecl                                                                        { $1 }
    | FuncArgDecl comma FuncDefArgDecl                                                   { ($3 ++ $1) }

FuncDefArgDecl :: { [AST.FuncArg] }
    : SingleDefArgDecl                                                                   { [$1] }
    | FuncDefArgDecl comma Type bender id Assign                                         {% (P.preCheckFunArg $ AST.FuncArg ((TK.name . TK.tktype) $5) $3 Nothing 0) >>= (return . (:$1)) }
    | FuncDefArgDecl comma Type '&' bender id Assign                                     {% (P.preCheckFunArg $ AST.FuncArg ((TK.name . TK.tktype) $6) (AST.TReference $3) Nothing 0) >>= (return . (:$1)) }

SingleDefArgDecl :: { AST.FuncArg }
    : Type bender id Assign                                                              {% P.preCheckFunArg $ AST.FuncArg ((TK.name . TK.tktype) $3) $1 Nothing 0 }
    | Type '&' bender id Assign                                                          {% P.preCheckFunArg $ AST.FuncArg ((TK.name . TK.tktype) $4) (AST.TReference $1) Nothing 0 }

FuncArgDecl     :: { [AST.FuncArg] }
    : SingleFuncArgDecl                                                                  { [$1] }
    | FuncArgDecl comma Type bender id                                                   {% (P.preCheckFunArg $ AST.FuncArg ((TK.name . TK.tktype) $5) $3 Nothing 0) >>= (return . (:$1)) }
    | FuncArgDecl comma Type '&' bender id                                               {% (P.preCheckFunArg $ AST.FuncArg ((TK.name . TK.tktype) $6) (AST.TReference $3) Nothing 0) >>= (return . (:$1)) }

SingleFuncArgDecl :: { AST.FuncArg }
    : Type bender id                                                                     {% P.preCheckFunArg $ AST.FuncArg ((TK.name . TK.tktype) $3) $1 Nothing 0 }
    | Type '&' bender id                                                                 {% P.preCheckFunArg $ AST.FuncArg ((TK.name . TK.tktype) $4) (AST.TReference $1) Nothing 0 }

StructIdDecls   
    : id skillOf Type                                                                    {}
    | StructIdDecls comma id skillOf Type                                                {}

UnionIdDecls    
    : id techniqueOf Type bending                                                        {}
    | UnionIdDecls comma id techniqueOf Type bending                                     {}

VarDecl         
    : bender id of Type                                                                  {}
    | bender id of Type Assign                                                           {} 
    | bender id Assign                                                                   {}
    | eternal bender id of Type Assign                                                   {} 
    | eternal bender id Assign                                                           {}
    | bender id is reincarnationOf id                                                    {}
    

    -- >> Expressions --------------------------------------------------------------------------

Expr           
    : '(' Expr ')'                                                                       {}
    | id                                                                                 {}
    | ExprBlock                                                                          {}
    | id Assign                                                                          {}
    
    | Expr quotmark_s id Assign                                                          {}
    | using Expr quotmark_s id skill                                                     {}
    | learning id control using
        ExprList rightNow                                                                {}
    
    | trying Expr quotmark_s id technique                                                {}
    | using Expr quotmark_s id technique                                                 {}
    | learning id quotmark_s id 
        techniqueFrom Expr                                                               {}
    
    | opening Expr of id chakrasFrom 
        Expr to Expr colon PushScope PushScope Expr PopScope PopScope                    {}
    | while Expr doing colon PushScope Expr PopScope                                     {}
    | if  Expr colon PushScope Expr PopScope otherwise PushScope Expr PopScope           {}
    | if  Expr colon PushScope Expr PopScope dotOtherwise PushScope Expr PopScope        {}
    | if  Expr colon PushScope Expr PopScope                                             {}
    | if  Expr dot colon PushScope Expr PopScope otherwise PushScope Expr PopScope       {}
    | if  Expr dot colon PushScope Expr PopScope dotOtherwise PushScope Expr PopScope    {}
    | if  Expr dot colon PushScope Expr PopScope                                         {}
   
    | in id bookWith ExprList elipsis                                                    {}
    | in id bookWith elipsis                                                             {}
    | id bookWith ExprList elipsis                                                       {}
    | id bookWith elipsis                                                                {} 

    | in id travelWith ExprList elipsis                                                  {}
    | in id travelWith elipsis                                                           {} 
    | id travelWith ExprList elipsis                                                     {}
    | id travelWith elipsis                                                              {} 

    | born Type member                                                                   {}
    | Expr died                                                                          {}
    | deref Expr is_ Expr                                                                {}

    | disciple Expr of Expr                                                              {}
    | masterOf ExprList rightNow                                                         {} 

    -- >> Const Values --------------------------------------------------------------------------------
    | int                                                                                {} 
    | float                                                                              {}
    | true                                                                               {}
    | false                                                                              {}
    | char                                                                               {}
    | string                                                                             {}
    | null                                                                               {}

    -- >> Binary Expressions --------------------------------------------------------------------------

    | Expr '+' Expr                                                                      {}
    | Expr '-' Expr                                                                      {}
    | Expr '*' Expr                                                                      {}
    | Expr '/' Expr                                                                      {}
    | Expr '%' Expr                                                                      {}
    | Expr '<' Expr                                                                      {}
    | Expr '<=' Expr                                                                     {}
    | Expr '>' Expr                                                                      {}
    | Expr '>=' Expr                                                                     {}
    | Expr '==' Expr                                                                     {}
    | Expr '!=' Expr                                                                     {}
    | Expr and Expr                                                                      {}
    | Expr or Expr                                                                       {}

    -- >> Unary Expressions ------------------------------------------------------------------------------
    | not Expr                                                                           {}
    | '-' Expr %prec NEG                                                                 {}
    | Expr unit                                                                          {}
    | deref Expr %prec DEREF                                                             {}

    -- >> Control Flow -----------------------------------------------------------------------------------
    | toBeContinued Expr                                                                 {}
    | burst Expr                                                                         {}
    | return Expr                                                                        {}
    | toBeContinuedUnit                                                                  {}
    | burstUnit                                                                          {}
    | returnUnit                                                                         {}
    
    -- >> Evaluable and none evaluable expressions > -----------------------------------------------------
Exprs         
    : Expr                                                                               {}
    | Declaration                                                                        {}

    -- >> Assigment ---------------------------------------------------------------------------------------
Assign          
    : is Expr                                                                            {} 


-- < Expression block Grammar > --------------------------------------------------------------------------  
ExprBlock      
    : beginBlock PushScope ExprSeq PopScope endBlock                                     {}
    | beginBlock PushScope PopScope endBlock                                             {}

ExprSeq         
    : LastInBlock                                                                        {}
    | Seq LastInBlock                                                                    {}

Seq             
    : Exprs Dots                                                                         {}
    | Seq Exprs Dots                                                                     {}
    | Dots                                                                               {}
    
LastInBlock    
    : Exprs                                                                              {}
    | Exprs Dots                                                                         {}

Dots 
    : dot                                                                                {}
    | Dots dot                                                                           {}

ExprList
    : Expr                                                                               {}
    | ExprList comma Expr                                                                {}

    -- >> Types -------------------------------------------------------------------------------------
Type            :: { AST.Type }
    : water                                             { AST.TFloat }
    | air                                               { AST.TInt }
    | earth                                             { AST.TChar }
    | metal Expr purity                                 { AST.TArray AST.TChar (AST.ConstInt 0 AST.TInt) }   
    | fire                                              { AST.TBool }
    | id                                                {% P.getCustomType ((TK.name . TK.tktype) $1) }
    | Type nation Expr year                             { AST.TArray $1 (AST.ConstInt 0 AST.TInt)}
    | Type art                                          { AST.TPtr $1 }


    -- >> Auxiliar Rules ----------------------------------------------------------------------------

PushScope 
    : {- empty -}                                                                        {% P.pushEmptyScope }

PopScope
    : {- empty -}                                                                        {% P.popEmptyScope }

{

-- Error function
-- parseError :: [TK.Token] -> a
parseError []       = P.addStaticError SE.UnexpectedEOF >> (fail . show) SE.UnexpectedEOF 
parseError rem@(tk:tks) = P.addStaticError (SE.ParseError rem)    >> (fail $ "parse error in "++ (show tk)) -- (fail . show) (SE.ParseError rem)


-- Run the preparser
runPreParse :: [TK.Token] -> IO (P.ParsingState, P.ErrorLog)
runPreParse tks =
    RWS.execRWST (preParseTokens tks) () P.startingState

}