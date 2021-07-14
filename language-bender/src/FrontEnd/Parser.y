{

module FrontEnd.Parser where

import qualified FrontEnd.Tokens as TK
import qualified FrontEnd.AST    as AST
import qualified FrontEnd.Errors  as E
import qualified FrontEnd.StaticAnalyis as SA

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
%right techniqueFrom


%%
-- Grammar

-- Source Symbol
Program         :: { SA.AnalyzerState AST.Program }
    : Declarations                                      { 
                                                            do
                                                            ls <- $1 
                                                            return . AST.Program . reverse $ ls 
                                                        }

-- Program as declaration list
Declarations    :: { SA.AnalyzerState [AST.Declaration] }
    : Declaration dot                                   { do
                                                            l <- $1
                                                            
                                                            return [l] 
                                                        }
    | Declarations Declaration dot                      { do
                                                            l  <- $2
                                                            ls <- $1 
                                                            
                                                            return l:ls
                                                        }


Declaration     :: { SA.AnalyzerState AST.Declaration }
    : VarDecl                                           { $1 }

VarDecl         :: { AST.Declaration }
    : bender id of Type                                 { AST.Variable ((TK.name . TK.tktype) $2) (Just $4) Nothing False } -- @TODO AÑADIR EL RESTO DE DECLARACIONES
    | bender id of Type Assign                          { AST.Variable ((TK.name . TK.tktype) $2) (Just $4) (Just $5) False }
    | bender id Assign                                  { AST.Variable ((TK.name . TK.tktype) $2) Nothing (Just $3) False }
    | eternal bender id of Type Assign                  { AST.Variable ((TK.name . TK.tktype) $3) (Just $5) (Just $6) True }
    | eternal bender id Assign                          { AST.Variable ((TK.name . TK.tktype) $3) Nothing (Just $4) True }
    | bender id is reincarnationOf id                   { AST.Reference ((TK.name . TK.tktype) $2) ((TK.name . TK.tktype) $5) }

    -- >> Expressions --------------------------------------------------------------------------

Expr            :: { SA.AnalyzerState AST.Expr }  
    : '(' Expr ')'                                      { $2 }
    | id                                                {  SA.checkExpr $ AST.Id ((TK.name . TK.tktype) $1) (TK.pos $1) }
    

    -- >> Types -------------------------------------------------------------------------------------
Type            :: { SA.AnalyzerState AST.Type }
    : water                                             { return AST.TFloat }
    | air                                               { return AST.TInt }
    | earth                                             { return AST.TChar }
    | metal                                             { return AST.TString }   
    | fire                                              { return AST.TBool }
    | id                                                { return SA.checkType AST.CustomType ((TK.name . TK.tktype) $1) }
    | Type nation Expr year                             { AST.TArray $1 $3}
    | Type art                                          { AST.TPtr $1 }


{

-- Error function
parseError :: [TK.Token] -> a
parseError []       = error "[Error]: Parse error after the end of file.\n"
parseError (tk:tks) = error $ "[Error]: Parse error at: " ++ (show tk) ++ "\n"

}