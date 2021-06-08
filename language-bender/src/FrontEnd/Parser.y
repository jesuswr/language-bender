{

module FrontEnd.Parser where

import qualified FrontEnd.Tokens as TK
import qualified FrontEnd.AST    as AST
import qualified FrontEnd.Errors  as E

}

%name parseTokens
%tokentype { TK.Token }
%error { parseError }
-- %monad {}?

%token
    bender              { TK.Token _ TK.TKbender }
    of                  { TK.Token _ TK.TKof }
    eternal             { TK.Token _ TK.TKeternal }
    is                  { TK.Token _ TK.TKis }
    reincarnation       { TK.Token _ TK.TKreincarnation }
    art                 { TK.Token _ TK.TKart }
    apprentice          { TK.Token _ TK.TKapprentice }
    born                { TK.Token _ TK.TKborn }
    member              { TK.Token _ TK.TKmember }
    died                { TK.Token _ TK.TKdied }
    air                 { TK.Token _ TK.TKair }
    water               { TK.Token _ TK.TKwater }
    fire                { TK.Token _ TK.TKfire }
    lightning           { TK.Token _ TK.TKlightning }
    fireMaster          { TK.Token _ TK.TKfireMaster }
    earth               { TK.Token _ TK.TKearth }
    metal               { TK.Token _ TK.TKmetal }
    nation              { TK.Token _ TK.TKnation }
    year                { TK.Token _ TK.TKyear }
    masterOf            { TK.Token _ TK.TKmasterOf }
    disciple            { TK.Token _ TK.TKdisciple }
    element             { TK.Token _ TK.TKelement }
    compoundBy          { TK.Token _ TK.TKcompoundBy }
    skillOf             { TK.Token _ TK.TKskillOf }
    learning            { TK.Token _ TK.TKlearning }
    control             { TK.Token _ TK.TKcontrol }
    energy              { TK.Token _ TK.TKenergy }
    allows              { TK.Token _ TK.TKallows }
    techniqueOf         { TK.Token _ TK.TKtechniqueOf }
    bending             { TK.Token _ TK.TKbending }
    techniquesFrom      { TK.Token _ TK.TKtechniquesFrom }
    using               { TK.Token _ TK.TKusing }
    quotmark_s          { TK.Token _ TK.TKquotmark_s }
    technique           { TK.Token _ TK.TKtechnique }
    trying              { TK.Token _ TK.TKtrying }
    book                { TK.Token _ TK.TKbook }
    about               { TK.Token _ TK.TKabout }
    travel              { TK.Token _ TK.TKtravel }
    madeBy              { TK.Token _ TK.TKmadeBy }
    andThen             { TK.Token _ TK.TKandThen }
    but                 { TK.Token _ TK.TKbut }
    andThus             { TK.Token _ TK.TKandThus }
    besides             { TK.Token _ TK.TKbesides }
    left                { TK.Token _ TK.TKleft }
    and                 { TK.Token _ TK.TKand }
    or                  { TK.Token _ TK.TKor }
    not                 { TK.Token _ TK.TKnot }
    if                  { TK.Token _ TK.TKif }
    otherwise           { TK.Token _ TK.TKotherwise }
    int                 { TK.Token _ (TK.TKint $$) }
    float               { TK.Token _ (TK.TKfloat $$) }
    comma               { TK.Token _ TK.TKcomma }
    colon               { TK.Token _ TK.TKcolon }
    beginBlock          { TK.Token _ TK.TKbeginBlock }
    endBlock            { TK.Token _ TK.TKendBlock }
    dot                 { TK.Token _ TK.TKdot }
    unit                { TK.Token _ TK.TKunit }
    openParent          { TK.Token _ TK.TKopenParent }
    closeParent         { TK.Token _ TK.TKcloseParent }
    in                  { TK.Token _ TK.TKin }
    bookWith            { TK.Token _ TK.TKbookWith }
    with                { TK.Token _ TK.TKwith }
    lessThan            { TK.Token _ TK.TKlessThan }
    lessEqThan          { TK.Token _ TK.TKlessEqThan }
    greaterThan         { TK.Token _ TK.TKgreaterThan }
    greaterEqThan       { TK.Token _ TK.TKgreaterEqThan }
    equal               { TK.Token _ TK.TKequal }
    while               { TK.Token _ TK.TKwhile }
    doing               { TK.Token _ TK.TKdoing }
    opening             { TK.Token _ TK.TKopening }
    chakrasFrom         { TK.Token _ TK.TKchakrasFrom }
    to                  { TK.Token _ TK.TKto }
    char                { TK.Token _ (TK.TKchar $$) }
    string              { TK.Token _ (TK.TKstring $$) }
    id                  { TK.Token _ (TK.TKid $$ ) }
    elipsis             { TK.Token _ TK.TKelipsis }
    toBeContinued       { TK.Token _ TK.TKtoBeContinued }
    burst               { TK.Token _ TK.TKburst }
    return              { TK.Token _ TK.TKreturn }
 

%%
-- Grammar

-- Source Symbol
Program :: { AST.Program }
Program : Declarations { AST.Program (reverse $1) }

-- Program as declaration list
Declarations    :: { [AST.Declaration] }
Declarations    : Declaration { [$1] }
                | Declarations Declaration { $2:$1 }

Declaration :: { AST.Declaration }
Declaration : element id compoundBy StructIdDecls               { AST.Struct $2 (reverse $4) }
            
StructIdDecls   :: { [(String, AST.Type)] }
                :  id skillOf Type                              {[($1, $3)]}
                |  StructIdDecls comma id skillOf Type          { ($3, $5):$1 }

Type        :: { AST.Type }
Type        :  water                    { AST.TFloat }
            |  air                      { AST.TInt }
            |  earth                    { AST.TChar }
            |  string                   { AST.TString }   
            |  fire                     { AST.TBool }
            |  id                       { AST.CustomType $1 } -- RECORDAR ARREGLOS @TODO
            |  Type art                 { AST.TPtr $1}





{

parseError :: [TK.Token] -> a
parseError ls = error $ "por " ++ show ls

}