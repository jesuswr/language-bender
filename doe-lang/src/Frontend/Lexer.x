{
module FrontEnd.Lexer (
    scanTokens,
    ) where

--import Data.List.Extra          (replace)
--import FrontEnd.Errors
import FrontEnd.Tokens
--import Utils
}

%wrapper "monadUserState"

-- macros for sets and regex
$digits = [0-9]
$characters = [^\\\|]

@ids = [A-Za-z][A-Za-z0-9_]*
@scapedchars = \\[nt\\\|]
@strings = \" ([$characters # \"] | "\"" | @scapedchars)*\"
@comments = \-\-.*
@avatarIntro = Water\.\ Earth\.\ Fire\.\ Air\.\ Long\ ago\,\ the\ four\ nations\ lived\ together\ in\ harmony\.\ Then\,\ everything\ changed\ when\ the\ Fire\ Nation\ attacked\.\ Only\ the\ Avatar\,\ master\ of\ all\ four\ elements\,\ could\ stop\ them

tokens :-

  
    -- ignore white spaces
    [$white ]+                         ;

    -- ignore 'avatar: the last air bender' first half of intro
    @avatarIntro                    ;

    -- comments
    @comments                       ;

    -- reserved keywords
    
    -- declarations

    bender                          { pushTK TKbender }
    of                              { pushTK TKof }
    eternal                         { pushTK TKeternal }

    -- asignment
    is                              { pushTK TKis }

    -- reference
    reincarnation\ of               { pushTK TKreincarnation }

    -- pointers
    art                             { pushTK TKart }
    an\ apprentice                  { pushTK TKapprentice }
    born\ as                        { pushTK TKborn }
    member                          { pushTK TKmember }
    has\ died                       { pushTK TKdied }

    -- Data types

    -- int
    air                             { pushTK TKair }
    -- float
    water                           { pushTK TKwater }
    -- boolean
    fire                            { pushTK TKfire } 
    lightning master                { pushTK TKlightning }
    fire master                     { pushTK TKfireMaster }
    -- char
    earth                           { pushTK TKearth }
    -- string
    metal                           { pushTK TKmetal }

    -- array 
    nation\ since                   { pushTK TKnation }
    years                           { pushTK TKyear }
    master\ of                      { pushTK TKmasterOf }
    disciple                        { pushTK TKdisciple }

    -- struct
    element                         { pushTK TKelement }
    is\ mastered\ by                { pushTK TKmasteredBy }
    learning                        { pushTK TKlearning }
    control\ from                   { pushTK TKcontrol }

    -- union
    energy                          { pushTK TKenergy }
    transforms\ into                { pushTK TKtransformsInto }
    bended\ by                      { pushTK TKbendedBy }
    transforming                    { pushTK TKtransforming }
    into                            { pushTK TKinto }
    -- missing acces union and consult union.

    -- other syntax is WIP

    -- literals
    $digits+                        { pushTK TKint }
    $digits+\.$digits+              { pushTK TKfloat }
    @strings                        { pushTK TKstring }

    -- special characters
    \,

    @ids                            { pushTK TKid }

    -- lexer error
    .                               { pushError }


{

pushTK = undefined

pushError = undefined
  
-- This isn't on the documentation but required by Alex
alexEOF :: Alex AlexUserState
alexEOF = getUserState

data AlexUserState = LexerResult [Error] [Token]

alexInitUserState :: AlexUserState
alexInitUserState = LexerResult [] []

getUserState :: Alex AlexUserState
getUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, ust)


scanTokens = undefined

}