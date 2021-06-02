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
$alpha = [a-zA-Z]
$alphaNum = [a-zA-Z0-9]
$scaped = [\n\t\\\"\0] 

-- \n \t \\ \" \0
@str_scapedchars = \\$scaped
-- @str_scapedchars or \' 
@char_scapedchars = \\[$scaped\']
@int = $digits+
@ids = $alpha$alphaNum*
@float = $digits+\.$digits+
@comments = \-\-.*
@linebreaks = \n+\r?\r+\n?
@avatarIntro = Water\.\ Earth\.\ Fire\.\ Air\.\ Long\ ago\,\ the\ four\ nations\ lived\ together\ in\ harmony\.\ Then\,\ everything\ changed\ when\ the\ Fire\ Nation\ attacked\.\ Only\ the\ Avatar\,\ master\ of\ all\ four\ elements\,\ could\ stop\ them

tokens :-

  
        -- ignore white spaces
<0>     $white+                         ;

        -- ignore 'avatar: the last air bender' first half of intro
<0>     @avatarIntro                    ;

        -- comments
<0>     @comments                       ;

        -- reserved keywords
        
        -- declarations

<0>     bender                          { pushTK TKbender }
<0>     of                              { pushTK TKof }
<0>     eternal                         { pushTK TKeternal }

        -- asignment
<0>     is                              { pushTK TKis }

        -- reference
<0>     reincarnation\ of               { pushTK TKreincarnation }

        -- pointers
<0>     art                             { pushTK TKart }
<0>     an\ apprentice                  { pushTK TKapprentice }
<0>     born\ as                        { pushTK TKborn }
<0>     member                          { pushTK TKmember }
<0>     has\ died                       { pushTK TKdied }

        -- Data types

        -- int
<0>     air                             { pushTK TKair }
        -- float
<0>     water                           { pushTK TKwater }
        -- boolean
<0>     fire                            { pushTK TKfire } 
<0>     lightning master                { pushTK TKlightning }
<0>     fire master                     { pushTK TKfireMaster }
        -- char
<0>     earth                           { pushTK TKearth }
        -- string
<0>     metal                           { pushTK TKmetal }

        -- array 
<0>     nation\ since                   { pushTK TKnation }
<0>     years                           { pushTK TKyear }
<0>     master\ of                      { pushTK TKmasterOf }
<0>     disciple                        { pushTK TKdisciple }

        -- struct
<0>     element                         { pushTK TKelement }
<0>     is\ mastered\ by                { pushTK TKmasteredBy }
<0>     learning                        { pushTK TKlearning }
<0>     control\ from                   { pushTK TKcontrol }

        -- union
<0>     energy                          { pushTK TKenergy }
<0>     allows                          { pushTK TKallows }
<0>     technique\ of                   { pushTK TKtecgniqueOf }
<0>     bending                         { pushTK TKbending }
<0>     techniques\ from                { pushTK TKtecgniquesFrom }
<0>     using                           { pushTK TKusing }
<0>     \'s                             { pushTK TKquotmark_s }
<0>     technique                       { pushTK TKtecgnique }
<0>     trying                          { pushTK TKtrying }

        -- functions and proc
<0>     book                            { pushTK TKbook }
<0>     about                           { pushTK TKabout }
<0>     travel                          { pushTK TKtravel }
<0>     made\ by                        { pushTK TKmadeBy }

        -- operators
<0>     and\ then                       { pushTK TKandThen }
<0>     but                             { pushTK TKbut }
<0>     and\ thus                       { pushTK TKandThus }
<0>     besides                         { pushTK TKbesides }
<0>     and                             { pushTK TKand }
<0>     or                              { pushTK TKor }
<0>     not                             { pushTK TKnot }

        -- conditionals
<0>     if
<0>     otherwise

        -- other syntax is WIP

        -- literals
<0>     @float                          { pushFloat }
<0>     @int                            { pushInt }
        
        -- special characters
<0>     \,                              { pushTK TKcomma }
<0>     \:                              { pushTK TKcolon }
<0>     \.\-                            { pushTK TKbeginBlock }
<0>     \-\.                            { pushTK TKendBlock }
<0>     \.                              { pushTK TKdot }
<0>     \~                              { pushTK TKunit }
<0>     \(                              { pushTK TKopenParent }   
<0>     \)                              { pushTK TKcloseParent }

        -- strings literals
<0>     \"                              { begin strSt }
<strSt> \"                              { pushStr `andBegin` 0 }
<strSt> @str_scapedchars                { saveToStr }
<strSt> @linebreaks                     { endlError }
<strSt> $printable                      { saveToStr }
<strSt> .                               { invalidCharError }

<0>     $digits[$alphaNum\_]+           { lexError }
<0>     @ids                            { pushId }

        -- lexer error
<0>     .                               { lexError }


{

-- Initial State
stateInitial :: Int
stateInitial = 0

-- Definition needed by Alex. Still not in Alex doc :(
alexEOF :: Alex Token
alexEOF = return TKEOF

-- User state
data AlexUserState = AlexUserState
                    {
                        literalString :: String,
                        lexerErrors :: [LexerError],
                    }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                    {
                        literalString = "" ,
                        lexerErrors = [] ,
                    }

-------------------------------------------------

-- add to Tokens.hs:   type TokenConstuct = (Int, Int) -> Token

-- push Token functions
pushTK :: TokenConstuct -> AlexInput -> Int -> Alex Token
pushTK tok ( (AlexPn _ l c ) , _ , _ , _ ) len = return ( tok (l, c) )

pushInt :: AlexInput -> Int -> Alex Token
pushInt ( (AlexPn _ l c ) , _ , _ , str ) len = return ( TKInt (l, c) ( read $ take len str :: Int) )

pushFloat :: AlexInput -> Int -> Alex Token
pushFloat ( (AlexPn _ l c ) , _ , _ , str ) len = return ( TKfloat (l, c) ( read $ take len str :: Float) )

pushId :: AlexInput -> Int -> Alex Token
pushId ( (AlexPn _ l c ) , _ , _ , str ) len = return ( TKId (l, c) ( take len str ) )

pushStr :: AlexAction Token
pushStr ( (AlexPn _ l c ) , _ , _ , _ ) len = do
  str <- getLitStr
  setLitStr ""
  return ( TKstring (l, c - (length str) - 2) str )

saveToStr :: AlexAction Token
saveToStr (_, _, _, str) _ = do
  savedStr <- getLitStr
  setLitStr (savedStr ++ str)
  alexMonadScan

-- could change all get's to getAtr <atr_ust>, same idea with set's

getLitStr :: Alex String
getLitStr =
  Alex $ \s@AlexState{alex_ust=ust} -> Right (s, literalString ust)

setLitStr :: String -> Alex ()
setLitStr str =
  return $ \s -> Right (s{ alex_ust = (alex_ust s){literalString = str} }, ())


lexError = undefined
endlError = undefined
invalidCharError = undefined

scanTokens = undefined

}