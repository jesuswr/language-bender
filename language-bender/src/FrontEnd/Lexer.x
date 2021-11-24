{
module FrontEnd.Lexer (
        scanTokens,
        ) where

import qualified FrontEnd.Errors as E
import FrontEnd.Tokens
import FrontEnd.Utils
}

%wrapper "monadUserState"

-- macros for sets and regex
$digits = [0-9]
$alpha = [a-zA-Z]
$alphaNum = [a-zA-Z0-9]
$scaped = [n t \\ 0 ]
$ascii_char = [\0-\127] # [\'\\]
$ascii_str = [\0-\127] # [\"\\]

-- \n \t \\ \0 \"
@str_scapedchars = \\[$scaped \"]
-- \n \t \\ \0 \' 
@char_scapedchars = \\[$scaped \']
@chars = \'(@char_scapedchars | [$ascii_char])\'
@int = $digits+
@ids = [$alpha\_][$alphaNum\_]*
@float = $digits+\.$digits+
@comments = \-\-.*
@linebreaks = \n+\r?\r+\n?
@wn = [\ \n \t \r]
@w = (@wn | @comments\n)+
@avatarIntro = Water@w*\.@w Earth@w*\.@w Fire@w*\.@w Air@w*\.@w Long@w ago@w*\,@w the@w four@w nations@w lived@w together@w in@w harmony@w*\.@w Then@w*\,@w everything@w changed@w when@w the@w Fire@w Nation@w attacked@w*\.@w Only@w the@w Avatar@w*\,@w master@w of@w all@w four@w elements@w*\,@w could@w stop@w them@w*\.

tokens :-

    
                -- ignore white spaces
<0>     $white+                                          ;

                -- ignore 'avatar: the last air bender' first half of intro
<0>     @avatarIntro                                     ;

                -- comments
<0>     @comments                                        ;

                -- reserved keywords
                
                -- declarations

<0>     bender                                           { pushTK TKbender }
<0>     of                                               { pushTK TKof }
<0>     eternal                                          { pushTK TKeternal }
                -- asignment
<0>     is                                               { pushTK TKis }

                -- reference
<0>     reincarnation @w of                              { pushTK TKreincarnation }
<0>     reincarnation                                    { pushTK TKReference }
<0>     reincarnation(\ reincarnation)+                  { lexError }

                -- pointers
<0>     art                                              { pushTK TKart }
<0>     an @w apprentice                                 { pushTK TKapprentice }
<0>     born @w as                                       { pushTK TKborn }
<0>     member                                           { pushTK TKmember }
<0>     has @w died                                      { pushTK TKdied }

                -- Data types

                -- int
<0>     air                                              { pushTK TKair }
                -- float
<0>     water                                            { pushTK TKwater }
                -- boolean
<0>     fire                                             { pushTK TKfire } 
<0>     lightning @w master                              { pushTK TKlightning }
<0>     fire @w master                                   { pushTK TKfireMaster }
                -- char
<0>     earth                                            { pushTK TKearth }
                -- string
<0>     metal @w with                                    { pushTK TKmetal }
<0>     purity                                           { pushTK TKpurity }

                -- array 
<0>     nation @w since                                  { pushTK TKnation }
<0>     years                                            { pushTK TKyear }
<0>     master @w of                                     { pushTK TKmasterOf }
<0>     right @w now                                     { pushTK TKRightNow }
<0>     disciple                                         { pushTK TKdisciple }

                -- struct
<0>     element                                          { pushTK TKelement }
<0>     is @w compound @w by                             { pushTK TKcompoundBy }
<0>     skill @w of                                      { pushTK TKskillOf }
<0>     skill                                            { pushTK TKskill }
<0>     learning                                         { pushTK TKlearning }
<0>     control                                          { pushTK TKcontrol }

                -- union
<0>     energy                                           { pushTK TKenergy }
<0>     allows                                           { pushTK TKallows }
<0>     technique @w of                                  { pushTK TKtechniqueOf }
<0>     bending                                          { pushTK TKbending }
<0>     technique @w from                                { pushTK TKtechniqueFrom }
<0>     using                                            { pushTK TKusing }
<0>     \'s                                              { pushTK TKquotmark_s }
<0>     technique                                        { pushTK TKtechnique }
<0>     trying                                           { pushTK TKtrying }

                -- functions and proc
<0>     book                                             { pushTK TKbook }
<0>     about                                            { pushTK TKabout }
<0>     travel                                           { pushTK TKtravel }
<0>     made @w by                                       { pushTK TKmadeBy }

                -- operators
<0>     and @w then                                      { pushTK TKandThen }
<0>     but                                              { pushTK TKbut }
<0>     and @w thus                                      { pushTK TKandThus }
<0>     besides                                          { pushTK TKbesides }
<0>     left                                             { pushTK TKleft }
<0>     and                                              { pushTK TKand }
<0>     or                                               { pushTK TKor }
<0>     not                                              { pushTK TKnot }
<0>     artwork @w of                                    { pushTK TKDeref }
<0>     really @w is                                     { pushTK TKDerefAssign }

                -- conditionals
<0>     if                                               { pushTK TKif }
<0>     \. @w otherwise                                  { pushTK TKdotOtherwise} 
<0>     otherwise                                        { pushTK TKotherwise }

                -- other syntax is WIP

                -- literals
<0>     @float                                           { pushFloat }
<0>     @int                                             { pushInt }
<0>     \-                                               { pushTK TKbut }
                
                -- special characters
<0>     \,                                               { pushTK TKcomma }
<0>     \:                                               { pushTK TKcolon }
<0>     \.\-                                             { pushTK TKbeginBlock }
<0>     \-\.                                             { pushTKlist [TKendBlock, TKdot] }
<0>     \.                                               { pushTK TKdot }
<0>     \~                                               { pushTK TKunit }
<0>     \(                                               { pushTK TKopenParent }   
<0>     \)                                               { pushTK TKcloseParent }
<0>     \.\.\.                                           { pushTK TKelipsis }
<0>     \.\-\-\.                                         { pushTKlist [TKbeginBlock, TKendBlock, TKdot] }

                -- function calls
<0>     in                                               { pushTK TKin }
<0>     book @w with                                     { pushTK TKbookWith }

                -- proc calls
<0>     travel @w with                                   { pushTK TKtravelWith }

                -- compare operators
<0>     is @w less @w than                               { pushTK TKlessThan }
<0>     is @w less @w or @w equal @w than                { pushTK TKlessEqThan }
<0>     is @w greater @w than                            { pushTK TKgreaterThan }
<0>     is @w greater @w or @w equal @w than             { pushTK TKgreaterEqThan }
<0>     is @w equal @w to                                { pushTK TKequal }
<0>     is @w not @w equal @w to                         { pushTK TKnotEqual }

                -- while
<0>     while                                            { pushTK TKwhile }
<0>     doing                                            { pushTK TKdoing }

                -- for
<0>     opening                                          { pushTK TKopening }
<0>     chakras @w from                                  { pushTK TKchakrasFrom }
<0>     to                                               { pushTK TKto }

                -- control flow

<0>     to @w be @w continued                            { pushTK TKtoBeContinued }
<0>     burst                                            { pushTK TKburst }
<0>     this @w story @w comes @w to @w an @w end        { pushTK TKreturn }
<0>     to @w be @w continued @w \~                      { pushTK TKtoBeContinuedUnit }
<0>     burst @w \~                                      { pushTK TKburstUnit }
<0>     this @w story @w comes @w to @w an @w end @w \~  { pushTK TKreturnUnit }

                -- strings literals
<0>     \"                                               { startStr `andBegin` strSt }
<strSt> \"                                               { pushStr `andBegin` 0 }
<strSt> @str_scapedchars                                 { saveToStr }
<strSt> @linebreaks                                      { skip }
<strSt> [\n\r\t]                                         { skip }
<strSt> $ascii_str                                       { saveToStr }
<strSt> .                                                { invalidCharError }

                -- chars
<0>     @chars                                           { pushChar }

<0>     $digits[$alphaNum\_]+                            { lexError }
<0>     @ids                                             { pushId }

                -- lexer error
<0>     .                                                { lexError }


{

-- Initial State
stateInitial :: Int
stateInitial = 0

-- Definition needed by Alex. Still not in Alex doc :(
alexEOF :: Alex AlexUserState
alexEOF = getUSt

-- return user state in EOF
getUSt :: Alex AlexUserState
getUSt = do
    startCode <- alexGetStartCode
    case startCode of
        0 -> getUSt'
        _ -> do
            pos <- getAtr strPosition
            addError $ E.LexerError pos E.UnexpectedEOF
            getUSt'  
    
getUSt' :: Alex AlexUserState
getUSt' = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, ust)

-- User state
data AlexUserState = AlexUserState
                                        {
                                                strPosition :: Position,
                                                literalString :: String,
                                                lexerErrors :: [E.Error],
                                                lexerTokens :: [Token]
                                        }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                                        {
                                                strPosition = Position 0 0,
                                                literalString = "" ,
                                                lexerErrors = [] ,
                                                lexerTokens = []
                                        }


getAtr :: (AlexUserState -> a) -> Alex a
getAtr atr = 
    Alex $ \s@AlexState{alex_ust=ust} -> Right (s, atr ust)

-------------------------------------------------

-- manage tokens in state

getTokensSt :: Alex [Token]
getTokensSt = getAtr lexerTokens

setTokensSt :: [Token] -> Alex ()
setTokensSt tks = Alex $ \s@AlexState{alex_ust=ust}
    -> Right (s{ alex_ust = (alex_ust s){lexerTokens = tks} }, ())

addToken :: Token -> Alex ()
addToken tk = do
    tks <- getTokensSt
    setTokensSt (tk:tks)

-- push Token functions
pushTK :: TokenType ->  AlexAction AlexUserState
pushTK tok ( (AlexPn _ l c ) , _ , _ , _ ) len = do
    addToken (Token (Position l c) tok)
    alexMonadScan

pushTKlist :: [TokenType] ->  AlexAction AlexUserState
pushTKlist toks ( (AlexPn _ l c ) , _ , _ , _ ) _ = do
    mapM_ (pushTK') toks
    alexMonadScan
  where
    pushTK' tok = addToken (Token (Position l c) tok)

pushInt :: AlexAction AlexUserState
pushInt ( (AlexPn _ l c ) , _ , _ , str ) len = do 
    addToken ( Token (Position l c) $ TKint ( read $ take len str :: Int) )
    alexMonadScan

pushFloat :: AlexAction AlexUserState
pushFloat ( (AlexPn _ l c ) , _ , _ , str ) len = do
    addToken ( Token (Position l c) $ TKfloat ( read $ take len str :: Float) )
    alexMonadScan

pushId :: AlexAction AlexUserState
pushId ( (AlexPn _ l c ) , _ , _ , str ) len = do
    addToken ( Token (Position l c) . TKid $ take len str  )
    alexMonadScan

pushChar :: AlexAction AlexUserState
pushChar ( (AlexPn _ l c ) , _ , _ , str ) len = do
    addToken ( Token (Position l c) $ TKchar str' )
    alexMonadScan
        where
            str' = take len str

pushStr :: AlexAction AlexUserState
pushStr ( _ , _ , _ , _ ) _ = do
    str <- getLitStr
    setLitStr ""
    (Position l c) <- getAtr strPosition
    addToken ( Token (Position l c) . TKstring $ str )
    alexMonadScan

startStr :: AlexAction AlexUserState
startStr ( (AlexPn _ l c ) , _ , _ , _ ) _ = do
    Alex $ \s@AlexState{alex_ust=ust}
        -> Right (s{ alex_ust = (alex_ust s){strPosition = (Position l c)} }, ())
    alexMonadScan

saveToStr :: AlexAction AlexUserState
saveToStr (_, _, _, str) len = do
    savedStr <- getLitStr
    setLitStr (savedStr ++ (take len str))
    alexMonadScan

getLitStr :: Alex String
getLitStr = getAtr literalString

setLitStr :: String -> Alex ()
setLitStr str = Alex $ \s@AlexState{alex_ust=ust}
    -> Right (s{ alex_ust = (alex_ust s){literalString = str} }, ())

-- manage error in the state

getErrorsSt :: Alex [E.Error]
getErrorsSt = getAtr lexerErrors

setErrorsSt :: [E.Error] -> Alex ()
setErrorsSt tks = Alex $ \s@AlexState{alex_ust=ust}
    -> Right (s{ alex_ust = (alex_ust s){lexerErrors = tks} }, ())

addError :: E.Error -> Alex ()
addError err = do
    errs <- getErrorsSt
    setErrorsSt (err:errs)

lexError :: AlexAction AlexUserState
lexError ((AlexPn _ l c), _, _, str) len = do
    addError (E.LexerError (Position l c) (E.InvalidToken $ take len str))
    alexMonadScan

invalidCharError :: AlexAction AlexUserState
invalidCharError ((AlexPn _ l c), _, _, str) len = do
    addError (E.LexerError (Position l c) (E.InvalidStrChar $ take 1 str))
    alexMonadScan


-- Scanner (Tokenizer)

scanTokens :: String -> ([E.Error], [Token])
scanTokens str = case runAlex str alexMonadScan of
    Left e -> do
        error $ "Alex error: " ++ show e
    Right ust ->
        let AlexUserState _ _ errors tokens = ust in (
                        reverse errors,
                        map postProcess $ reverse tokens)

removeBorder :: [a] -> [a]
removeBorder = init . tail

postProcess :: Token -> Token
postProcess (Token p (TKchar s) )= Token p . TKchar $ f s
    where
        f s = if head a == '\\' then mapEscaped $ last a else a
        a = removeBorder s
        mapEscaped 'n' = "\n"
        mapEscaped 't' = "\t"
        mapEscaped '\\' = "\\"
        mapEscaped '\'' = "\'"
        mapEscaped '0' = "\0"
postProcess (Token p (TKstring s)) = Token p $ TKstring ss
    where
        ss = postProcess' s
postProcess a = a        

postProcess' [] = []
postProcess' ('\\':'n':xs) = '\n':postProcess' xs
postProcess' ('\\':'t':xs) = '\t':postProcess' xs
postProcess' ('\\':'\\':xs) = '\\':postProcess' xs
postProcess' ('\\':'"':xs) = '\"':postProcess' xs
postProcess' ('\\':'0':xs) = '\0':postProcess' xs
postProcess' (x:xs) = x:postProcess' xs

}