{-
    File defining token data type
-}

module FrontEnd.Tokens (
    Token(..),
    TokenType(..)
    ) where

import FrontEnd.Utils -- Position

-- Token Data
data Token = Token {pos :: Position, tktype :: TokenType} deriving(Eq)

-- Token Type Information
data TokenType =
    TKbender                             |
    TKReference                          |
    TKof                                 |
    TKeternal                            |
    TKis                                 |
    TKreincarnation                      |
    TKart                                |
    TKapprentice                         |
    TKborn                               |
    TKmember                             |
    TKdied                               |
    TKair                                |
    TKwater                              |
    TKfire                               |
    TKlightning                          |
    TKfireMaster                         |
    TKearth                              |
    TKmetal                              |
    TKnation                             |
    TKyear                               |
    TKmasterOf                           |
    TKRightNow                           |
    TKdisciple                           |
    TKelement                            |
    TKcompoundBy                         |
    TKskillOf                            |
    TKskill                              |
    TKlearning                           |
    TKcontrol                            |
    TKenergy                             |
    TKallows                             |
    TKtechniqueOf                        |
    TKbending                            |
    TKtechniqueFrom                      |
    TKusing                              |
    TKquotmark_s                         |
    TKtechnique                          |
    TKtrying                             |
    TKbook                               |
    TKabout                              |
    TKtravel                             |
    TKmadeBy                             |
    TKandThen                            |
    TKbut                                |
    TKandThus                            |
    TKbesides                            |
    TKleft                               |
    TKand                                |
    TKor                                 |
    TKnot                                |
    TKif                                 |
    TKotherwise                          |
    TKdotOtherwise                       |
    TKint             {numI :: Int}      |
    TKfloat           {numF :: Float}    |
    TKcomma                              |
    TKcolon                              |
    TKbeginBlock                         |
    TKendBlock                           |
    TKdot                                |
    TKunit                               |
    TKopenParent                         |
    TKcloseParent                        |
    TKin                                 |
    TKbookWith                           |
    TKtravelWith                         |
    TKlessThan                           |
    TKlessEqThan                         |
    TKgreaterThan                        |
    TKgreaterEqThan                      |
    TKequal                              |
    TKnotEqual                           |
    TKwhile                              |
    TKdoing                              |
    TKopening                            |
    TKchakrasFrom                        |
    TKto                                 |
    TKchar            {char  :: String}  |
    TKstring          {str :: String}    |
    TKid              {name  :: String}  |
    TKelipsis                            |
    TKtoBeContinued                      |
    TKtoBeContinuedUnit                  |
    TKburst                              |
    TKburstUnit                          |
    TKreturn                             |
    TKreturnUnit

    deriving(Eq, Show)

instance Show Token where
    show (Token pos TKint{numI = n} )    = "Literal Integer: '" ++ show n ++ showPos pos
    show (Token pos TKfloat{numF = n} )  = "Literal Float: '" ++ show n ++ showPos pos
    show (Token pos TKchar{char = c} )   = "Literal Char: '" ++ c ++ showPos pos
    show (Token pos TKstring{str = s} )  = "Literal String: '" ++ s ++ showPos pos
    show (Token pos TKid{name = id_} )   = "Identifier token: '" ++ id_ ++ showPos pos
    show (Token pos token )              = "Token: '" ++ show token ++ showPos pos

showPos :: Position -> String
showPos pos = "' -- at position: " ++ show pos

