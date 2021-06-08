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
    TKdisciple                           |
    TKelement                            |
    TKcompoundBy                         |
    TKskillOf                            |
    TKlearning                           |
    TKcontrol                            |
    TKenergy                             |
    TKallows                             |
    TKtechniqueOf                        |
    TKbending                            |
    TKtechniquesFrom                     |
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
    TKwith                               |
    TKlessThan                           |
    TKlessEqThan                         |
    TKgreaterThan                        |
    TKgreaterEqThan                      |
    TKequal                              |
    TKwhile                              |
    TKdoing                              |
    TKopening                            |
    TKchakrasFrom                        |
    TKto                                 |
    TKchar            {char  :: String}  |
    TKstring          {str :: String}    |
    TKid              {id  :: String}    |
    TKelipsis                            |
    TKtoBeContinued                      |
    TKburst                              |
    TKreturn         

    deriving(Eq, Show)

instance Show Token where
    show = "oli"