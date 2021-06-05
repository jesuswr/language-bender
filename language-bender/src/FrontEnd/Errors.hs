-- File describing possible error and they're corresponding error message
module FrontEnd.Errors where

import FrontEnd.Utils -- Position

-- Lexical error
data LexerError = InvalidToken{ token :: String }
                | InvalidStrChar
                | UnexpectedEOF deriving(Eq)

-- Coming soon
{-
data ParsingError = ...
-}

-- Error Type: Possible errors we can report in the command line, so they should be properly 
-- formated in their show instance
data Error = 
    LexerError { pos :: Position, error :: LexerError }
    deriving(Eq)

instance Show Error where
    show (LexerError pos error) = "[Error]: lexical error near of " ++ show pos ++ ".\n\t" ++ show error

instance Show LexerError where
    show InvalidToken{token=tk} = "Invalid token: " ++ tk