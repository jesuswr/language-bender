-- File describing possible error and their corresponding error message
module FrontEnd.Errors where
import FrontEnd.Utils -- Position
import FrontEnd.StaticErrors


---- < Errors > -----

-- Lexical error
data LexerError = InvalidToken   { token :: String }
                | InvalidStrChar { char :: String }
                | UnexpectedEOF 
                deriving(Eq)

-- CLI error
data CliError = NoArgs
              | NoFileName
              | DoesNotExistFileName
              deriving (Eq)

-- Coming soon
{-
data ParsingError = ...
-}


-- Error Type: Possible errors we can report in the command line, so they should be properly 
-- formated in their show instance
data Error = 
    LexerError { pos :: Position, lexError :: LexerError }    |
    CliError {cliError :: CliError}                           |
    StaticError {staticError :: StaticError}
    deriving(Eq)

instance Show Error where
    
    show (LexerError pos lexError) = "[Error]: lexical error near of " ++ show pos ++ ".\n\t" ++ show lexError

    show (CliError cliError) = "[Error]: " ++ show cliError

instance Show LexerError where
    
    show InvalidToken{token=tk} = "Invalid token: " ++ tk

    show InvalidStrChar{char=c} = "Invalid character '" ++ c ++ "' found in string."

    show FrontEnd.Errors.UnexpectedEOF = "Unexpected EOF: string is not closed."

instance Show CliError where
    
    show NoArgs = "No arguments given\nUsage: For basic information, try the `--help' option."
    
    show NoFileName = "No file name was given."
    
    show DoesNotExistFileName = "The given file name doesn't exist." 

---- < Warnings > -----

data Warning = 
    CliWarning {warning :: CliWarning}
    deriving (Eq)

data CliWarning = NoObjFileName
                | InvalidObjFileName { objFileName :: String }
                | UnknownArg { unkArg :: String }
                deriving(Eq)


instance Show Warning where
    show (CliWarning warning) = "[Warning]: " ++ show warning


instance Show CliWarning where
    show NoObjFileName = "No name was given for -o flag." 

    show InvalidObjFileName{objFileName=s} = "Invalid object file name: " ++ s ++ "."

    show UnknownArg{unkArg=s} = "Unknown argument: " ++ s ++ "."