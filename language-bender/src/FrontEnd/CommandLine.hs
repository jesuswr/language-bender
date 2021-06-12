module FrontEnd.CommandLine where

import           System.Directory
import qualified FrontEnd.Errors as E

data Opts = Opts {
        fileName :: String,
        help     :: Bool,
        justLex  :: Bool,
        justPar  :: Bool,
        printLex :: Bool,
        printPar :: Bool,
        objFile  :: String
    }
    deriving(Show)

data Result = Result Opts [E.Warning]

-- Process incoming arguments as strings and return result if everything ok, 
-- or an error message otherwise
processArgs :: [String] -> IO (Either E.Error Result)
processArgs args
    | null args =
        return $ Left (E.CliError E.NoArgs)
    | not $ validFileName (head args) =
        return $ Left (E.CliError E.InvalidFileName)
    | otherwise = do
        fileExist <- doesFileExist name
        if not fileExist then
            return $ Left (E.CliError E.DoesNotExistFileName)
        else do
            let opts = Opts {
                    fileName = name,
                    help     = False,
                    justLex  = False,
                    justPar  = False,
                    printLex = False,
                    printPar = False,
                    objFile  = ""
                }
            let (newOpts, warnings) = processFlags (tail args) opts []
            if null $ objFile newOpts then
                return . Right $ Result newOpts{objFile = fileName opts} warnings
            else
                return . Right $ Result newOpts warnings
        where
            name = head args



processFlags :: [String] -> Opts -> [E.Warning] -> (Opts, [E.Warning])
processFlags [] opts warnings =
    (opts, warnings)
processFlags ("--help":xs) opts warnings =
    processFlags xs (opts{help = True}) warnings
processFlags ("-lex":xs) opts warnings =
    processFlags xs (opts{printLex = True}) warnings
processFlags ("-par":xs) opts warnings =
    processFlags xs (opts{printPar = True}) warnings
processFlags ("-jlex":xs) opts warnings =
    processFlags xs (opts{justLex = True}) warnings
processFlags ("-jpar":xs) opts warnings =
    processFlags xs (opts{justPar = True}) warnings
processFlags ("-o":xs) opts warnings
    | null xs =
        processFlags xs opts (noObjFileName:warnings)
    | not $ validObjFileName name =
        processFlags (tail xs) opts (unvalidFileName name:warnings)
    | otherwise =
        processFlags (tail xs) (opts{objFile = name}) warnings
    where
        name = head xs
        noObjFileName = (E.CliWarning E.NoObjFileName)
        unvalidFileName s = (E.CliWarning (E.InvalidObjFileName s))
processFlags (x:xs) opts warnings =
    processFlags xs opts (unknownArg x:warnings)
    where
        unknownArg s = (E.CliWarning (E.UnknownArg s))


validObjFileName :: String -> Bool
validObjFileName name = not $ null name || head name == '-'


validFileName :: String -> Bool
validFileName ('-':_) = False
validFileName s = take 5 (reverse s) == "dneb." && length s > 5