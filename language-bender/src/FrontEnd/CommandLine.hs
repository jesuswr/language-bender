module FrontEnd.CommandLine where

import           System.Directory
import qualified FrontEnd.Errors as E

data Opts = Opts {
        fileName :: String,
        help     :: Bool,
        verbose  :: Bool,
        justLex  :: Bool,
        justPar  :: Bool,
        tac      :: Bool,
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
    | notElem "--help" args && not (foldl (\b arg -> validFileName arg || b) False args) =
        return $ Left (E.CliError E.NoFileName)
    | otherwise = do

        let opts = Opts {
                fileName = "",
                help     = False,
                verbose  = False,
                justLex  = False,
                justPar  = False,
                tac      = False,
                printLex = False,
                printPar = False,
                objFile  = ""
        }

        if "--help" `elem` args then
            return . Right $ Result opts{help=True} []
        else do
            let (newOpts, warnings) = processFlags args opts []

            let name = fileName newOpts

            fileExist <- doesFileExist name

            if not fileExist then
                return $ Left (E.CliError E.DoesNotExistFileName)
            else
                if null $ objFile newOpts then
                    return . Right $ Result newOpts{objFile = take (length name - 5) name} warnings
                else
                    return . Right $ Result newOpts warnings






processFlags :: [String] -> Opts -> [E.Warning] -> (Opts, [E.Warning])
processFlags [] opts warnings =
    (opts, warnings)
processFlags ("--help":xs) opts warnings =
    processFlags xs (opts{help = True}) warnings
processFlags ("-v":xs) opts warnings =
    processFlags xs (opts{verbose = True}) warnings
processFlags ("-lex":xs) opts warnings =
    processFlags xs (opts{printLex = True}) warnings
processFlags ("-par":xs) opts warnings =
    processFlags xs (opts{printPar = True}) warnings
processFlags ("-jlex":xs) opts warnings =
    processFlags xs (opts{justLex = True}) warnings
processFlags ("-jpar":xs) opts warnings =
    processFlags xs (opts{justPar = True}) warnings
processFlags ("-tac":xs) opts warnings =
    processFlags xs (opts{tac = True}) warnings
processFlags ("-o":xs) opts warnings
    | null xs =
        processFlags xs opts (noObjFileName:warnings)
    | not $ validObjFileName name =
        processFlags (tail xs) opts (unvalidFileName name:warnings)
    | otherwise =
        processFlags (tail xs) (opts{objFile = name}) warnings
    where
        name = head xs
        noObjFileName = E.CliWarning E.NoObjFileName
        unvalidFileName s = E.CliWarning (E.InvalidObjFileName s)
processFlags (x:xs) opts warnings =
    if validFileName x && null (fileName opts) then
        processFlags xs (opts{fileName = x}) warnings
    else
        processFlags xs opts (unknownArg x:warnings)
        where
            unknownArg s = E.CliWarning (E.UnknownArg s)


validObjFileName :: String -> Bool
validObjFileName name = not $ null name || head name == '-'


validFileName :: String -> Bool
validFileName ('-':_) = False
validFileName s = take 5 (reverse s) == "dneb." && length s > 5