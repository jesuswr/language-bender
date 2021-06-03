module FrontEnd.CommandLine where

import System.Directory

data Opts = Opts {
        fileName :: String,
        help     :: Bool,
        printLex :: Bool,
        printPar :: Bool,
        objFile  :: String
    }
    deriving(Show)


processArgs :: [String] -> IO(Either (Opts, [String]) String)
processArgs args 
    | null args =
        return $ Right "ERROR: No arguments given"
    | not $ validFileName (head args) =
        return $ Right "ERROR: The given file name its not valid"
    | otherwise = do
        fileExist <- doesFileExist name
        if not fileExist then
            return $ Right "ERROR: The given file name doesnt exist"
        else do
            let opts = Opts {
                    fileName = name,
                    help     = False,
                    printLex = False,
                    printPar = False,
                    objFile  = ""
                }
            let (newOpts, warnings) = processFlags (tail args) opts []
            if null $ objFile newOpts then
                return $ Left $ (newOpts{objFile = fileName opts}, warnings)
            else
                return $ Left (newOpts, warnings)
        where
            name = head args



processFlags :: [String] -> Opts -> [String] -> (Opts, [String])
processFlags [] opts warnings = 
    (opts, warnings)
processFlags ("--help":xs) opts warnings = 
    processFlags xs (opts{help = True}) warnings
processFlags ("-lex":xs) opts warnings = 
    processFlags xs (opts{printLex = True}) warnings
processFlags ("-par":xs) opts warnings =
    processFlags xs (opts{printPar = True}) warnings
processFlags ("-o":xs) opts warnings 
    | null xs =
        processFlags xs opts (noObjFileName:warnings)
    | not $ validObjFileName name =
        processFlags (tail xs) opts (unvalidFileName name:warnings)
    | otherwise =
        processFlags (tail xs) (opts{objFile = name}) warnings
    where
        name = head xs
        noObjFileName = "WARNING: No name was given for -o flag"
        unvalidFileName s = "WARNING: Unvalid object file name: " ++ s
processFlags (x:xs) opts warnings =
    processFlags xs opts (unknownArg x:warnings)
    where
        unknownArg s = "WARNING: Unknown argument: " ++ s


validObjFileName :: String -> Bool
validObjFileName name = not $ null name || head name == '-'


validFileName :: String -> Bool
validFileName ('-':_) = False
validFileName s = (take 5 $ reverse s) == "dneb." && length s > 5