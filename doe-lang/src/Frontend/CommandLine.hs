module Frontend.CommandLine where

data Opts = Opts {
        fileName :: String,
        help     :: Bool,
        printLex :: Bool,
        printPar :: Bool,
        objFile  :: String
    }
    deriving(Show)
    


processArgs :: [String] -> Either (Opts, [String]) String
processArgs args = do
    if null args then
        Right "ERROR: No arguments given"
    else do
        let opts = Opts {
                fileName = head args,
                help     = False,
                printLex = False,
                printPar = False,
                objFile  = ""
            }
        let (newOpts, warnings) = processFlags (tail args) opts []
        if null $ objFile newOpts then
            Left $ (newOpts{objFile = fileName opts}, warnings)
        else
            Left (newOpts, warnings)



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
    | not $ validObjFile name =
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


validObjFile :: String -> Bool
validObjFile name = True