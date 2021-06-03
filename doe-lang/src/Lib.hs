module Lib
    ( langBender
    ) where


import           System.Environment
import           FrontEnd.CommandLine
import           FrontEnd.Lexer


langBender :: IO ()
langBender = do
    args <- getArgs
    procArgs <- processArgs args
    case procArgs of
        Right strError -> do
            putStrLn strError
        Left (opts, warnings) -> do
            -- seguir con el flujo
            --print opts
            content <- readFile (fileName opts)
            --print content
            let tokens = scanTokens content
            print tokens

