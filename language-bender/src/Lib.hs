module Lib
    (   langBender,
    ) where


import              System.Environment
import              FrontEnd.CommandLine
import              FrontEnd.Lexer
import              FrontEnd.Parser
import qualified    Utils.Constants

langBender :: IO ()
langBender = do
    args <- getArgs
    procArgs <- processArgs args
    case procArgs of
        Left strError -> do
            putStrLn strError
        Right (Result opts warnings) -> do
            -- seguir con el flujo
            --print opts
            content <- readFile (fileName opts)
            --print content
            let tokens = scanTokens content
            --print tokens
            print . parseTokens . snd $ tokens

