module Lib
    (   langBender,
    ) where


import              System.Environment
import              FrontEnd.CommandLine
import              FrontEnd.Lexer
import              FrontEnd.Parser
import qualified    Utils.Constants
import qualified    Control.Monad as CM

langBender :: IO ()
langBender = do
    args <- getArgs
    procArgs <- processArgs args
    case procArgs of
        Left strError -> do
            putStrLn strError
        Right (Result opts warnings) -> do

            CM.unless (null warnings) $ print warnings -- poner bonito

            CM.when (help opts) $ print "help" -- poner bonito

            content <- readFile (fileName opts)

            let tokens = scanTokens content

            CM.when (printLex opts || justLex opts) $ 
                print tokens -- poner bonito

            if justLex opts then return ()
                else do

                    let ast = parseTokens . snd $ tokens

                    CM.when (printPar opts || justPar opts) $ 
                        print ast -- poner bonito

                    if justPar opts then return ()
                        else do
                            putStrLn "not implemented"
                            return ()




