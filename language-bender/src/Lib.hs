{-# OPTIONS_GHC -Wall #-}
module Lib
    (   langBender,
    ) where


import              System.Environment
import              FrontEnd.CommandLine
import              FrontEnd.Lexer
import qualified    FrontEnd.PreParser   as PP
import qualified    FrontEnd.Parser      as P
import qualified    FrontEnd.ParserCheck as PC
import qualified    FrontEnd.SymTable    as ST
import qualified    Control.Monad        as M
import qualified    BackEnd.TacGenerator as TG

-- Third party imports 
import qualified System.Console.ANSI     as C -- colors

langBender :: IO ()
langBender = do
    args <- getArgs
    procArgs <- processArgs args
    case procArgs of
        Left cliError -> do
            C.setSGR [C.SetColor C.Foreground C.Vivid C.Red]
            print cliError
            C.setSGR [C.Reset]
        Right (Result opts warnings) -> do
            M.unless (null warnings) $ do
                C.setSGR [C.SetColor C.Foreground C.Vivid C.Yellow]
                putStrLn "~ lbend CLI warnings ~"
                C.setSGR [C.Reset]
                mapM_ print warnings
                putStrLn "\n"

            if help opts then
                putStrLn helpMsg
            else do
                content <- readFile (fileName opts)

                let lexerResult = scanTokens content
                let lexerErrors = fst lexerResult
                let tokens = snd lexerResult


                let _printTokens1 = printLex opts || justLex opts
                let _printTokens2 = null lexerErrors || verbose opts
                let printTokens = _printTokens1 && _printTokens2

                M.when printTokens $ do
                    C.setSGR [C.SetColor C.Foreground C.Vivid C.Blue]
                    putStrLn "~ Tokens ~\n"
                    C.setSGR [C.Reset]
                    mapM_ print tokens
                    putStrLn "\n"

                M.unless (null lexerErrors) $ do
                    C.setSGR [C.SetColor C.Foreground C.Vivid C.Red]
                    putStrLn "~ Lexer Errors ~\n"
                    C.setSGR [C.Reset]
                    mapM_ print lexerErrors

                M.when (null tokens) $ do
                    putStrLn "No token was found.\nAn executable will not be generated\n"

                if justLex opts || null tokens then return ()
                else do
                    (preParseState, preParseErrors) <- PP.runPreParse tokens
                    let preSymT = PC.symTable preParseState

                    let preParseState' = preParseState{ PC.symTable = (PC.symTable preParseState){ST.stNextScope = 1} }
                    (ast, parseState, parseErrors) <- P.runParse tokens preParseState'

                    let symT = PC.symTable parseState
                        errors = preParseErrors ++ parseErrors
                        exist_errors = (not $ null errors) || (not $ null lexerErrors)

                        _printST1 = printPar opts || justPar opts
                        _printST2 = null errors || verbose opts
                        printST = _printST1 && _printST2

                    M.when printST $ do

                        C.setSGR [C.SetColor C.Foreground C.Vivid C.Blue]
                        putStrLn "~ AST ~"
                        C.setSGR [C.Reset]

                        print ast

                        C.setSGR [C.SetColor C.Foreground C.Vivid C.Blue]
                        putStrLn "~ Pre Symbol Table ~"
                        C.setSGR [C.Reset]

                        print preSymT

                        C.setSGR [C.SetColor C.Foreground C.Vivid C.Blue]
                        putStrLn "~ Symbol Table ~"
                        C.setSGR [C.Reset]

                        print symT
                        putStrLn "\n"

                    M.unless (null errors) $ do
                        C.setSGR [C.SetColor C.Foreground C.Vivid C.Red]
                        putStrLn "~ Parse Errors ~\n"
                        C.setSGR [C.Reset]

                        mapM_ print errors
                        putStrLn "\n"


                    if justPar opts || exist_errors then return ()
                    else do

                        (_, tac') <- TG.generateTac symT ast
                        --C.setSGR [C.SetColor C.Foreground C.Vivid C.Blue]
                        --putStrLn "~ TAC ~\n"
                        --C.setSGR [C.Reset]
                        print tac'
                        return ()




helpMsg :: String
helpMsg = "~ lbend ~ The Last Language Bender Compiler.\n"
    ++ "Usage: stack exec -- lbend <bend file> [options]\n"
    ++ "Options:\n"
    ++ "--help         show this help.\n"
    ++ "-v             print lexer and parser info even when errors occur.\n"
    ++ "-lex           print lexer output.\n"
    ++ "-par           print parser output.\n"
    ++ "-jlex          just use the Lexer in the input.\n"
    ++ "-jpar          just use the Lexer and the Parser in the input.\n"
    ++ "-o <file>      Place the output into <file>."