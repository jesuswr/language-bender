module Lib
    (   langBender,
    ) where


import              System.Environment
import              FrontEnd.CommandLine
import              FrontEnd.Lexer
import qualified    FrontEnd.PreParser   as PP
import qualified    FrontEnd.Parser      as P
import              FrontEnd.Errors
--import              FrontEnd.StaticAnalysis as SE
import qualified    FrontEnd.ParserCheck as PC
import qualified    FrontEnd.SymTable    as ST
import qualified    Utils.Constants
import qualified    Control.Monad        as M
import qualified    BackEnd.TacGenerator as TG

langBender :: IO ()
langBender = do
    args <- getArgs
    procArgs <- processArgs args
    case procArgs of
        Left cliError -> do
            print cliError
        Right (Result opts warnings) -> do
            M.unless (null warnings) $ do
                putStrLn "lbend CLI warnings:"
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
                    putStrLn "~ Tokens ~\n"
                    mapM_ print tokens
                    putStrLn "\n"

                M.unless (null lexerErrors) $ do
                    putStrLn "~ Lexer Errors ~\n"
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
                    let errors = preParseErrors ++ parseErrors

                    let _printST1 = printPar opts || justPar opts
                    let _printST2 = null errors || verbose opts
                    let printST = _printST1 && _printST2

                    M.when printST $ do
                        putStrLn "~ AST ~"
                        print ast
                        putStrLn "~ Pre Symbol Table ~"
                        print preSymT
                        putStrLn "~ Symbol Table ~"
                        print symT
                        putStrLn "\n"

                    M.unless (null errors) $ do
                        putStrLn "~ Parse Errors ~\n"
                        mapM_ print errors
                        putStrLn "\n"

                    (gs, tac) <- TG.generateTac symT ast
                    print tac
                    return ()




helpMsg :: String
helpMsg = "~ lbend ~ A language bender compiler.\n"
    ++ "Usage: stack exec -- lbend <bend file> [options]\n"
    ++ "Options:\n"
    ++ "--help         show this help.\n"
    ++ "-v             print lexer and parser info even when errors occur.\n"
    ++ "-lex           print lexer output.\n"
    ++ "-par           print parser output.\n"
    ++ "-jlex          just use the Lexer in the input.\n"
    ++ "-jpar          just use the Lexer and the Parser in the input.\n"
    ++ "-o <file>      Place the output into <file>."