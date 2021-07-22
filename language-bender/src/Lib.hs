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

langBender :: IO ()
langBender = do
    args <- getArgs
    procArgs <- processArgs args
    case procArgs of
        Left cliError -> do
            print cliError
        Right (Result opts warnings) -> do

            M.when (not $ null warnings) $ do
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

                M.unless (null lexerErrors) $ do
                    putStrLn "~ Lexer Errors ~\n"
                    mapM_ print lexerErrors
                    putStrLn "\n"

                M.when (printLex opts || justLex opts) $ do
                    putStrLn "~ Tokens ~\n"
                    mapM_ print tokens
                    putStrLn "\n"

                M.when (null tokens) $ do
                    putStrLn "No token was found.\nAn executable will not be generated\n"

                if justLex opts || null tokens then return ()
                    else do

                        putStrLn " antes del preparsing\n"

                        (preParseState, preParseErrors) <- PP.runPreParse tokens

                        let lol = PC.symTable preParseState
                        putStrLn "~ Pre Symbol Table ~"
                        print lol

                        putStrLn " entre preparsing y parsing\n"

                        -- 2) El parser debe ignorar redef. de funciones
                        -- 3) el preparser debe consultar redef. de funciones
                        -- 4) crear funcion update symbol

                        -- 5) adaptar type checking actual al nuevo

                        -- 6) cambiar mucho el parser (dirigido por sintaxis)

                        -- 7) Añadir arreglos a la tabla de simbolos

                        -- 9) añadir el tipo void para tipos que se construyen de otros tipos (ptr por ahora)

                        let preParseState' = preParseState{ PC.symTable = (PC.symTable preParseState){ST.stNextScope = 1} }
                        (parseState, parseErrors) <- P.runParse tokens preParseState'

                        putStrLn " despues del parsing\n"

                        let symT = PC.symTable parseState

                        let errors = preParseErrors ++ parseErrors

                        M.unless (null errors) $ do
                            putStrLn "~ Errors ~\n"
                            mapM_ print errors
                            putStrLn "\n"

                        putStrLn "~ Symbol Table ~"
                        print symT

                        --let ast = parseTokens tokens

                        -- M.when (printPar opts || justPar opts) $ do
                        --     putStrLn "~ Abstract Syntax Tree ~\n"
                        --     print ast

                        -- ((SE.State symT _), stErr) <- SE.analyzeProgram ast

                        -- M.unless (null stErr) $ do
                        --     putStrLn "~ Static Errors ~"
                        --     print stErr

                        -- if justPar opts 
                        --     then return ()
                        --     else do
                        --         putStrLn "~ Symbol Table ~"
                        --         print symT





helpMsg :: String
helpMsg = "~ lbend ~ A language bender compiler.\n"
    ++ "Usage: stack exec -- lbend <bend file> [options]\n"
    ++ "Options:\n"
    ++ "--help         show this help.\n"
    ++ "-lex           print lexer output.\n"
    ++ "-par           print parser output.\n"
    ++ "-jlex          just use the Lexer in the input.\n"
    ++ "-jpar          just use the Lexer and the Parser in the input.\n"
    ++ "-o <file>      Place the output into <file>."