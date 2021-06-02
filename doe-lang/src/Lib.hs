module Lib
    ( langBender
    ) where


import           System.Environment
import           FrontEnd.CommandLine


langBender :: IO ()
langBender = do
    args <- getArgs
    case processArgs args of
        Right strError -> do
            putStrLn strError
        Left opts -> do
            -- seguir con el flujo
            print opts
