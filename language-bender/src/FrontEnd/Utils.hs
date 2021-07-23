-- Utility data types and functions 

module FrontEnd.Utils where

import Data.List(nub)
import Data.Char(isSpace)

-- Token position in file
data Position = Position {row :: Int, col :: Int} deriving (Eq)

-- Name of symbol for example
type Name = String

instance Show Position where
    show (Position r c) =
        " line " ++ show r ++ ", column " ++ show c ++ "."

-- | Function to check if a list contains duplicates
hasDuplicates :: (Eq a) => [a] -> Bool 
hasDuplicates l = nub l == l

-- | remove space characters from end and start of the string
trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace