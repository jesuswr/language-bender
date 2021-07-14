-- Utility data types and functions 

module FrontEnd.Utils where

import Data.List(nub)

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