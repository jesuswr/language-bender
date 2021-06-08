-- Utility data types and functions 

module FrontEnd.Utils where

-- Token position in file
data Position = Position {row :: Int, col :: Int} deriving (Eq)

instance Show Position where
	show (Position r c) =
		" line " ++ (show r) ++ ", column " ++ (show c) ++ "."