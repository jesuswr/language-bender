-- Utility data types and functions 

module FrontEnd.Utils where

-- Token position in file
data Position = Position {row :: Int, col :: Int} deriving (Eq, Show)