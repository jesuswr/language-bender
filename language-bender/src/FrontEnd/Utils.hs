-- Utility data types and functions 

module FrontEnd.Utils where

-- Token position in file
data Position = Position {row :: Int, rol :: Int} deriving (Eq, Show)