
module FrontEnd.Errors where

import FrontEnd.Utils -- Position



data Error = 
	LexerError { pos :: Position, errorMessage :: String }
	deriving(Show, Eq)
