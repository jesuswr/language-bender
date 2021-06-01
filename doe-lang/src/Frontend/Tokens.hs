
module FrontEnd.Tokens (
	Token(..),
	) where


data Token = 
	TKbender {}      |
	TKof {}
	deriving(Eq)