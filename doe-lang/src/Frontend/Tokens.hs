
module FrontEnd.Tokens (
	Token(..),
	) where

data Position = Position{
        row :: Int,
        col :: Int   
    }
    deriving(Show, Eq)

data Token = 
	TKbender     {pos :: Position}                    |
	TKof         {pos :: Position}                    |
    TKeternal    {pos :: Position}                    |
    TKis         {pos :: Position}                    |
    TKart        {pos :: Position}                    |
    TKapprentice {pos :: Position}                    |
    TKborn       {pos :: Position}                    |
    TKmember     {pos :: Position}                    |
    TKdied       {pos :: Position}                    |
    TKair        {pos :: Position}                    |
    TKwater      {pos :: Position}                    |
    TKfire       {pos :: Position}                    |
    TKlightning  {pos :: Position}                    |
    TKfireMaster {pos :: Position}                    |
    TKearth      {pos :: Position}                    |
    TKmetal      {pos :: Position}                    |
    TKnation     {pos :: Position}                    |
    TKyear       {pos :: Position}                    |
    TKmasterOf   {pos :: Position}                    |
    TKdisciple   {pos :: Position}                    |
    TKelement    {pos :: Position}                    |
    TKmasteredby {pos :: Position}                    |
    TKlearning   {pos :: Position}                    |
    TKcontrol    {pos :: Position}                    |
    -- Poner las de union
    TKint        {pos :: Position, value :: Int}      |
    TKfloat      {pos :: Position, value :: Float}    |
    TKstring     {pos :: Position, value :: String}   |
    TKid         {pos :: Position, value :: String}   |
    


   	deriving(Eq)