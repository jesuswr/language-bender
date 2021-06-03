
module FrontEnd.Tokens (
	Token(..),
	) where

import FrontEnd.Utils -- Position

data Token = 
	TKbender          {pos :: Position}                    |
	TKof              {pos :: Position}                    |
    TKeternal         {pos :: Position}                    |

    TKis              {pos :: Position}                    |

    TKreincarnation   {pos :: Position}                    |

    TKart             {pos :: Position}                    |
    TKapprentice      {pos :: Position}                    |
    TKborn            {pos :: Position}                    |
    TKmember          {pos :: Position}                    |
    TKdied            {pos :: Position}                    |

    TKair             {pos :: Position}                    |
    TKwater           {pos :: Position}                    |
    TKfire            {pos :: Position}                    |
    TKlightning       {pos :: Position}                    |
    TKfireMaster      {pos :: Position}                    |
    TKearth           {pos :: Position}                    |
    TKmetal           {pos :: Position}                    |

    TKnation          {pos :: Position}                    |
    TKyear            {pos :: Position}                    |
    TKmasterOf        {pos :: Position}                    |
    TKdisciple        {pos :: Position}                    |
    
    TKelement         {pos :: Position}                    |
    TKmasteredby      {pos :: Position}                    |
    TKlearning        {pos :: Position}                    |
    TKcontrol         {pos :: Position}                    |

    TKenery           {pos :: Position}                    |
    TKallows          {pos :: Position}                    |
    TKtechniqueOf     {pos :: Position}                    |
    TKbending         {pos :: Position}                    |
    TKtechniquesFrom  {pos :: Position}                    |
    TKusing           {pos :: Position}                    |
    TKquotmark_s      {pos :: Position}                    |
    TKtechnique       {pos :: Position}                    |
    TKtrying          {pos :: Position}                    |

    TKbook            {pos :: Position}                    |
    TKabout           {pos :: Position}                    |
    TKtravel          {pos :: Position}                    |
    TKmadeBy          {pos :: Position}                    |

    TKandThen         {pos :: Position}                    |
    TKbut             {pos :: Position}                    |
    TKandThus         {pos :: Position}                    |
    TKbesides         {pos :: Position}                    |
    TKand             {pos :: Position}                    |
    TKor              {pos :: Position}                    |
    TKnot             {pos :: Position}                    |

    TKif              {pos :: Position}                    |
    TKotherwise       {pos :: Position}                    |

    TKint             {pos :: Position, value :: Int}      |
    TKfloat           {pos :: Position, value :: Float}    |

    TKcomma           {pos :: Position}                    |
    TKcolon           {pos :: Position}                    |
    TKbeginBlock      {pos :: Position}                    |
    TKendBlock        {pos :: Position}                    |
    TKdot             {pos :: Position}                    |
    TKunit            {pos :: Position}                    |
    TKopenParent      {pos :: Position}                    |
    TKcloseParent     {pos :: Position}                    |

    TKchar            {pos :: Position, value :: String}     |
    TKstring          {pos :: Position, value :: String}   

   	deriving(Eq, Show)


type TokenConstuct = (Int, Int) -> Token