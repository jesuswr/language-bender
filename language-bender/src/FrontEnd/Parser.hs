{-# OPTIONS_GHC -w #-}
module FrontEnd.Parser where

import qualified FrontEnd.Tokens as TK
import qualified FrontEnd.AST    as AST
import qualified FrontEnd.Errors  as E
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.11

data HappyAbsSyn 
	= HappyTerminal (TK.Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (AST.Program)
	| HappyAbsSyn5 ([AST.Declaration])
	| HappyAbsSyn6 (AST.Declaration)
	| HappyAbsSyn9 ([AST.FuncArg])
	| HappyAbsSyn12 ([(String, AST.Type)])
	| HappyAbsSyn15 (AST.Expr)
	| HappyAbsSyn21 ([AST.Expr])
	| HappyAbsSyn29 (AST.Type)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (TK.Token)
	-> HappyState (TK.Token) (HappyStk HappyAbsSyn -> [(TK.Token)] -> m HappyAbsSyn)
	-> [HappyState (TK.Token) (HappyStk HappyAbsSyn -> [(TK.Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(TK.Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (TK.Token)
	-> HappyState (TK.Token) (HappyStk HappyAbsSyn -> [(TK.Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (TK.Token) (HappyStk HappyAbsSyn -> [(TK.Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(TK.Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (TK.Token)
	-> HappyState (TK.Token) (HappyStk HappyAbsSyn -> [(TK.Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (TK.Token) (HappyStk HappyAbsSyn -> [(TK.Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(TK.Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,508) ([0,40960,0,264,10,0,0,0,10240,0,32834,2,0,0,0,2560,32768,40976,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,8192,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,256,0,0,0,0,0,0,64,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,256,16,0,0,0,1,0,16,4,0,0,0,0,512,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,256,0,1280,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14336,1,0,0,32768,1,0,12296,33028,32772,520,32650,0,16384,1,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,256,0,0,78,0,0,0,96,0,32768,19,0,0,0,24,0,40,16899,712,9352,63648,7,0,14336,1,0,0,32768,1,32768,12290,33824,32812,584,32650,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,16384,0,0,0,0,0,32,0,0,10240,768,51266,34818,40999,2040,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,12,288,33312,57984,31,0,0,3,72,8328,63648,7,0,49152,0,18,2082,65064,1,0,12288,32768,32772,520,32650,0,0,3072,8192,8193,32898,8162,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,16,0,0,0,0,0,0,0,16,0,0,0,0,0,0,2,0,0,0,0,0,32768,0,0,0,2048,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,16384,0,16384,4096,0,0,0,0,0,0,512,0,0,0,0,0,4,0,0,0,0,0,0,0,32,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,4992,0,0,0,6144,0,0,17152,18448,34816,40992,2040,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,49152,0,18,2082,65064,1,0,19968,0,0,0,24576,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,258,0,0,0,0,0,0,0,0,0,0,0,0,128,0,32,0,0,0,0,0,0,0,0,0,0,20,0,0,0,0,0,0,4992,0,0,0,6144,0,0,0,0,0,0,1024,0,0,312,0,0,0,384,0,0,0,0,0,0,64,0,32768,19,0,0,0,24,0,40,16899,712,9352,63648,7,0,49162,4224,178,2338,65064,1,0,0,0,0,0,16384,0,0,4992,0,0,0,6144,0,0,1248,0,0,0,1536,0,2560,32960,45584,8704,10249,510,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,16,0,0,0,0,0,8192,0,0,0,0,0,0,1024,0,0,0,40,16899,712,9352,63648,7,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,192,4608,8704,10248,510,0,0,0,256,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,1,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,3,72,8328,64672,7,0,0,0,0,16,0,0,0,0,0,0,0,16384,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,16896,0,0,0,0,0,0,0,0,0,512,0,0,0,1056,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,256,0,0,0,0,16384,0,128,0,0,0,0,0,32768,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12288,32768,32772,520,32650,0,0,0,8192,0,0,0,0,0,0,0,0,0,1024,0,0,0,256,0,0,0,0,0,48,1152,2176,35330,127,0,0,0,0,0,0,0,0,57344,4,0,0,0,6,0,0,0,0,0,0,0,0,19968,0,0,0,24576,0,40960,3072,8456,8203,32914,8162,0,0,768,18432,34816,40992,2040,0,0,0,0,0,0,256,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,49152,0,18,2082,65064,1,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,8192,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,256,0,64,0,0,0,0,0,0,0,512,0,0,0,49152,0,18,2082,65064,1,0,0,0,2,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,3,72,8328,63648,7,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,768,18432,34816,40992,2040,0,4096,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,32768,0,0,0,0,0,16,0,0,0,0,0,0,0,2,32768,0,0,0,0,768,18432,34816,40992,2040,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,72,8328,63648,7,0,0,0,0,0,128,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,192,4608,8704,10248,510,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,72,8328,63648,7,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,3072,8192,8193,32898,8162,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseTokens","Program","Declarations","Declaration","ProcDecl","FuncDecl","FuncArg","FuncDefArgDecl","FuncArgDecl","StructIdDecls","UnionIdDecls","VarDecl","Expr","Exprs","Assign","AssignStruct","AssignUnion","ExprBlock","ExprSeq","Seq","LastInBlock","Dots","FunCallArg","FunDefArgCall","SimpleFuncArg","ExprList","Type","bender","of","eternal","is","reincarnationOf","art","apprentice","born","member","died","air","water","fire","lightning","fireMaster","earth","metal","nation","year","masterOf","rightNow","disciple","element","compoundBy","skillOf","learning","control","energy","allows","techniqueOf","bending","techniqueFrom","using","quotmark_s","technique","trying","book","about","travel","madeBy","andThen","but","andThus","besides","left","and","or","not","if","otherwise","comma","colon","beginBlock","endBlock","dot","unit","'('","')'","in","bookWith","with","lessThan","lessEqThan","greaterThan","greaterEqThan","equal","while","doing","opening","chakrasFrom","to","elipsis","toBeContinued","burst","return","int","float","char","string","id","%eof"]
        bit_start = st * 110
        bit_end = (st + 1) * 110
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..109]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (30) = happyShift action_7
action_0 (32) = happyShift action_8
action_0 (52) = happyShift action_9
action_0 (57) = happyShift action_10
action_0 (66) = happyShift action_11
action_0 (68) = happyShift action_12
action_0 (4) = happyGoto action_13
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (14) = happyGoto action_6
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (30) = happyShift action_7
action_1 (32) = happyShift action_8
action_1 (52) = happyShift action_9
action_1 (57) = happyShift action_10
action_1 (66) = happyShift action_11
action_1 (68) = happyShift action_12
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 (14) = happyGoto action_6
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (30) = happyShift action_7
action_2 (32) = happyShift action_8
action_2 (52) = happyShift action_9
action_2 (57) = happyShift action_10
action_2 (66) = happyShift action_11
action_2 (68) = happyShift action_12
action_2 (6) = happyGoto action_20
action_2 (7) = happyGoto action_4
action_2 (8) = happyGoto action_5
action_2 (14) = happyGoto action_6
action_2 _ = happyReduce_1

action_3 _ = happyReduce_2

action_4 _ = happyReduce_8

action_5 _ = happyReduce_7

action_6 _ = happyReduce_6

action_7 (109) = happyShift action_19
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (30) = happyShift action_18
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (109) = happyShift action_17
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (109) = happyShift action_16
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (109) = happyShift action_15
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (109) = happyShift action_14
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (110) = happyAccept
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (69) = happyShift action_30
action_14 (81) = happyShift action_31
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (31) = happyShift action_27
action_15 (67) = happyShift action_28
action_15 (81) = happyShift action_29
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (58) = happyShift action_26
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (53) = happyShift action_25
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (109) = happyShift action_24
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (31) = happyShift action_22
action_19 (33) = happyShift action_23
action_19 (17) = happyGoto action_21
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_3

action_21 _ = happyReduce_28

action_22 (40) = happyShift action_58
action_22 (41) = happyShift action_59
action_22 (42) = happyShift action_60
action_22 (45) = happyShift action_61
action_22 (108) = happyShift action_62
action_22 (109) = happyShift action_63
action_22 (29) = happyGoto action_80
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (34) = happyShift action_77
action_23 (43) = happyShift action_36
action_23 (44) = happyShift action_37
action_23 (49) = happyShift action_78
action_23 (55) = happyShift action_79
action_23 (62) = happyShift action_38
action_23 (65) = happyShift action_39
action_23 (78) = happyShift action_40
action_23 (82) = happyShift action_41
action_23 (88) = happyShift action_43
action_23 (96) = happyShift action_44
action_23 (98) = happyShift action_45
action_23 (102) = happyShift action_46
action_23 (103) = happyShift action_47
action_23 (104) = happyShift action_48
action_23 (105) = happyShift action_49
action_23 (106) = happyShift action_50
action_23 (107) = happyShift action_51
action_23 (108) = happyShift action_52
action_23 (109) = happyShift action_53
action_23 (15) = happyGoto action_74
action_23 (18) = happyGoto action_75
action_23 (19) = happyGoto action_76
action_23 (20) = happyGoto action_35
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (31) = happyShift action_72
action_24 (33) = happyShift action_73
action_24 (17) = happyGoto action_71
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (109) = happyShift action_70
action_25 (12) = happyGoto action_69
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (109) = happyShift action_68
action_26 (13) = happyGoto action_67
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (40) = happyShift action_58
action_27 (41) = happyShift action_59
action_27 (42) = happyShift action_60
action_27 (45) = happyShift action_61
action_27 (108) = happyShift action_62
action_27 (109) = happyShift action_63
action_27 (29) = happyGoto action_66
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (40) = happyShift action_58
action_28 (41) = happyShift action_59
action_28 (42) = happyShift action_60
action_28 (45) = happyShift action_61
action_28 (108) = happyShift action_62
action_28 (109) = happyShift action_63
action_28 (9) = happyGoto action_65
action_28 (10) = happyGoto action_55
action_28 (11) = happyGoto action_56
action_28 (29) = happyGoto action_57
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (30) = happyShift action_7
action_29 (32) = happyShift action_8
action_29 (43) = happyShift action_36
action_29 (44) = happyShift action_37
action_29 (52) = happyShift action_9
action_29 (57) = happyShift action_10
action_29 (62) = happyShift action_38
action_29 (65) = happyShift action_39
action_29 (66) = happyShift action_11
action_29 (68) = happyShift action_12
action_29 (78) = happyShift action_40
action_29 (82) = happyShift action_41
action_29 (85) = happyShift action_42
action_29 (88) = happyShift action_43
action_29 (96) = happyShift action_44
action_29 (98) = happyShift action_45
action_29 (102) = happyShift action_46
action_29 (103) = happyShift action_47
action_29 (104) = happyShift action_48
action_29 (105) = happyShift action_49
action_29 (106) = happyShift action_50
action_29 (107) = happyShift action_51
action_29 (108) = happyShift action_52
action_29 (109) = happyShift action_53
action_29 (6) = happyGoto action_32
action_29 (7) = happyGoto action_4
action_29 (8) = happyGoto action_5
action_29 (14) = happyGoto action_6
action_29 (15) = happyGoto action_33
action_29 (16) = happyGoto action_64
action_29 (20) = happyGoto action_35
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (40) = happyShift action_58
action_30 (41) = happyShift action_59
action_30 (42) = happyShift action_60
action_30 (45) = happyShift action_61
action_30 (108) = happyShift action_62
action_30 (109) = happyShift action_63
action_30 (9) = happyGoto action_54
action_30 (10) = happyGoto action_55
action_30 (11) = happyGoto action_56
action_30 (29) = happyGoto action_57
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (30) = happyShift action_7
action_31 (32) = happyShift action_8
action_31 (43) = happyShift action_36
action_31 (44) = happyShift action_37
action_31 (52) = happyShift action_9
action_31 (57) = happyShift action_10
action_31 (62) = happyShift action_38
action_31 (65) = happyShift action_39
action_31 (66) = happyShift action_11
action_31 (68) = happyShift action_12
action_31 (78) = happyShift action_40
action_31 (82) = happyShift action_41
action_31 (85) = happyShift action_42
action_31 (88) = happyShift action_43
action_31 (96) = happyShift action_44
action_31 (98) = happyShift action_45
action_31 (102) = happyShift action_46
action_31 (103) = happyShift action_47
action_31 (104) = happyShift action_48
action_31 (105) = happyShift action_49
action_31 (106) = happyShift action_50
action_31 (107) = happyShift action_51
action_31 (108) = happyShift action_52
action_31 (109) = happyShift action_53
action_31 (6) = happyGoto action_32
action_31 (7) = happyGoto action_4
action_31 (8) = happyGoto action_5
action_31 (14) = happyGoto action_6
action_31 (15) = happyGoto action_33
action_31 (16) = happyGoto action_34
action_31 (20) = happyGoto action_35
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_58

action_33 _ = happyReduce_56

action_34 _ = happyReduce_10

action_35 _ = happyReduce_45

action_36 _ = happyReduce_36

action_37 _ = happyReduce_37

action_38 (109) = happyShift action_116
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (109) = happyShift action_115
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (86) = happyShift action_114
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (30) = happyShift action_7
action_41 (32) = happyShift action_8
action_41 (43) = happyShift action_36
action_41 (44) = happyShift action_37
action_41 (52) = happyShift action_9
action_41 (57) = happyShift action_10
action_41 (62) = happyShift action_38
action_41 (65) = happyShift action_39
action_41 (66) = happyShift action_11
action_41 (68) = happyShift action_12
action_41 (78) = happyShift action_40
action_41 (82) = happyShift action_41
action_41 (83) = happyShift action_112
action_41 (84) = happyShift action_113
action_41 (85) = happyShift action_42
action_41 (88) = happyShift action_43
action_41 (96) = happyShift action_44
action_41 (98) = happyShift action_45
action_41 (102) = happyShift action_46
action_41 (103) = happyShift action_47
action_41 (104) = happyShift action_48
action_41 (105) = happyShift action_49
action_41 (106) = happyShift action_50
action_41 (107) = happyShift action_51
action_41 (108) = happyShift action_52
action_41 (109) = happyShift action_53
action_41 (6) = happyGoto action_32
action_41 (7) = happyGoto action_4
action_41 (8) = happyGoto action_5
action_41 (14) = happyGoto action_6
action_41 (15) = happyGoto action_33
action_41 (16) = happyGoto action_107
action_41 (20) = happyGoto action_35
action_41 (21) = happyGoto action_108
action_41 (22) = happyGoto action_109
action_41 (23) = happyGoto action_110
action_41 (24) = happyGoto action_111
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_57

action_43 (109) = happyShift action_106
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (43) = happyShift action_36
action_44 (44) = happyShift action_37
action_44 (62) = happyShift action_38
action_44 (65) = happyShift action_39
action_44 (78) = happyShift action_40
action_44 (82) = happyShift action_41
action_44 (88) = happyShift action_43
action_44 (96) = happyShift action_44
action_44 (98) = happyShift action_45
action_44 (102) = happyShift action_46
action_44 (103) = happyShift action_47
action_44 (104) = happyShift action_48
action_44 (105) = happyShift action_49
action_44 (106) = happyShift action_50
action_44 (107) = happyShift action_51
action_44 (108) = happyShift action_52
action_44 (109) = happyShift action_53
action_44 (15) = happyGoto action_105
action_44 (20) = happyGoto action_35
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (43) = happyShift action_36
action_45 (44) = happyShift action_37
action_45 (62) = happyShift action_38
action_45 (65) = happyShift action_39
action_45 (78) = happyShift action_40
action_45 (82) = happyShift action_41
action_45 (88) = happyShift action_43
action_45 (96) = happyShift action_44
action_45 (98) = happyShift action_45
action_45 (102) = happyShift action_46
action_45 (103) = happyShift action_47
action_45 (104) = happyShift action_48
action_45 (105) = happyShift action_49
action_45 (106) = happyShift action_50
action_45 (107) = happyShift action_51
action_45 (108) = happyShift action_52
action_45 (109) = happyShift action_53
action_45 (15) = happyGoto action_104
action_45 (20) = happyGoto action_35
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (43) = happyShift action_36
action_46 (44) = happyShift action_37
action_46 (62) = happyShift action_38
action_46 (65) = happyShift action_39
action_46 (78) = happyShift action_40
action_46 (82) = happyShift action_41
action_46 (88) = happyShift action_43
action_46 (96) = happyShift action_44
action_46 (98) = happyShift action_45
action_46 (102) = happyShift action_46
action_46 (103) = happyShift action_47
action_46 (104) = happyShift action_48
action_46 (105) = happyShift action_49
action_46 (106) = happyShift action_50
action_46 (107) = happyShift action_51
action_46 (108) = happyShift action_52
action_46 (109) = happyShift action_53
action_46 (15) = happyGoto action_103
action_46 (20) = happyGoto action_35
action_46 _ = happyReduce_42

action_47 (43) = happyShift action_36
action_47 (44) = happyShift action_37
action_47 (62) = happyShift action_38
action_47 (65) = happyShift action_39
action_47 (78) = happyShift action_40
action_47 (82) = happyShift action_41
action_47 (88) = happyShift action_43
action_47 (96) = happyShift action_44
action_47 (98) = happyShift action_45
action_47 (102) = happyShift action_46
action_47 (103) = happyShift action_47
action_47 (104) = happyShift action_48
action_47 (105) = happyShift action_49
action_47 (106) = happyShift action_50
action_47 (107) = happyShift action_51
action_47 (108) = happyShift action_52
action_47 (109) = happyShift action_53
action_47 (15) = happyGoto action_102
action_47 (20) = happyGoto action_35
action_47 _ = happyReduce_43

action_48 (43) = happyShift action_36
action_48 (44) = happyShift action_37
action_48 (62) = happyShift action_38
action_48 (65) = happyShift action_39
action_48 (78) = happyShift action_40
action_48 (82) = happyShift action_41
action_48 (88) = happyShift action_43
action_48 (96) = happyShift action_44
action_48 (98) = happyShift action_45
action_48 (102) = happyShift action_46
action_48 (103) = happyShift action_47
action_48 (104) = happyShift action_48
action_48 (105) = happyShift action_49
action_48 (106) = happyShift action_50
action_48 (107) = happyShift action_51
action_48 (108) = happyShift action_52
action_48 (109) = happyShift action_53
action_48 (15) = happyGoto action_101
action_48 (20) = happyGoto action_35
action_48 _ = happyReduce_44

action_49 _ = happyReduce_32

action_50 _ = happyReduce_33

action_51 _ = happyReduce_34

action_52 _ = happyReduce_35

action_53 (33) = happyShift action_73
action_53 (63) = happyShift action_100
action_53 (17) = happyGoto action_99
action_53 _ = happyReduce_38

action_54 (81) = happyShift action_98
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (80) = happyShift action_97
action_55 _ = happyReduce_15

action_56 (80) = happyShift action_96
action_56 _ = happyReduce_16

action_57 (30) = happyShift action_95
action_57 (35) = happyShift action_82
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_86

action_59 _ = happyReduce_85

action_60 _ = happyReduce_89

action_61 _ = happyReduce_87

action_62 _ = happyReduce_88

action_63 _ = happyReduce_90

action_64 _ = happyReduce_14

action_65 (81) = happyShift action_94
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (35) = happyShift action_82
action_66 (67) = happyShift action_92
action_66 (81) = happyShift action_93
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (80) = happyShift action_91
action_67 _ = happyReduce_5

action_68 (59) = happyShift action_90
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (80) = happyShift action_89
action_69 _ = happyReduce_4

action_70 (54) = happyShift action_88
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_30

action_72 (40) = happyShift action_58
action_72 (41) = happyShift action_59
action_72 (42) = happyShift action_60
action_72 (45) = happyShift action_61
action_72 (108) = happyShift action_62
action_72 (109) = happyShift action_63
action_72 (29) = happyGoto action_87
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (43) = happyShift action_36
action_73 (44) = happyShift action_37
action_73 (49) = happyShift action_78
action_73 (55) = happyShift action_79
action_73 (62) = happyShift action_38
action_73 (65) = happyShift action_39
action_73 (78) = happyShift action_40
action_73 (82) = happyShift action_41
action_73 (88) = happyShift action_43
action_73 (96) = happyShift action_44
action_73 (98) = happyShift action_45
action_73 (102) = happyShift action_46
action_73 (103) = happyShift action_47
action_73 (104) = happyShift action_48
action_73 (105) = happyShift action_49
action_73 (106) = happyShift action_50
action_73 (107) = happyShift action_51
action_73 (108) = happyShift action_52
action_73 (109) = happyShift action_53
action_73 (15) = happyGoto action_74
action_73 (18) = happyGoto action_75
action_73 (19) = happyGoto action_76
action_73 (20) = happyGoto action_35
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_59

action_75 _ = happyReduce_61

action_76 _ = happyReduce_62

action_77 (109) = happyShift action_86
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (43) = happyShift action_36
action_78 (44) = happyShift action_37
action_78 (62) = happyShift action_38
action_78 (65) = happyShift action_39
action_78 (78) = happyShift action_40
action_78 (82) = happyShift action_41
action_78 (88) = happyShift action_43
action_78 (96) = happyShift action_44
action_78 (98) = happyShift action_45
action_78 (102) = happyShift action_46
action_78 (103) = happyShift action_47
action_78 (104) = happyShift action_48
action_78 (105) = happyShift action_49
action_78 (106) = happyShift action_50
action_78 (107) = happyShift action_51
action_78 (108) = happyShift action_52
action_78 (109) = happyShift action_53
action_78 (15) = happyGoto action_84
action_78 (20) = happyGoto action_35
action_78 (28) = happyGoto action_85
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (40) = happyShift action_58
action_79 (41) = happyShift action_59
action_79 (42) = happyShift action_60
action_79 (45) = happyShift action_61
action_79 (108) = happyShift action_62
action_79 (109) = happyShift action_63
action_79 (29) = happyGoto action_83
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (33) = happyShift action_73
action_80 (35) = happyShift action_82
action_80 (17) = happyGoto action_81
action_80 _ = happyReduce_26

action_81 _ = happyReduce_27

action_82 _ = happyReduce_91

action_83 (35) = happyShift action_82
action_83 (56) = happyShift action_144
action_83 (63) = happyShift action_145
action_83 _ = happyFail (happyExpListPerState 83)

action_84 _ = happyReduce_83

action_85 (50) = happyShift action_142
action_85 (80) = happyShift action_143
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_31

action_87 (33) = happyShift action_73
action_87 (35) = happyShift action_82
action_87 (17) = happyGoto action_141
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (40) = happyShift action_58
action_88 (41) = happyShift action_59
action_88 (42) = happyShift action_60
action_88 (45) = happyShift action_61
action_88 (108) = happyShift action_62
action_88 (109) = happyShift action_63
action_88 (29) = happyGoto action_140
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (109) = happyShift action_139
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (40) = happyShift action_58
action_90 (41) = happyShift action_59
action_90 (42) = happyShift action_60
action_90 (45) = happyShift action_61
action_90 (108) = happyShift action_62
action_90 (109) = happyShift action_63
action_90 (29) = happyGoto action_138
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (109) = happyShift action_137
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (40) = happyShift action_58
action_92 (41) = happyShift action_59
action_92 (42) = happyShift action_60
action_92 (45) = happyShift action_61
action_92 (108) = happyShift action_62
action_92 (109) = happyShift action_63
action_92 (9) = happyGoto action_136
action_92 (10) = happyGoto action_55
action_92 (11) = happyGoto action_56
action_92 (29) = happyGoto action_57
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (30) = happyShift action_7
action_93 (32) = happyShift action_8
action_93 (43) = happyShift action_36
action_93 (44) = happyShift action_37
action_93 (52) = happyShift action_9
action_93 (57) = happyShift action_10
action_93 (62) = happyShift action_38
action_93 (65) = happyShift action_39
action_93 (66) = happyShift action_11
action_93 (68) = happyShift action_12
action_93 (78) = happyShift action_40
action_93 (82) = happyShift action_41
action_93 (85) = happyShift action_42
action_93 (88) = happyShift action_43
action_93 (96) = happyShift action_44
action_93 (98) = happyShift action_45
action_93 (102) = happyShift action_46
action_93 (103) = happyShift action_47
action_93 (104) = happyShift action_48
action_93 (105) = happyShift action_49
action_93 (106) = happyShift action_50
action_93 (107) = happyShift action_51
action_93 (108) = happyShift action_52
action_93 (109) = happyShift action_53
action_93 (6) = happyGoto action_32
action_93 (7) = happyGoto action_4
action_93 (8) = happyGoto action_5
action_93 (14) = happyGoto action_6
action_93 (15) = happyGoto action_33
action_93 (16) = happyGoto action_135
action_93 (20) = happyGoto action_35
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (30) = happyShift action_7
action_94 (32) = happyShift action_8
action_94 (43) = happyShift action_36
action_94 (44) = happyShift action_37
action_94 (52) = happyShift action_9
action_94 (57) = happyShift action_10
action_94 (62) = happyShift action_38
action_94 (65) = happyShift action_39
action_94 (66) = happyShift action_11
action_94 (68) = happyShift action_12
action_94 (78) = happyShift action_40
action_94 (82) = happyShift action_41
action_94 (85) = happyShift action_42
action_94 (88) = happyShift action_43
action_94 (96) = happyShift action_44
action_94 (98) = happyShift action_45
action_94 (102) = happyShift action_46
action_94 (103) = happyShift action_47
action_94 (104) = happyShift action_48
action_94 (105) = happyShift action_49
action_94 (106) = happyShift action_50
action_94 (107) = happyShift action_51
action_94 (108) = happyShift action_52
action_94 (109) = happyShift action_53
action_94 (6) = happyGoto action_32
action_94 (7) = happyGoto action_4
action_94 (8) = happyGoto action_5
action_94 (14) = happyGoto action_6
action_94 (15) = happyGoto action_33
action_94 (16) = happyGoto action_134
action_94 (20) = happyGoto action_35
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (109) = happyShift action_133
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (40) = happyShift action_58
action_96 (41) = happyShift action_59
action_96 (42) = happyShift action_60
action_96 (45) = happyShift action_61
action_96 (108) = happyShift action_62
action_96 (109) = happyShift action_63
action_96 (10) = happyGoto action_131
action_96 (29) = happyGoto action_132
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (40) = happyShift action_58
action_97 (41) = happyShift action_59
action_97 (42) = happyShift action_60
action_97 (45) = happyShift action_61
action_97 (108) = happyShift action_62
action_97 (109) = happyShift action_63
action_97 (29) = happyGoto action_130
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (30) = happyShift action_7
action_98 (32) = happyShift action_8
action_98 (43) = happyShift action_36
action_98 (44) = happyShift action_37
action_98 (52) = happyShift action_9
action_98 (57) = happyShift action_10
action_98 (62) = happyShift action_38
action_98 (65) = happyShift action_39
action_98 (66) = happyShift action_11
action_98 (68) = happyShift action_12
action_98 (78) = happyShift action_40
action_98 (82) = happyShift action_41
action_98 (85) = happyShift action_42
action_98 (88) = happyShift action_43
action_98 (96) = happyShift action_44
action_98 (98) = happyShift action_45
action_98 (102) = happyShift action_46
action_98 (103) = happyShift action_47
action_98 (104) = happyShift action_48
action_98 (105) = happyShift action_49
action_98 (106) = happyShift action_50
action_98 (107) = happyShift action_51
action_98 (108) = happyShift action_52
action_98 (109) = happyShift action_53
action_98 (6) = happyGoto action_32
action_98 (7) = happyGoto action_4
action_98 (8) = happyGoto action_5
action_98 (14) = happyGoto action_6
action_98 (15) = happyGoto action_33
action_98 (16) = happyGoto action_129
action_98 (20) = happyGoto action_35
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_46

action_100 (109) = happyShift action_128
action_100 _ = happyFail (happyExpListPerState 100)

action_101 _ = happyReduce_41

action_102 _ = happyReduce_40

action_103 _ = happyReduce_39

action_104 (31) = happyShift action_127
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (97) = happyShift action_126
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (89) = happyShift action_125
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (84) = happyShift action_113
action_107 (24) = happyGoto action_124
action_107 _ = happyReduce_72

action_108 (83) = happyShift action_123
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (30) = happyShift action_7
action_109 (32) = happyShift action_8
action_109 (43) = happyShift action_36
action_109 (44) = happyShift action_37
action_109 (52) = happyShift action_9
action_109 (57) = happyShift action_10
action_109 (62) = happyShift action_38
action_109 (65) = happyShift action_39
action_109 (66) = happyShift action_11
action_109 (68) = happyShift action_12
action_109 (78) = happyShift action_40
action_109 (82) = happyShift action_41
action_109 (85) = happyShift action_42
action_109 (88) = happyShift action_43
action_109 (96) = happyShift action_44
action_109 (98) = happyShift action_45
action_109 (102) = happyShift action_46
action_109 (103) = happyShift action_47
action_109 (104) = happyShift action_48
action_109 (105) = happyShift action_49
action_109 (106) = happyShift action_50
action_109 (107) = happyShift action_51
action_109 (108) = happyShift action_52
action_109 (109) = happyShift action_53
action_109 (6) = happyGoto action_32
action_109 (7) = happyGoto action_4
action_109 (8) = happyGoto action_5
action_109 (14) = happyGoto action_6
action_109 (15) = happyGoto action_33
action_109 (16) = happyGoto action_121
action_109 (20) = happyGoto action_35
action_109 (23) = happyGoto action_122
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_67

action_111 (84) = happyShift action_120
action_111 _ = happyReduce_71

action_112 _ = happyReduce_66

action_113 _ = happyReduce_74

action_114 (43) = happyShift action_36
action_114 (44) = happyShift action_37
action_114 (62) = happyShift action_38
action_114 (65) = happyShift action_39
action_114 (78) = happyShift action_40
action_114 (82) = happyShift action_41
action_114 (88) = happyShift action_43
action_114 (96) = happyShift action_44
action_114 (98) = happyShift action_45
action_114 (102) = happyShift action_46
action_114 (103) = happyShift action_47
action_114 (104) = happyShift action_48
action_114 (105) = happyShift action_49
action_114 (106) = happyShift action_50
action_114 (107) = happyShift action_51
action_114 (108) = happyShift action_52
action_114 (109) = happyShift action_53
action_114 (15) = happyGoto action_119
action_114 (20) = happyGoto action_35
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (63) = happyShift action_118
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (63) = happyShift action_117
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (109) = happyShift action_168
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (109) = happyShift action_167
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (87) = happyShift action_166
action_119 _ = happyFail (happyExpListPerState 119)

action_120 _ = happyReduce_75

action_121 (84) = happyShift action_113
action_121 (24) = happyGoto action_165
action_121 _ = happyReduce_72

action_122 _ = happyReduce_68

action_123 _ = happyReduce_65

action_124 (83) = happyReduce_73
action_124 (84) = happyShift action_120
action_124 _ = happyReduce_69

action_125 (43) = happyShift action_36
action_125 (44) = happyShift action_37
action_125 (62) = happyShift action_38
action_125 (65) = happyShift action_39
action_125 (78) = happyShift action_40
action_125 (82) = happyShift action_41
action_125 (88) = happyShift action_43
action_125 (96) = happyShift action_44
action_125 (98) = happyShift action_45
action_125 (101) = happyShift action_163
action_125 (102) = happyShift action_46
action_125 (103) = happyShift action_47
action_125 (104) = happyShift action_48
action_125 (105) = happyShift action_49
action_125 (106) = happyShift action_50
action_125 (107) = happyShift action_51
action_125 (108) = happyShift action_52
action_125 (109) = happyShift action_164
action_125 (15) = happyGoto action_159
action_125 (20) = happyGoto action_35
action_125 (25) = happyGoto action_160
action_125 (26) = happyGoto action_161
action_125 (27) = happyGoto action_162
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (81) = happyShift action_158
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (109) = happyShift action_157
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (33) = happyShift action_73
action_128 (17) = happyGoto action_156
action_128 _ = happyFail (happyExpListPerState 128)

action_129 _ = happyReduce_9

action_130 (30) = happyShift action_155
action_130 (35) = happyShift action_82
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (80) = happyShift action_97
action_131 _ = happyReduce_17

action_132 (30) = happyShift action_154
action_132 (35) = happyShift action_82
action_132 _ = happyFail (happyExpListPerState 132)

action_133 (33) = happyShift action_153
action_133 _ = happyReduce_20

action_134 _ = happyReduce_13

action_135 _ = happyReduce_12

action_136 (81) = happyShift action_152
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (59) = happyShift action_151
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (35) = happyShift action_82
action_138 (60) = happyShift action_150
action_138 _ = happyFail (happyExpListPerState 138)

action_139 (54) = happyShift action_149
action_139 _ = happyFail (happyExpListPerState 139)

action_140 (35) = happyShift action_82
action_140 _ = happyReduce_22

action_141 _ = happyReduce_29

action_142 _ = happyReduce_60

action_143 (43) = happyShift action_36
action_143 (44) = happyShift action_37
action_143 (62) = happyShift action_38
action_143 (65) = happyShift action_39
action_143 (78) = happyShift action_40
action_143 (82) = happyShift action_41
action_143 (88) = happyShift action_43
action_143 (96) = happyShift action_44
action_143 (98) = happyShift action_45
action_143 (102) = happyShift action_46
action_143 (103) = happyShift action_47
action_143 (104) = happyShift action_48
action_143 (105) = happyShift action_49
action_143 (106) = happyShift action_50
action_143 (107) = happyShift action_51
action_143 (108) = happyShift action_52
action_143 (109) = happyShift action_53
action_143 (15) = happyGoto action_148
action_143 (20) = happyGoto action_35
action_143 _ = happyFail (happyExpListPerState 143)

action_144 (62) = happyShift action_147
action_144 _ = happyFail (happyExpListPerState 144)

action_145 (109) = happyShift action_146
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (61) = happyShift action_185
action_146 _ = happyFail (happyExpListPerState 146)

action_147 (43) = happyShift action_36
action_147 (44) = happyShift action_37
action_147 (62) = happyShift action_38
action_147 (65) = happyShift action_39
action_147 (78) = happyShift action_40
action_147 (82) = happyShift action_41
action_147 (88) = happyShift action_43
action_147 (96) = happyShift action_44
action_147 (98) = happyShift action_45
action_147 (102) = happyShift action_46
action_147 (103) = happyShift action_47
action_147 (104) = happyShift action_48
action_147 (105) = happyShift action_49
action_147 (106) = happyShift action_50
action_147 (107) = happyShift action_51
action_147 (108) = happyShift action_52
action_147 (109) = happyShift action_53
action_147 (15) = happyGoto action_84
action_147 (20) = happyGoto action_35
action_147 (28) = happyGoto action_184
action_147 _ = happyFail (happyExpListPerState 147)

action_148 _ = happyReduce_84

action_149 (40) = happyShift action_58
action_149 (41) = happyShift action_59
action_149 (42) = happyShift action_60
action_149 (45) = happyShift action_61
action_149 (108) = happyShift action_62
action_149 (109) = happyShift action_63
action_149 (29) = happyGoto action_183
action_149 _ = happyFail (happyExpListPerState 149)

action_150 _ = happyReduce_24

action_151 (40) = happyShift action_58
action_151 (41) = happyShift action_59
action_151 (42) = happyShift action_60
action_151 (45) = happyShift action_61
action_151 (108) = happyShift action_62
action_151 (109) = happyShift action_63
action_151 (29) = happyGoto action_182
action_151 _ = happyFail (happyExpListPerState 151)

action_152 (30) = happyShift action_7
action_152 (32) = happyShift action_8
action_152 (43) = happyShift action_36
action_152 (44) = happyShift action_37
action_152 (52) = happyShift action_9
action_152 (57) = happyShift action_10
action_152 (62) = happyShift action_38
action_152 (65) = happyShift action_39
action_152 (66) = happyShift action_11
action_152 (68) = happyShift action_12
action_152 (78) = happyShift action_40
action_152 (82) = happyShift action_41
action_152 (85) = happyShift action_42
action_152 (88) = happyShift action_43
action_152 (96) = happyShift action_44
action_152 (98) = happyShift action_45
action_152 (102) = happyShift action_46
action_152 (103) = happyShift action_47
action_152 (104) = happyShift action_48
action_152 (105) = happyShift action_49
action_152 (106) = happyShift action_50
action_152 (107) = happyShift action_51
action_152 (108) = happyShift action_52
action_152 (109) = happyShift action_53
action_152 (6) = happyGoto action_32
action_152 (7) = happyGoto action_4
action_152 (8) = happyGoto action_5
action_152 (14) = happyGoto action_6
action_152 (15) = happyGoto action_33
action_152 (16) = happyGoto action_181
action_152 (20) = happyGoto action_35
action_152 _ = happyFail (happyExpListPerState 152)

action_153 (43) = happyShift action_36
action_153 (44) = happyShift action_37
action_153 (62) = happyShift action_38
action_153 (65) = happyShift action_39
action_153 (78) = happyShift action_40
action_153 (82) = happyShift action_41
action_153 (88) = happyShift action_43
action_153 (96) = happyShift action_44
action_153 (98) = happyShift action_45
action_153 (102) = happyShift action_46
action_153 (103) = happyShift action_47
action_153 (104) = happyShift action_48
action_153 (105) = happyShift action_49
action_153 (106) = happyShift action_50
action_153 (107) = happyShift action_51
action_153 (108) = happyShift action_52
action_153 (109) = happyShift action_53
action_153 (15) = happyGoto action_180
action_153 (20) = happyGoto action_35
action_153 _ = happyFail (happyExpListPerState 153)

action_154 (109) = happyShift action_179
action_154 _ = happyFail (happyExpListPerState 154)

action_155 (109) = happyShift action_178
action_155 _ = happyFail (happyExpListPerState 155)

action_156 _ = happyReduce_47

action_157 (99) = happyShift action_177
action_157 _ = happyFail (happyExpListPerState 157)

action_158 (43) = happyShift action_36
action_158 (44) = happyShift action_37
action_158 (62) = happyShift action_38
action_158 (65) = happyShift action_39
action_158 (78) = happyShift action_40
action_158 (82) = happyShift action_41
action_158 (88) = happyShift action_43
action_158 (96) = happyShift action_44
action_158 (98) = happyShift action_45
action_158 (102) = happyShift action_46
action_158 (103) = happyShift action_47
action_158 (104) = happyShift action_48
action_158 (105) = happyShift action_49
action_158 (106) = happyShift action_50
action_158 (107) = happyShift action_51
action_158 (108) = happyShift action_52
action_158 (109) = happyShift action_53
action_158 (15) = happyGoto action_176
action_158 (20) = happyGoto action_35
action_158 _ = happyFail (happyExpListPerState 158)

action_159 _ = happyReduce_81

action_160 (101) = happyShift action_175
action_160 _ = happyFail (happyExpListPerState 160)

action_161 (80) = happyShift action_174
action_161 _ = happyReduce_76

action_162 (80) = happyShift action_173
action_162 _ = happyReduce_77

action_163 _ = happyReduce_55

action_164 (33) = happyShift action_73
action_164 (63) = happyShift action_100
action_164 (17) = happyGoto action_172
action_164 _ = happyReduce_38

action_165 (83) = happyReduce_73
action_165 (84) = happyShift action_120
action_165 _ = happyReduce_70

action_166 (43) = happyShift action_36
action_166 (44) = happyShift action_37
action_166 (62) = happyShift action_38
action_166 (65) = happyShift action_39
action_166 (78) = happyShift action_40
action_166 (82) = happyShift action_41
action_166 (88) = happyShift action_43
action_166 (96) = happyShift action_44
action_166 (98) = happyShift action_45
action_166 (102) = happyShift action_46
action_166 (103) = happyShift action_47
action_166 (104) = happyShift action_48
action_166 (105) = happyShift action_49
action_166 (106) = happyShift action_50
action_166 (107) = happyShift action_51
action_166 (108) = happyShift action_52
action_166 (109) = happyShift action_53
action_166 (15) = happyGoto action_171
action_166 (20) = happyGoto action_35
action_166 _ = happyFail (happyExpListPerState 166)

action_167 (64) = happyShift action_170
action_167 _ = happyFail (happyExpListPerState 167)

action_168 (64) = happyShift action_169
action_168 _ = happyFail (happyExpListPerState 168)

action_169 _ = happyReduce_51

action_170 _ = happyReduce_50

action_171 (79) = happyShift action_194
action_171 _ = happyReduce_53

action_172 (80) = happyReduce_79
action_172 (101) = happyReduce_79
action_172 _ = happyReduce_79

action_173 (43) = happyShift action_36
action_173 (44) = happyShift action_37
action_173 (62) = happyShift action_38
action_173 (65) = happyShift action_39
action_173 (78) = happyShift action_40
action_173 (82) = happyShift action_41
action_173 (88) = happyShift action_43
action_173 (96) = happyShift action_44
action_173 (98) = happyShift action_45
action_173 (102) = happyShift action_46
action_173 (103) = happyShift action_47
action_173 (104) = happyShift action_48
action_173 (105) = happyShift action_49
action_173 (106) = happyShift action_50
action_173 (107) = happyShift action_51
action_173 (108) = happyShift action_52
action_173 (109) = happyShift action_164
action_173 (15) = happyGoto action_192
action_173 (20) = happyGoto action_35
action_173 (26) = happyGoto action_193
action_173 _ = happyFail (happyExpListPerState 173)

action_174 (109) = happyShift action_191
action_174 _ = happyFail (happyExpListPerState 174)

action_175 _ = happyReduce_54

action_176 _ = happyReduce_49

action_177 (43) = happyShift action_36
action_177 (44) = happyShift action_37
action_177 (62) = happyShift action_38
action_177 (65) = happyShift action_39
action_177 (78) = happyShift action_40
action_177 (82) = happyShift action_41
action_177 (88) = happyShift action_43
action_177 (96) = happyShift action_44
action_177 (98) = happyShift action_45
action_177 (102) = happyShift action_46
action_177 (103) = happyShift action_47
action_177 (104) = happyShift action_48
action_177 (105) = happyShift action_49
action_177 (106) = happyShift action_50
action_177 (107) = happyShift action_51
action_177 (108) = happyShift action_52
action_177 (109) = happyShift action_53
action_177 (15) = happyGoto action_190
action_177 (20) = happyGoto action_35
action_177 _ = happyFail (happyExpListPerState 177)

action_178 (33) = happyShift action_189
action_178 _ = happyFail (happyExpListPerState 178)

action_179 (33) = happyShift action_153
action_179 _ = happyReduce_21

action_180 _ = happyReduce_18

action_181 _ = happyReduce_11

action_182 (35) = happyShift action_82
action_182 (60) = happyShift action_188
action_182 _ = happyFail (happyExpListPerState 182)

action_183 (35) = happyShift action_82
action_183 _ = happyReduce_23

action_184 (50) = happyShift action_187
action_184 (80) = happyShift action_143
action_184 _ = happyFail (happyExpListPerState 184)

action_185 (43) = happyShift action_36
action_185 (44) = happyShift action_37
action_185 (62) = happyShift action_38
action_185 (65) = happyShift action_39
action_185 (78) = happyShift action_40
action_185 (82) = happyShift action_41
action_185 (88) = happyShift action_43
action_185 (96) = happyShift action_44
action_185 (98) = happyShift action_45
action_185 (102) = happyShift action_46
action_185 (103) = happyShift action_47
action_185 (104) = happyShift action_48
action_185 (105) = happyShift action_49
action_185 (106) = happyShift action_50
action_185 (107) = happyShift action_51
action_185 (108) = happyShift action_52
action_185 (109) = happyShift action_53
action_185 (15) = happyGoto action_186
action_185 (20) = happyGoto action_35
action_185 _ = happyFail (happyExpListPerState 185)

action_186 _ = happyReduce_64

action_187 _ = happyReduce_63

action_188 _ = happyReduce_25

action_189 (43) = happyShift action_36
action_189 (44) = happyShift action_37
action_189 (62) = happyShift action_38
action_189 (65) = happyShift action_39
action_189 (78) = happyShift action_40
action_189 (82) = happyShift action_41
action_189 (88) = happyShift action_43
action_189 (96) = happyShift action_44
action_189 (98) = happyShift action_45
action_189 (102) = happyShift action_46
action_189 (103) = happyShift action_47
action_189 (104) = happyShift action_48
action_189 (105) = happyShift action_49
action_189 (106) = happyShift action_50
action_189 (107) = happyShift action_51
action_189 (108) = happyShift action_52
action_189 (109) = happyShift action_53
action_189 (15) = happyGoto action_198
action_189 (20) = happyGoto action_35
action_189 _ = happyFail (happyExpListPerState 189)

action_190 (100) = happyShift action_197
action_190 _ = happyFail (happyExpListPerState 190)

action_191 (33) = happyShift action_73
action_191 (17) = happyGoto action_196
action_191 _ = happyFail (happyExpListPerState 191)

action_192 _ = happyReduce_82

action_193 (80) = happyShift action_174
action_193 _ = happyReduce_78

action_194 (43) = happyShift action_36
action_194 (44) = happyShift action_37
action_194 (62) = happyShift action_38
action_194 (65) = happyShift action_39
action_194 (78) = happyShift action_40
action_194 (82) = happyShift action_41
action_194 (88) = happyShift action_43
action_194 (96) = happyShift action_44
action_194 (98) = happyShift action_45
action_194 (102) = happyShift action_46
action_194 (103) = happyShift action_47
action_194 (104) = happyShift action_48
action_194 (105) = happyShift action_49
action_194 (106) = happyShift action_50
action_194 (107) = happyShift action_51
action_194 (108) = happyShift action_52
action_194 (109) = happyShift action_53
action_194 (15) = happyGoto action_195
action_194 (20) = happyGoto action_35
action_194 _ = happyFail (happyExpListPerState 194)

action_195 _ = happyReduce_52

action_196 _ = happyReduce_80

action_197 (43) = happyShift action_36
action_197 (44) = happyShift action_37
action_197 (62) = happyShift action_38
action_197 (65) = happyShift action_39
action_197 (78) = happyShift action_40
action_197 (82) = happyShift action_41
action_197 (88) = happyShift action_43
action_197 (96) = happyShift action_44
action_197 (98) = happyShift action_45
action_197 (102) = happyShift action_46
action_197 (103) = happyShift action_47
action_197 (104) = happyShift action_48
action_197 (105) = happyShift action_49
action_197 (106) = happyShift action_50
action_197 (107) = happyShift action_51
action_197 (108) = happyShift action_52
action_197 (109) = happyShift action_53
action_197 (15) = happyGoto action_199
action_197 (20) = happyGoto action_35
action_197 _ = happyFail (happyExpListPerState 197)

action_198 _ = happyReduce_19

action_199 (81) = happyShift action_200
action_199 _ = happyFail (happyExpListPerState 199)

action_200 (43) = happyShift action_36
action_200 (44) = happyShift action_37
action_200 (62) = happyShift action_38
action_200 (65) = happyShift action_39
action_200 (78) = happyShift action_40
action_200 (82) = happyShift action_41
action_200 (88) = happyShift action_43
action_200 (96) = happyShift action_44
action_200 (98) = happyShift action_45
action_200 (102) = happyShift action_46
action_200 (103) = happyShift action_47
action_200 (104) = happyShift action_48
action_200 (105) = happyShift action_49
action_200 (106) = happyShift action_50
action_200 (107) = happyShift action_51
action_200 (108) = happyShift action_52
action_200 (109) = happyShift action_53
action_200 (15) = happyGoto action_201
action_200 (20) = happyGoto action_35
action_200 _ = happyFail (happyExpListPerState 200)

action_201 _ = happyReduce_48

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (AST.Program (reverse happy_var_1)
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2:happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 4 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_2 ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (AST.Struct happy_var_2 (reverse happy_var_4)
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 4 6 happyReduction_5
happyReduction_5 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_2 ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (AST.Union  happy_var_2 (reverse happy_var_4)
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  6 happyReduction_8
happyReduction_8 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happyReduce 6 7 happyReduction_9
happyReduction_9 ((HappyAbsSyn15  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_2 ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (AST.Func happy_var_2 (reverse happy_var_4) (Just AST.TUnit) happy_var_6
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 4 7 happyReduction_10
happyReduction_10 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_2 ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (AST.Func happy_var_2 [] (Just AST.TUnit) happy_var_4
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 8 8 happyReduction_11
happyReduction_11 ((HappyAbsSyn15  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_2 ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (AST.Func happy_var_2 (reverse happy_var_6) (Just happy_var_4) happy_var_8
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 6 8 happyReduction_12
happyReduction_12 ((HappyAbsSyn15  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_2 ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (AST.Func happy_var_2 [] (Just happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 6 8 happyReduction_13
happyReduction_13 ((HappyAbsSyn15  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_2 ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (AST.Func happy_var_2 (reverse happy_var_4) Nothing happy_var_6
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 4 8 happyReduction_14
happyReduction_14 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_2 ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (AST.Func happy_var_2 [] Nothing happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  9 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  9 happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  9 happyReduction_17
happyReduction_17 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_3 ++ happy_var_1
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 5 10 happyReduction_18
happyReduction_18 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_3 ))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ([AST.FuncArg happy_var_3 happy_var_1 (Just happy_var_5)]
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 7 10 happyReduction_19
happyReduction_19 ((HappyAbsSyn15  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_5 ))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((AST.FuncArg happy_var_5 happy_var_3 (Just happy_var_7)):happy_var_1
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_3  11 happyReduction_20
happyReduction_20 (HappyTerminal (TK.Token _ (TK.TKid happy_var_3 )))
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn9
		 ([AST.FuncArg happy_var_3 happy_var_1 Nothing]
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 5 11 happyReduction_21
happyReduction_21 ((HappyTerminal (TK.Token _ (TK.TKid happy_var_5 ))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ((AST.FuncArg happy_var_5 happy_var_3 Nothing):happy_var_1
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_3  12 happyReduction_22
happyReduction_22 (HappyAbsSyn29  happy_var_3)
	_
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_1 )))
	 =  HappyAbsSyn12
		 ([(happy_var_1, happy_var_3)]
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 5 12 happyReduction_23
happyReduction_23 ((HappyAbsSyn29  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_3 ))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 ((happy_var_3, happy_var_5):happy_var_1
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 4 13 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn29  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_1 ))) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 ([(happy_var_1, happy_var_3)]
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 6 13 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn29  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_3 ))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 ((happy_var_3, happy_var_5):happy_var_1
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 4 14 happyReduction_26
happyReduction_26 ((HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_2 ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (AST.Variable happy_var_2 (Just happy_var_4) Nothing False
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 5 14 happyReduction_27
happyReduction_27 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	(HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_2 ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (AST.Variable happy_var_2 (Just happy_var_4) (Just happy_var_5) False
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_3  14 happyReduction_28
happyReduction_28 (HappyAbsSyn15  happy_var_3)
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_2 )))
	_
	 =  HappyAbsSyn6
		 (AST.Variable happy_var_2 Nothing (Just happy_var_3) False
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happyReduce 6 14 happyReduction_29
happyReduction_29 ((HappyAbsSyn15  happy_var_6) `HappyStk`
	(HappyAbsSyn29  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_3 ))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (AST.Variable happy_var_3 (Just happy_var_5) (Just happy_var_6) True
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 4 14 happyReduction_30
happyReduction_30 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_3 ))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (AST.Variable happy_var_3 Nothing (Just happy_var_4) True
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 5 14 happyReduction_31
happyReduction_31 ((HappyTerminal (TK.Token _ (TK.TKid happy_var_5 ))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_2 ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (AST.Reference happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_1  15 happyReduction_32
happyReduction_32 (HappyTerminal (TK.Token _ (TK.TKint happy_var_1)))
	 =  HappyAbsSyn15
		 (AST.NumExpr . AST.ConstInt $ happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  15 happyReduction_33
happyReduction_33 (HappyTerminal (TK.Token _ (TK.TKfloat happy_var_1)))
	 =  HappyAbsSyn15
		 (AST.NumExpr . AST.ConstFloat $ happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  15 happyReduction_34
happyReduction_34 (HappyTerminal (TK.Token _ (TK.TKchar happy_var_1)))
	 =  HappyAbsSyn15
		 (AST.ConstChar happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  15 happyReduction_35
happyReduction_35 (HappyTerminal (TK.Token _ (TK.TKstring happy_var_1)))
	 =  HappyAbsSyn15
		 (AST.ConstString happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  15 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn15
		 (AST.BoolExpr $ AST.TrueC
	)

happyReduce_37 = happySpecReduce_1  15 happyReduction_37
happyReduction_37 _
	 =  HappyAbsSyn15
		 (AST.BoolExpr $ AST.FalseC
	)

happyReduce_38 = happySpecReduce_1  15 happyReduction_38
happyReduction_38 (HappyTerminal (TK.Token _ (TK.TKid happy_var_1 )))
	 =  HappyAbsSyn15
		 (AST.Id happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  15 happyReduction_39
happyReduction_39 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (AST.Continue  (Just happy_var_2)
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_2  15 happyReduction_40
happyReduction_40 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (AST.Break     (Just happy_var_2)
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  15 happyReduction_41
happyReduction_41 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (AST.Return    (Just happy_var_2)
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  15 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn15
		 (AST.Continue  Nothing
	)

happyReduce_43 = happySpecReduce_1  15 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn15
		 (AST.Break     Nothing
	)

happyReduce_44 = happySpecReduce_1  15 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn15
		 (AST.Return    Nothing
	)

happyReduce_45 = happySpecReduce_1  15 happyReduction_45
happyReduction_45 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  15 happyReduction_46
happyReduction_46 (HappyAbsSyn15  happy_var_2)
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_1 )))
	 =  HappyAbsSyn15
		 (AST.Assign happy_var_1 happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happyReduce 4 15 happyReduction_47
happyReduction_47 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_3 ))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_1 ))) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (AST.StructAssign happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_48 = happyReduce 10 15 happyReduction_48
happyReduction_48 ((HappyAbsSyn15  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_4 ))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (AST.For happy_var_4 happy_var_2 happy_var_6 happy_var_8 happy_var_10
	) `HappyStk` happyRest

happyReduce_49 = happyReduce 5 15 happyReduction_49
happyReduction_49 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (AST.While happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 5 15 happyReduction_50
happyReduction_50 (_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_4 ))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_2 ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (AST.UnionTrying happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_51 = happyReduce 5 15 happyReduction_51
happyReduction_51 (_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_4 ))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_2 ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (AST.UnionUsing happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_52 = happyReduce 7 15 happyReduction_52
happyReduction_52 ((HappyAbsSyn15  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (AST.If happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_53 = happyReduce 5 15 happyReduction_53
happyReduction_53 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (AST.If happy_var_3 happy_var_5 AST.ConstUnit
	) `HappyStk` happyRest

happyReduce_54 = happyReduce 5 15 happyReduction_54
happyReduction_54 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_2 ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (AST.FunCall happy_var_2 (reverse happy_var_4)
	) `HappyStk` happyRest

happyReduce_55 = happyReduce 4 15 happyReduction_55
happyReduction_55 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_2 ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (AST.FunCall happy_var_2 []
	) `HappyStk` happyRest

happyReduce_56 = happySpecReduce_1  16 happyReduction_56
happyReduction_56 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  16 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn15
		 (AST.ConstUnit
	)

happyReduce_58 = happySpecReduce_1  16 happyReduction_58
happyReduction_58 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn15
		 (AST.Declaration happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_2  17 happyReduction_59
happyReduction_59 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happyReduce 4 17 happyReduction_60
happyReduction_60 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (AST.Array (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_61 = happySpecReduce_2  17 happyReduction_61
happyReduction_61 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_2  17 happyReduction_62
happyReduction_62 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happyReduce 6 18 happyReduction_63
happyReduction_63 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (AST.ConstStruct happy_var_2 (reverse happy_var_5)
	) `HappyStk` happyRest

happyReduce_64 = happyReduce 6 19 happyReduction_64
happyReduction_64 ((HappyAbsSyn15  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_4 ))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (AST.ConstUnion happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_65 = happySpecReduce_3  20 happyReduction_65
happyReduction_65 _
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (AST.ExprBlock (reverse happy_var_2)
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_2  20 happyReduction_66
happyReduction_66 _
	_
	 =  HappyAbsSyn15
		 (AST.ExprBlock []
	)

happyReduce_67 = happySpecReduce_1  21 happyReduction_67
happyReduction_67 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_2  21 happyReduction_68
happyReduction_68 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_2:happy_var_1
	)
happyReduction_68 _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_2  22 happyReduction_69
happyReduction_69 _
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_69 _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  22 happyReduction_70
happyReduction_70 _
	(HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_2:happy_var_1
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  22 happyReduction_71
happyReduction_71 _
	 =  HappyAbsSyn21
		 ([]
	)

happyReduce_72 = happySpecReduce_1  23 happyReduction_72
happyReduction_72 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_2  23 happyReduction_73
happyReduction_73 _
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_73 _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  24 happyReduction_74
happyReduction_74 _
	 =  HappyAbsSyn21
		 ([]
	)

happyReduce_75 = happySpecReduce_2  24 happyReduction_75
happyReduction_75 _
	_
	 =  HappyAbsSyn21
		 ([]
	)

happyReduce_76 = happySpecReduce_1  25 happyReduction_76
happyReduction_76 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  25 happyReduction_77
happyReduction_77 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  25 happyReduction_78
happyReduction_78 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_3 ++ happy_var_1
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_2  26 happyReduction_79
happyReduction_79 (HappyAbsSyn15  happy_var_2)
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_1 )))
	 =  HappyAbsSyn21
		 ([AST.Assign happy_var_1 happy_var_2]
	)
happyReduction_79 _ _  = notHappyAtAll 

happyReduce_80 = happyReduce 4 26 happyReduction_80
happyReduction_80 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	(HappyTerminal (TK.Token _ (TK.TKid happy_var_3 ))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ((AST.Assign happy_var_3 happy_var_4):happy_var_1
	) `HappyStk` happyRest

happyReduce_81 = happySpecReduce_1  27 happyReduction_81
happyReduction_81 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_3  27 happyReduction_82
happyReduction_82 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_3 : happy_var_1
	)
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  28 happyReduction_83
happyReduction_83 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_3  28 happyReduction_84
happyReduction_84 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_3:happy_var_1
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  29 happyReduction_85
happyReduction_85 _
	 =  HappyAbsSyn29
		 (AST.TFloat
	)

happyReduce_86 = happySpecReduce_1  29 happyReduction_86
happyReduction_86 _
	 =  HappyAbsSyn29
		 (AST.TInt
	)

happyReduce_87 = happySpecReduce_1  29 happyReduction_87
happyReduction_87 _
	 =  HappyAbsSyn29
		 (AST.TChar
	)

happyReduce_88 = happySpecReduce_1  29 happyReduction_88
happyReduction_88 _
	 =  HappyAbsSyn29
		 (AST.TString
	)

happyReduce_89 = happySpecReduce_1  29 happyReduction_89
happyReduction_89 _
	 =  HappyAbsSyn29
		 (AST.TBool
	)

happyReduce_90 = happySpecReduce_1  29 happyReduction_90
happyReduction_90 (HappyTerminal (TK.Token _ (TK.TKid happy_var_1 )))
	 =  HappyAbsSyn29
		 (AST.CustomType happy_var_1
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_2  29 happyReduction_91
happyReduction_91 _
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (AST.TPtr happy_var_1
	)
happyReduction_91 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 110 110 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TK.Token _ TK.TKbender -> cont 30;
	TK.Token _ TK.TKof -> cont 31;
	TK.Token _ TK.TKeternal -> cont 32;
	TK.Token _ TK.TKis -> cont 33;
	TK.Token _ TK.TKreincarnation -> cont 34;
	TK.Token _ TK.TKart -> cont 35;
	TK.Token _ TK.TKapprentice -> cont 36;
	TK.Token _ TK.TKborn -> cont 37;
	TK.Token _ TK.TKmember -> cont 38;
	TK.Token _ TK.TKdied -> cont 39;
	TK.Token _ TK.TKair -> cont 40;
	TK.Token _ TK.TKwater -> cont 41;
	TK.Token _ TK.TKfire -> cont 42;
	TK.Token _ TK.TKlightning -> cont 43;
	TK.Token _ TK.TKfireMaster -> cont 44;
	TK.Token _ TK.TKearth -> cont 45;
	TK.Token _ TK.TKmetal -> cont 46;
	TK.Token _ TK.TKnation -> cont 47;
	TK.Token _ TK.TKyear -> cont 48;
	TK.Token _ TK.TKmasterOf -> cont 49;
	TK.Token _ TK.TKRightNow -> cont 50;
	TK.Token _ TK.TKdisciple -> cont 51;
	TK.Token _ TK.TKelement -> cont 52;
	TK.Token _ TK.TKcompoundBy -> cont 53;
	TK.Token _ TK.TKskillOf -> cont 54;
	TK.Token _ TK.TKlearning -> cont 55;
	TK.Token _ TK.TKcontrol -> cont 56;
	TK.Token _ TK.TKenergy -> cont 57;
	TK.Token _ TK.TKallows -> cont 58;
	TK.Token _ TK.TKtechniqueOf -> cont 59;
	TK.Token _ TK.TKbending -> cont 60;
	TK.Token _ TK.TKtechniqueFrom -> cont 61;
	TK.Token _ TK.TKusing -> cont 62;
	TK.Token _ TK.TKquotmark_s -> cont 63;
	TK.Token _ TK.TKtechnique -> cont 64;
	TK.Token _ TK.TKtrying -> cont 65;
	TK.Token _ TK.TKbook -> cont 66;
	TK.Token _ TK.TKabout -> cont 67;
	TK.Token _ TK.TKtravel -> cont 68;
	TK.Token _ TK.TKmadeBy -> cont 69;
	TK.Token _ TK.TKandThen -> cont 70;
	TK.Token _ TK.TKbut -> cont 71;
	TK.Token _ TK.TKandThus -> cont 72;
	TK.Token _ TK.TKbesides -> cont 73;
	TK.Token _ TK.TKleft -> cont 74;
	TK.Token _ TK.TKand -> cont 75;
	TK.Token _ TK.TKor -> cont 76;
	TK.Token _ TK.TKnot -> cont 77;
	TK.Token _ TK.TKif -> cont 78;
	TK.Token _ TK.TKotherwise -> cont 79;
	TK.Token _ TK.TKcomma -> cont 80;
	TK.Token _ TK.TKcolon -> cont 81;
	TK.Token _ TK.TKbeginBlock -> cont 82;
	TK.Token _ TK.TKendBlock -> cont 83;
	TK.Token _ TK.TKdot -> cont 84;
	TK.Token _ TK.TKunit -> cont 85;
	TK.Token _ TK.TKopenParent -> cont 86;
	TK.Token _ TK.TKcloseParent -> cont 87;
	TK.Token _ TK.TKin -> cont 88;
	TK.Token _ TK.TKbookWith -> cont 89;
	TK.Token _ TK.TKwith -> cont 90;
	TK.Token _ TK.TKlessThan -> cont 91;
	TK.Token _ TK.TKlessEqThan -> cont 92;
	TK.Token _ TK.TKgreaterThan -> cont 93;
	TK.Token _ TK.TKgreaterEqThan -> cont 94;
	TK.Token _ TK.TKequal -> cont 95;
	TK.Token _ TK.TKwhile -> cont 96;
	TK.Token _ TK.TKdoing -> cont 97;
	TK.Token _ TK.TKopening -> cont 98;
	TK.Token _ TK.TKchakrasFrom -> cont 99;
	TK.Token _ TK.TKto -> cont 100;
	TK.Token _ TK.TKelipsis -> cont 101;
	TK.Token _ TK.TKtoBeContinued -> cont 102;
	TK.Token _ TK.TKburst -> cont 103;
	TK.Token _ TK.TKreturn -> cont 104;
	TK.Token _ (TK.TKint happy_dollar_dollar) -> cont 105;
	TK.Token _ (TK.TKfloat happy_dollar_dollar) -> cont 106;
	TK.Token _ (TK.TKchar happy_dollar_dollar) -> cont 107;
	TK.Token _ (TK.TKstring happy_dollar_dollar) -> cont 108;
	TK.Token _ (TK.TKid happy_dollar_dollar ) -> cont 109;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 110 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(TK.Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parseTokens tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [TK.Token] -> a
parseError ls = error $ "por " ++ show ls
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}















{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8336_0/ghc_2.h" #-}
































































































































































































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
