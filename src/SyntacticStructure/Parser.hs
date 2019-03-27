{-# OPTIONS_GHC -w #-}
module SyntacticStructure.Parser where
import SyntacticStructure.WhileGrammar
import Data.Char

-- Op precedence:    https://en.cppreference.com/w/cpp/language/operator_precedence
-- Happy precedence: https://www.haskell.org/happy/doc/html/sec-Precedences.html
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t4 t5 t6
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,167) ([18048,2,0,1,0,0,8192,0,0,128,0,0,0,56,4169,28672,37376,32,224,16676,0,15360,15456,0,0,6,0,0,0,0,0,0,0,6144,18688,0,16,2,57360,9216,65,448,33352,0,256,1536,8192,0,12,4660,0,3072,9344,0,32768,7,12288,37376,0,0,2048,32768,582,0,896,1168,1,8199,521,13312,18,0,0,0,0,34688,1932,0,49408,0,16384,0,64,0,0,0,1,0,0,0,16390,18,3072,9344,0,24,73,12288,37376,0,96,292,49152,18432,2,384,1168,0,8195,9,1536,4672,0,32780,36,0,1920,0,0,15,0,7680,0,0,60,0,30720,0,0,240,0,0,0,0,0,0,0,6,0,3072,0,32,4,64,32768,0,128,0,256,0,0,0,0,0,4096,256,0,0,32,0,0,0,8,8,0,1084,0,1165,0,0,0,0,0,8,0,4096,0,8,1,16,16384,0,32,0,0,0,1,0,0,0,0,4,0,0,0,0,16,2048,0,0,0,64,0,0,0,0,0,0,64,32,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_while_parse","Stmt","AExpr","BExpr","int","var","bool","'skip'","'if'","'then'","'else'","'endif'","'while'","'do'","'done'","'assert'","'+'","'-'","'*'","'/'","'['","','","']'","'('","')'","':='","';'","'='","'!='","'not'","'and'","'or'","'<='","'>='","'<'","'>'","'neginf'","'posinf'","%eof"]
        bit_start = st * 41
        bit_end = (st + 1) * 41
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..40]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (8) = happyShift action_2
action_0 (10) = happyShift action_4
action_0 (11) = happyShift action_5
action_0 (15) = happyShift action_6
action_0 (18) = happyShift action_7
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (8) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (28) = happyShift action_20
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (29) = happyShift action_19
action_3 (41) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_3

action_5 (7) = happyShift action_10
action_5 (8) = happyShift action_11
action_5 (9) = happyShift action_12
action_5 (20) = happyShift action_13
action_5 (23) = happyShift action_14
action_5 (26) = happyShift action_15
action_5 (32) = happyShift action_16
action_5 (5) = happyGoto action_8
action_5 (6) = happyGoto action_18
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (7) = happyShift action_10
action_6 (8) = happyShift action_11
action_6 (9) = happyShift action_12
action_6 (20) = happyShift action_13
action_6 (23) = happyShift action_14
action_6 (26) = happyShift action_15
action_6 (32) = happyShift action_16
action_6 (5) = happyGoto action_8
action_6 (6) = happyGoto action_17
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (7) = happyShift action_10
action_7 (8) = happyShift action_11
action_7 (9) = happyShift action_12
action_7 (20) = happyShift action_13
action_7 (23) = happyShift action_14
action_7 (26) = happyShift action_15
action_7 (32) = happyShift action_16
action_7 (5) = happyGoto action_8
action_7 (6) = happyGoto action_9
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (19) = happyShift action_35
action_8 (20) = happyShift action_36
action_8 (21) = happyShift action_37
action_8 (22) = happyShift action_38
action_8 (30) = happyShift action_39
action_8 (31) = happyShift action_40
action_8 (35) = happyShift action_41
action_8 (36) = happyShift action_42
action_8 (37) = happyShift action_43
action_8 (38) = happyShift action_44
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (33) = happyShift action_25
action_9 (34) = happyShift action_26
action_9 _ = happyReduce_6

action_10 _ = happyReduce_8

action_11 _ = happyReduce_9

action_12 _ = happyReduce_24

action_13 (7) = happyShift action_10
action_13 (8) = happyShift action_11
action_13 (20) = happyShift action_13
action_13 (23) = happyShift action_14
action_13 (26) = happyShift action_22
action_13 (5) = happyGoto action_34
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (7) = happyShift action_31
action_14 (20) = happyShift action_32
action_14 (39) = happyShift action_33
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (7) = happyShift action_10
action_15 (8) = happyShift action_11
action_15 (9) = happyShift action_12
action_15 (20) = happyShift action_13
action_15 (23) = happyShift action_14
action_15 (26) = happyShift action_15
action_15 (32) = happyShift action_16
action_15 (5) = happyGoto action_29
action_15 (6) = happyGoto action_30
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (7) = happyShift action_10
action_16 (8) = happyShift action_11
action_16 (9) = happyShift action_12
action_16 (20) = happyShift action_13
action_16 (23) = happyShift action_14
action_16 (26) = happyShift action_15
action_16 (32) = happyShift action_16
action_16 (5) = happyGoto action_8
action_16 (6) = happyGoto action_28
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (16) = happyShift action_27
action_17 (33) = happyShift action_25
action_17 (34) = happyShift action_26
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (12) = happyShift action_24
action_18 (33) = happyShift action_25
action_18 (34) = happyShift action_26
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (8) = happyShift action_2
action_19 (10) = happyShift action_4
action_19 (11) = happyShift action_5
action_19 (15) = happyShift action_6
action_19 (18) = happyShift action_7
action_19 (4) = happyGoto action_23
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (7) = happyShift action_10
action_20 (8) = happyShift action_11
action_20 (20) = happyShift action_13
action_20 (23) = happyShift action_14
action_20 (26) = happyShift action_22
action_20 (5) = happyGoto action_21
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (19) = happyShift action_35
action_21 (20) = happyShift action_36
action_21 (21) = happyShift action_37
action_21 (22) = happyShift action_38
action_21 _ = happyReduce_1

action_22 (7) = happyShift action_10
action_22 (8) = happyShift action_11
action_22 (20) = happyShift action_13
action_22 (23) = happyShift action_14
action_22 (26) = happyShift action_22
action_22 (5) = happyGoto action_64
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (29) = happyShift action_19
action_23 _ = happyReduce_2

action_24 (8) = happyShift action_2
action_24 (10) = happyShift action_4
action_24 (11) = happyShift action_5
action_24 (15) = happyShift action_6
action_24 (18) = happyShift action_7
action_24 (4) = happyGoto action_63
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (7) = happyShift action_10
action_25 (8) = happyShift action_11
action_25 (9) = happyShift action_12
action_25 (20) = happyShift action_13
action_25 (23) = happyShift action_14
action_25 (26) = happyShift action_15
action_25 (32) = happyShift action_16
action_25 (5) = happyGoto action_8
action_25 (6) = happyGoto action_62
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (7) = happyShift action_10
action_26 (8) = happyShift action_11
action_26 (9) = happyShift action_12
action_26 (20) = happyShift action_13
action_26 (23) = happyShift action_14
action_26 (26) = happyShift action_15
action_26 (32) = happyShift action_16
action_26 (5) = happyGoto action_8
action_26 (6) = happyGoto action_61
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (8) = happyShift action_2
action_27 (10) = happyShift action_4
action_27 (11) = happyShift action_5
action_27 (15) = happyShift action_6
action_27 (18) = happyShift action_7
action_27 (4) = happyGoto action_60
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_25

action_29 (19) = happyShift action_35
action_29 (20) = happyShift action_36
action_29 (21) = happyShift action_37
action_29 (22) = happyShift action_38
action_29 (27) = happyShift action_59
action_29 (30) = happyShift action_39
action_29 (31) = happyShift action_40
action_29 (35) = happyShift action_41
action_29 (36) = happyShift action_42
action_29 (37) = happyShift action_43
action_29 (38) = happyShift action_44
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (27) = happyShift action_58
action_30 (33) = happyShift action_25
action_30 (34) = happyShift action_26
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (24) = happyShift action_57
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (7) = happyShift action_56
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (24) = happyShift action_55
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_10

action_35 (7) = happyShift action_10
action_35 (8) = happyShift action_11
action_35 (20) = happyShift action_13
action_35 (23) = happyShift action_14
action_35 (26) = happyShift action_22
action_35 (5) = happyGoto action_54
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (7) = happyShift action_10
action_36 (8) = happyShift action_11
action_36 (20) = happyShift action_13
action_36 (23) = happyShift action_14
action_36 (26) = happyShift action_22
action_36 (5) = happyGoto action_53
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (7) = happyShift action_10
action_37 (8) = happyShift action_11
action_37 (20) = happyShift action_13
action_37 (23) = happyShift action_14
action_37 (26) = happyShift action_22
action_37 (5) = happyGoto action_52
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (7) = happyShift action_10
action_38 (8) = happyShift action_11
action_38 (20) = happyShift action_13
action_38 (23) = happyShift action_14
action_38 (26) = happyShift action_22
action_38 (5) = happyGoto action_51
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (7) = happyShift action_10
action_39 (8) = happyShift action_11
action_39 (20) = happyShift action_13
action_39 (23) = happyShift action_14
action_39 (26) = happyShift action_22
action_39 (5) = happyGoto action_50
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (7) = happyShift action_10
action_40 (8) = happyShift action_11
action_40 (20) = happyShift action_13
action_40 (23) = happyShift action_14
action_40 (26) = happyShift action_22
action_40 (5) = happyGoto action_49
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (7) = happyShift action_10
action_41 (8) = happyShift action_11
action_41 (20) = happyShift action_13
action_41 (23) = happyShift action_14
action_41 (26) = happyShift action_22
action_41 (5) = happyGoto action_48
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (7) = happyShift action_10
action_42 (8) = happyShift action_11
action_42 (20) = happyShift action_13
action_42 (23) = happyShift action_14
action_42 (26) = happyShift action_22
action_42 (5) = happyGoto action_47
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (7) = happyShift action_10
action_43 (8) = happyShift action_11
action_43 (20) = happyShift action_13
action_43 (23) = happyShift action_14
action_43 (26) = happyShift action_22
action_43 (5) = happyGoto action_46
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (7) = happyShift action_10
action_44 (8) = happyShift action_11
action_44 (20) = happyShift action_13
action_44 (23) = happyShift action_14
action_44 (26) = happyShift action_22
action_44 (5) = happyGoto action_45
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (19) = happyShift action_35
action_45 (20) = happyShift action_36
action_45 (21) = happyShift action_37
action_45 (22) = happyShift action_38
action_45 _ = happyReduce_33

action_46 (19) = happyShift action_35
action_46 (20) = happyShift action_36
action_46 (21) = happyShift action_37
action_46 (22) = happyShift action_38
action_46 _ = happyReduce_32

action_47 (19) = happyShift action_35
action_47 (20) = happyShift action_36
action_47 (21) = happyShift action_37
action_47 (22) = happyShift action_38
action_47 _ = happyReduce_31

action_48 (19) = happyShift action_35
action_48 (20) = happyShift action_36
action_48 (21) = happyShift action_37
action_48 (22) = happyShift action_38
action_48 _ = happyReduce_30

action_49 (19) = happyShift action_35
action_49 (20) = happyShift action_36
action_49 (21) = happyShift action_37
action_49 (22) = happyShift action_38
action_49 _ = happyReduce_28

action_50 (19) = happyShift action_35
action_50 (20) = happyShift action_36
action_50 (21) = happyShift action_37
action_50 (22) = happyShift action_38
action_50 _ = happyReduce_29

action_51 _ = happyReduce_14

action_52 _ = happyReduce_13

action_53 (21) = happyShift action_37
action_53 (22) = happyShift action_38
action_53 _ = happyReduce_12

action_54 (21) = happyShift action_37
action_54 (22) = happyShift action_38
action_54 _ = happyReduce_11

action_55 (7) = happyShift action_70
action_55 (20) = happyShift action_71
action_55 (40) = happyShift action_72
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (24) = happyShift action_69
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (7) = happyShift action_67
action_57 (40) = happyShift action_68
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_23

action_59 _ = happyReduce_7

action_60 (17) = happyShift action_66
action_60 (29) = happyShift action_19
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (33) = happyShift action_25
action_61 _ = happyReduce_27

action_62 _ = happyReduce_26

action_63 (13) = happyShift action_65
action_63 (29) = happyShift action_19
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (19) = happyShift action_35
action_64 (20) = happyShift action_36
action_64 (21) = happyShift action_37
action_64 (22) = happyShift action_38
action_64 (27) = happyShift action_59
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (8) = happyShift action_2
action_65 (10) = happyShift action_4
action_65 (11) = happyShift action_5
action_65 (15) = happyShift action_6
action_65 (18) = happyShift action_7
action_65 (4) = happyGoto action_81
action_65 _ = happyFail (happyExpListPerState 65)

action_66 _ = happyReduce_5

action_67 (25) = happyShift action_80
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (25) = happyShift action_79
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (7) = happyShift action_76
action_69 (20) = happyShift action_77
action_69 (40) = happyShift action_78
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (25) = happyShift action_75
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (7) = happyShift action_74
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (25) = happyShift action_73
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_22

action_74 (25) = happyShift action_86
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_19

action_76 (25) = happyShift action_85
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (7) = happyShift action_84
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (25) = happyShift action_83
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_21

action_80 _ = happyReduce_15

action_81 (14) = happyShift action_82
action_81 (29) = happyShift action_19
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_4

action_83 _ = happyReduce_20

action_84 (25) = happyShift action_87
action_84 _ = happyFail (happyExpListPerState 84)

action_85 _ = happyReduce_16

action_86 _ = happyReduce_18

action_87 _ = happyReduce_17

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Seq happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn4
		 (Skip
	)

happyReduce_4 = happyReduce 7 4 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 5 4 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_2  4 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Assert happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  5 happyReduction_8
happyReduction_8 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn5
		 (IntConst happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  5 happyReduction_9
happyReduction_9 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn5
		 (Var happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  5 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (AUnary Neg happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  5 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (ABinary Add happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  5 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (ABinary Subtract happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  5 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (ABinary Multiply happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (ABinary Division happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 5 5 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (NonDet (Positive happy_var_2) (Positive happy_var_4)
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 6 5 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (NonDet (Negative happy_var_3) (Positive happy_var_5)
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 7 5 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_6)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (NonDet (Negative happy_var_3) (Negative happy_var_6)
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 6 5 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (NonDet NegInf (Negative happy_var_5)
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 5 5 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (NonDet NegInf (Positive happy_var_4)
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 6 5 happyReduction_20
happyReduction_20 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (NonDet (Negative happy_var_3) PosInf
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 5 5 happyReduction_21
happyReduction_21 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (NonDet (Positive happy_var_2) PosInf
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 5 5 happyReduction_22
happyReduction_22 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (NonDet NegInf PosInf
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_3  6 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  6 happyReduction_24
happyReduction_24 (HappyTerminal (TokenBoolConst happy_var_1))
	 =  HappyAbsSyn6
		 (BoolConst happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  6 happyReduction_25
happyReduction_25 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (BooleanUnary Not happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  6 happyReduction_26
happyReduction_26 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (BooleanBinary And happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  6 happyReduction_27
happyReduction_27 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (BooleanBinary Or happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  6 happyReduction_28
happyReduction_28 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (ArithmeticBinary IsNEqual happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  6 happyReduction_29
happyReduction_29 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (ArithmeticBinary IsEqual happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  6 happyReduction_30
happyReduction_30 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (ArithmeticBinary LessEq happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  6 happyReduction_31
happyReduction_31 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (ArithmeticBinary GreaterEq happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  6 happyReduction_32
happyReduction_32 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (ArithmeticBinary Less happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  6 happyReduction_33
happyReduction_33 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (ArithmeticBinary Greater happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 41 41 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenInt happy_dollar_dollar -> cont 7;
	TokenVar happy_dollar_dollar -> cont 8;
	TokenBoolConst happy_dollar_dollar -> cont 9;
	TokenSkip -> cont 10;
	TokenIf -> cont 11;
	TokenThen -> cont 12;
	TokenElse -> cont 13;
	TokenEndif -> cont 14;
	TokenWhile -> cont 15;
	TokenDo -> cont 16;
	TokenDone -> cont 17;
	TokenAssert -> cont 18;
	TokenPlus -> cont 19;
	TokenMinus -> cont 20;
	TokenTimes -> cont 21;
	TokenDivide -> cont 22;
	TokenNonDetOB -> cont 23;
	TokenNonDetDel -> cont 24;
	TokenNonDetCB -> cont 25;
	TokenOB -> cont 26;
	TokenCB -> cont 27;
	TokenAssign -> cont 28;
	TokenSemi -> cont 29;
	TokenEq -> cont 30;
	TokenNEq -> cont 31;
	TokenNot -> cont 32;
	TokenAnd -> cont 33;
	TokenOr -> cont 34;
	TokenLessEq -> cont 35;
	TokenGreaterEq -> cont 36;
	TokenLess -> cont 37;
	TokenGreater -> cont 38;
	TokenNegInf -> cont 39;
	TokenPosInf -> cont 40;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 41 tk tks = happyError' (tks, explist)
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
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
while_parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
-- https://www.haskell.org/happy/doc/html/sec-monads.html#sec-exception
parseError _ = error "Parse error"

data Token
    = TokenInt I
    | TokenBoolConst Bool
    | TokenVar String
    | TokenAssign
    | TokenPlus
    | TokenMinus
    | TokenTimes
    | TokenDivide
    | TokenEq
    | TokenNonDetOB
    | TokenNonDetDel
    | TokenNonDetCB
    | TokenOB
    | TokenCB
    | TokenSemi
    | TokenSkip
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenEndif
    | TokenNot
    | TokenAnd
    | TokenOr
    | TokenLessEq
    | TokenLess
    | TokenGreaterEq
    | TokenGreater
    | TokenWhile
    | TokenDo
    | TokenDone
    | TokenAssert
    | TokenNEq
    | TokenExp
    | TokenPosInf
    | TokenNegInf
    deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer ('#':cs) = lexer $ tail $ dropWhile (/= '#') cs
lexer (c:cs)
        | isSpace c = lexer cs
        | isAlpha c = lexVar (c:cs)
        | isDigit c = lexNum (c:cs)
-- note that is important for pattern matching to have >= upper than >
lexer ('!':'=':cs) = TokenNEq : lexer cs
lexer ('<':'=':cs) = TokenLessEq : lexer cs
lexer ('>':'=':cs) = TokenGreaterEq : lexer cs
lexer ('<':cs) = TokenLess : lexer cs
lexer ('>':cs) = TokenGreater : lexer cs
lexer (':':'=':cs) = TokenAssign : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDivide : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('[':cs) = TokenNonDetOB : lexer cs
lexer (',':cs) = TokenNonDetDel : lexer cs
lexer (']':cs) = TokenNonDetCB : lexer cs
lexer (';':cs) = TokenSemi : lexer cs
lexer ('=':cs) = TokenEq : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
        where (num,rest) = span isDigit cs

lexVar cs =
    case span isAlphaOrDigit cs of
        ("if",rest) -> TokenIf : lexer rest
        ("then",rest) -> TokenThen : lexer rest
        ("else",rest) -> TokenElse : lexer rest
        ("endif",rest) -> TokenEndif : lexer rest
        ("skip",rest) -> TokenSkip : lexer rest
        ("true",rest) -> TokenBoolConst True : lexer rest
        ("false",rest) -> TokenBoolConst False : lexer rest
        ("not",rest) -> TokenNot : lexer rest
        ("and",rest) -> TokenAnd : lexer rest
        ("or",rest) -> TokenOr : lexer rest
        ("while",rest) -> TokenWhile : lexer rest
        ("do",rest) -> TokenDo : lexer rest
        ("done",rest) -> TokenDone : lexer rest
        ("assert",rest) -> TokenAssert : lexer rest
        ("neginf",rest) -> TokenNegInf : lexer rest
        ("posinf",rest) -> TokenPosInf : lexer rest
        (var,rest)   -> TokenVar var : lexer rest

isAlphaOrDigit c = (isAlpha c) || (isDigit c)

parse string = (while_parse . lexer) string
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}















{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8814_0/ghc_2.h" #-}








































































































































































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
