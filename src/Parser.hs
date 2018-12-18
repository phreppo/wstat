{-# OPTIONS_GHC -w #-}
module Parser where
import Data.Char
import WhileGrammar

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
happyExpList = Happy_Data_Array.listArray (0,157) ([42624,130,0,4096,0,2714,2,0,512,0,8192,0,0,0,112,2084,3584,1152,32769,33446,0,16,0,0,2048,0,128,2,32768,61831,32768,32768,1,0,0,0,0,0,0,6144,4608,0,16391,130,224,4168,32768,0,32780,8193,1,43424,32,0,10240,0,0,0,0,128,0,480,24576,18432,0,10856,8,896,16672,28672,9216,8,0,0,0,6520,15,8192,24,0,0,21312,65,3072,2304,32768,8193,1,48,36,512,0,49152,36864,0,24,18,768,576,24576,18432,0,12,9,384,288,28672,9216,8,32774,4,0,124,0,0,24,57344,1,0,60,0,1920,0,61440,0,0,30,0,960,0,0,0,0,8,0,384,0,12288,0,0,64,0,0,0,0,0,0,512,0,0,0,32770,0,57344,5,21312,65,3072,2304,0,61568,0,0,256,13312,1045,0,0,4,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_while_parse","Stmt","AExpr","BExpr","int","var","bool","'skip'","'if'","'then'","'else'","'while'","'do'","'repeat'","'until'","'for'","'to'","'+'","'-'","'*'","'^'","'('","')'","':='","';'","'='","'!='","'not'","'and'","'or'","'<='","'>='","'<'","'>'","%eof"]
        bit_start = st * 37
        bit_end = (st + 1) * 37
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..36]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (8) = happyShift action_4
action_0 (10) = happyShift action_5
action_0 (11) = happyShift action_6
action_0 (14) = happyShift action_7
action_0 (16) = happyShift action_8
action_0 (18) = happyShift action_9
action_0 (24) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (24) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (8) = happyShift action_4
action_2 (10) = happyShift action_5
action_2 (11) = happyShift action_6
action_2 (14) = happyShift action_7
action_2 (16) = happyShift action_8
action_2 (18) = happyShift action_9
action_2 (24) = happyShift action_2
action_2 (4) = happyGoto action_23
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (27) = happyShift action_22
action_3 (37) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (26) = happyShift action_21
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_4

action_6 (7) = happyShift action_14
action_6 (8) = happyShift action_15
action_6 (9) = happyShift action_16
action_6 (21) = happyShift action_17
action_6 (24) = happyShift action_18
action_6 (30) = happyShift action_19
action_6 (5) = happyGoto action_12
action_6 (6) = happyGoto action_20
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (7) = happyShift action_14
action_7 (8) = happyShift action_15
action_7 (9) = happyShift action_16
action_7 (21) = happyShift action_17
action_7 (24) = happyShift action_18
action_7 (30) = happyShift action_19
action_7 (5) = happyGoto action_12
action_7 (6) = happyGoto action_13
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (8) = happyShift action_4
action_8 (10) = happyShift action_5
action_8 (11) = happyShift action_6
action_8 (14) = happyShift action_7
action_8 (16) = happyShift action_8
action_8 (18) = happyShift action_9
action_8 (24) = happyShift action_2
action_8 (4) = happyGoto action_11
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (8) = happyShift action_10
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (26) = happyShift action_47
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (17) = happyShift action_46
action_11 (27) = happyShift action_22
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (20) = happyShift action_36
action_12 (21) = happyShift action_37
action_12 (22) = happyShift action_38
action_12 (23) = happyShift action_39
action_12 (28) = happyShift action_40
action_12 (29) = happyShift action_41
action_12 (33) = happyShift action_42
action_12 (34) = happyShift action_43
action_12 (35) = happyShift action_44
action_12 (36) = happyShift action_45
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (15) = happyShift action_35
action_13 (31) = happyShift action_29
action_13 (32) = happyShift action_30
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_10

action_15 _ = happyReduce_11

action_16 _ = happyReduce_18

action_17 (7) = happyShift action_14
action_17 (8) = happyShift action_15
action_17 (21) = happyShift action_17
action_17 (24) = happyShift action_27
action_17 (5) = happyGoto action_34
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (7) = happyShift action_14
action_18 (8) = happyShift action_15
action_18 (9) = happyShift action_16
action_18 (21) = happyShift action_17
action_18 (24) = happyShift action_18
action_18 (30) = happyShift action_19
action_18 (5) = happyGoto action_32
action_18 (6) = happyGoto action_33
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (7) = happyShift action_14
action_19 (8) = happyShift action_15
action_19 (9) = happyShift action_16
action_19 (21) = happyShift action_17
action_19 (24) = happyShift action_18
action_19 (30) = happyShift action_19
action_19 (5) = happyGoto action_12
action_19 (6) = happyGoto action_31
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (12) = happyShift action_28
action_20 (31) = happyShift action_29
action_20 (32) = happyShift action_30
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (7) = happyShift action_14
action_21 (8) = happyShift action_15
action_21 (21) = happyShift action_17
action_21 (24) = happyShift action_27
action_21 (5) = happyGoto action_26
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (8) = happyShift action_4
action_22 (10) = happyShift action_5
action_22 (11) = happyShift action_6
action_22 (14) = happyShift action_7
action_22 (16) = happyShift action_8
action_22 (18) = happyShift action_9
action_22 (24) = happyShift action_2
action_22 (4) = happyGoto action_25
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (25) = happyShift action_24
action_23 (27) = happyShift action_22
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_1

action_25 (27) = happyShift action_22
action_25 _ = happyReduce_3

action_26 (20) = happyShift action_36
action_26 (21) = happyShift action_37
action_26 (22) = happyShift action_38
action_26 (23) = happyShift action_39
action_26 _ = happyReduce_2

action_27 (7) = happyShift action_14
action_27 (8) = happyShift action_15
action_27 (21) = happyShift action_17
action_27 (24) = happyShift action_27
action_27 (5) = happyGoto action_66
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (8) = happyShift action_4
action_28 (10) = happyShift action_5
action_28 (11) = happyShift action_6
action_28 (14) = happyShift action_7
action_28 (16) = happyShift action_8
action_28 (18) = happyShift action_9
action_28 (24) = happyShift action_2
action_28 (4) = happyGoto action_65
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (7) = happyShift action_14
action_29 (8) = happyShift action_15
action_29 (9) = happyShift action_16
action_29 (21) = happyShift action_17
action_29 (24) = happyShift action_18
action_29 (30) = happyShift action_19
action_29 (5) = happyGoto action_12
action_29 (6) = happyGoto action_64
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (7) = happyShift action_14
action_30 (8) = happyShift action_15
action_30 (9) = happyShift action_16
action_30 (21) = happyShift action_17
action_30 (24) = happyShift action_18
action_30 (30) = happyShift action_19
action_30 (5) = happyGoto action_12
action_30 (6) = happyGoto action_63
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_19

action_32 (20) = happyShift action_36
action_32 (21) = happyShift action_37
action_32 (22) = happyShift action_38
action_32 (23) = happyShift action_39
action_32 (25) = happyShift action_62
action_32 (28) = happyShift action_40
action_32 (29) = happyShift action_41
action_32 (33) = happyShift action_42
action_32 (34) = happyShift action_43
action_32 (35) = happyShift action_44
action_32 (36) = happyShift action_45
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (25) = happyShift action_61
action_33 (31) = happyShift action_29
action_33 (32) = happyShift action_30
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_12

action_35 (8) = happyShift action_4
action_35 (10) = happyShift action_5
action_35 (11) = happyShift action_6
action_35 (14) = happyShift action_7
action_35 (16) = happyShift action_8
action_35 (18) = happyShift action_9
action_35 (24) = happyShift action_2
action_35 (4) = happyGoto action_60
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (7) = happyShift action_14
action_36 (8) = happyShift action_15
action_36 (21) = happyShift action_17
action_36 (24) = happyShift action_27
action_36 (5) = happyGoto action_59
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (7) = happyShift action_14
action_37 (8) = happyShift action_15
action_37 (21) = happyShift action_17
action_37 (24) = happyShift action_27
action_37 (5) = happyGoto action_58
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (7) = happyShift action_14
action_38 (8) = happyShift action_15
action_38 (21) = happyShift action_17
action_38 (24) = happyShift action_27
action_38 (5) = happyGoto action_57
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (7) = happyShift action_56
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (7) = happyShift action_14
action_40 (8) = happyShift action_15
action_40 (21) = happyShift action_17
action_40 (24) = happyShift action_27
action_40 (5) = happyGoto action_55
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (7) = happyShift action_14
action_41 (8) = happyShift action_15
action_41 (21) = happyShift action_17
action_41 (24) = happyShift action_27
action_41 (5) = happyGoto action_54
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (7) = happyShift action_14
action_42 (8) = happyShift action_15
action_42 (21) = happyShift action_17
action_42 (24) = happyShift action_27
action_42 (5) = happyGoto action_53
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (7) = happyShift action_14
action_43 (8) = happyShift action_15
action_43 (21) = happyShift action_17
action_43 (24) = happyShift action_27
action_43 (5) = happyGoto action_52
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (7) = happyShift action_14
action_44 (8) = happyShift action_15
action_44 (21) = happyShift action_17
action_44 (24) = happyShift action_27
action_44 (5) = happyGoto action_51
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (7) = happyShift action_14
action_45 (8) = happyShift action_15
action_45 (21) = happyShift action_17
action_45 (24) = happyShift action_27
action_45 (5) = happyGoto action_50
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (7) = happyShift action_14
action_46 (8) = happyShift action_15
action_46 (9) = happyShift action_16
action_46 (21) = happyShift action_17
action_46 (24) = happyShift action_18
action_46 (30) = happyShift action_19
action_46 (5) = happyGoto action_12
action_46 (6) = happyGoto action_49
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (7) = happyShift action_14
action_47 (8) = happyShift action_15
action_47 (21) = happyShift action_17
action_47 (24) = happyShift action_27
action_47 (5) = happyGoto action_48
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (19) = happyShift action_68
action_48 (20) = happyShift action_36
action_48 (21) = happyShift action_37
action_48 (22) = happyShift action_38
action_48 (23) = happyShift action_39
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (31) = happyShift action_29
action_49 (32) = happyShift action_30
action_49 _ = happyReduce_7

action_50 (20) = happyShift action_36
action_50 (21) = happyShift action_37
action_50 (22) = happyShift action_38
action_50 (23) = happyShift action_39
action_50 _ = happyReduce_27

action_51 (20) = happyShift action_36
action_51 (21) = happyShift action_37
action_51 (22) = happyShift action_38
action_51 (23) = happyShift action_39
action_51 _ = happyReduce_26

action_52 (20) = happyShift action_36
action_52 (21) = happyShift action_37
action_52 (22) = happyShift action_38
action_52 (23) = happyShift action_39
action_52 _ = happyReduce_25

action_53 (20) = happyShift action_36
action_53 (21) = happyShift action_37
action_53 (22) = happyShift action_38
action_53 (23) = happyShift action_39
action_53 _ = happyReduce_24

action_54 (20) = happyShift action_36
action_54 (21) = happyShift action_37
action_54 (22) = happyShift action_38
action_54 (23) = happyShift action_39
action_54 _ = happyReduce_22

action_55 (20) = happyShift action_36
action_55 (21) = happyShift action_37
action_55 (22) = happyShift action_38
action_55 (23) = happyShift action_39
action_55 _ = happyReduce_23

action_56 _ = happyReduce_16

action_57 (23) = happyShift action_39
action_57 _ = happyReduce_15

action_58 (22) = happyShift action_38
action_58 (23) = happyShift action_39
action_58 _ = happyReduce_14

action_59 (22) = happyShift action_38
action_59 (23) = happyShift action_39
action_59 _ = happyReduce_13

action_60 (27) = happyShift action_22
action_60 _ = happyReduce_6

action_61 _ = happyReduce_17

action_62 _ = happyReduce_9

action_63 (31) = happyShift action_29
action_63 _ = happyReduce_21

action_64 _ = happyReduce_20

action_65 (13) = happyShift action_67
action_65 (27) = happyShift action_22
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (20) = happyShift action_36
action_66 (21) = happyShift action_37
action_66 (22) = happyShift action_38
action_66 (23) = happyShift action_39
action_66 (25) = happyShift action_62
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (8) = happyShift action_4
action_67 (10) = happyShift action_5
action_67 (11) = happyShift action_6
action_67 (14) = happyShift action_7
action_67 (16) = happyShift action_8
action_67 (18) = happyShift action_9
action_67 (24) = happyShift action_2
action_67 (4) = happyGoto action_70
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (7) = happyShift action_14
action_68 (8) = happyShift action_15
action_68 (21) = happyShift action_17
action_68 (24) = happyShift action_27
action_68 (5) = happyGoto action_69
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (15) = happyShift action_71
action_69 (20) = happyShift action_36
action_69 (21) = happyShift action_37
action_69 (22) = happyShift action_38
action_69 (23) = happyShift action_39
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (27) = happyShift action_22
action_70 _ = happyReduce_5

action_71 (8) = happyShift action_4
action_71 (10) = happyShift action_5
action_71 (11) = happyShift action_6
action_71 (14) = happyShift action_7
action_71 (16) = happyShift action_8
action_71 (18) = happyShift action_9
action_71 (24) = happyShift action_2
action_71 (4) = happyGoto action_72
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (27) = happyShift action_22
action_72 _ = happyReduce_8

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  4 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Seq happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn4
		 (Skip
	)

happyReduce_5 = happyReduce 6 4 happyReduction_5
happyReduction_5 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 4 4 happyReduction_6
happyReduction_6 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 4 4 happyReduction_7
happyReduction_7 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Repeat happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 8 4 happyReduction_8
happyReduction_8 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (For happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn5
		 (IntConst happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  5 happyReduction_11
happyReduction_11 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn5
		 (Var happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  5 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Neg happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  5 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (ABinary Add happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (ABinary Subtract happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  5 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (ABinary Multiply happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  5 happyReduction_16
happyReduction_16 (HappyTerminal (TokenInt happy_var_3))
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Exp happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  6 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  6 happyReduction_18
happyReduction_18 (HappyTerminal (TokenBoolConst happy_var_1))
	 =  HappyAbsSyn6
		 (BoolConst happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  6 happyReduction_19
happyReduction_19 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Not happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  6 happyReduction_20
happyReduction_20 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (BooleanBinary And happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  6 happyReduction_21
happyReduction_21 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (BooleanBinary Or happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  6 happyReduction_22
happyReduction_22 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (ArithmeticBinary IsNEqual happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  6 happyReduction_23
happyReduction_23 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (ArithmeticBinary IsEqual happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  6 happyReduction_24
happyReduction_24 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (ArithmeticBinary LessEq happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  6 happyReduction_25
happyReduction_25 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (ArithmeticBinary GreaterEq happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  6 happyReduction_26
happyReduction_26 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (ArithmeticBinary Less happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  6 happyReduction_27
happyReduction_27 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (ArithmeticBinary Greater happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 37 37 notHappyAtAll (HappyState action) sts stk []

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
	TokenWhile -> cont 14;
	TokenDo -> cont 15;
	TokenRepeat -> cont 16;
	TokenUntil -> cont 17;
	TokenFor -> cont 18;
	TokenTo -> cont 19;
	TokenPlus -> cont 20;
	TokenMinus -> cont 21;
	TokenTimes -> cont 22;
	TokenExp -> cont 23;
	TokenOB -> cont 24;
	TokenCB -> cont 25;
	TokenAssign -> cont 26;
	TokenSemi -> cont 27;
	TokenEq -> cont 28;
	TokenNEq -> cont 29;
	TokenNot -> cont 30;
	TokenAnd -> cont 31;
	TokenOr -> cont 32;
	TokenLessEq -> cont 33;
	TokenGreaterEq -> cont 34;
	TokenLess -> cont 35;
	TokenGreater -> cont 36;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 37 tk tks = happyError' (tks, explist)
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
    = TokenInt Integer
    | TokenBoolConst Bool
    | TokenVar String
    | TokenAssign
    | TokenPlus
    | TokenMinus
    | TokenTimes
    | TokenEq
    | TokenOB
    | TokenCB
    | TokenSemi
    | TokenSkip
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenNot
    | TokenAnd
    | TokenOr
    | TokenLessEq
    | TokenLess
    | TokenGreaterEq
    | TokenGreater
    | TokenWhile
    | TokenDo
    | TokenRepeat
    | TokenUntil
    | TokenFor
    | TokenTo
    | TokenNEq
    | TokenExp
    deriving Show

lexer :: String -> [Token]
lexer [] = []
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
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer (';':cs) = TokenSemi : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('^':cs) = TokenExp : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
        where (num,rest) = span isDigit cs

lexVar cs =
    case span isAlphaOrDigit cs of
        ("if",rest) -> TokenIf : lexer rest
        ("then",rest) -> TokenThen : lexer rest
        ("else",rest) -> TokenElse : lexer rest
        ("skip",rest) -> TokenSkip : lexer rest
        ("true",rest) -> TokenBoolConst True : lexer rest
        ("false",rest) -> TokenBoolConst False : lexer rest
        ("not",rest) -> TokenNot : lexer rest
        ("and",rest) -> TokenAnd : lexer rest
        ("or",rest) -> TokenOr : lexer rest
        ("while",rest) -> TokenWhile : lexer rest
        ("do",rest) -> TokenDo : lexer rest
        ("repeat",rest) -> TokenRepeat : lexer rest
        ("until",rest) -> TokenUntil : lexer rest
        ("for",rest) -> TokenFor : lexer rest
        ("to",rest) -> TokenTo : lexer rest
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
