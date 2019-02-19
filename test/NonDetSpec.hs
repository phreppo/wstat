module NonDetSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Parser.Parser
import WhileGrammar

tests = [whilePrograms x (parse y) name | (x, y, name) <- zip3 st_cases cases names]

nondet1 = "x := [0, 3]"
nondet1_st = Assign "x" (NonDet (Positive 0) (Positive 3))
nondet1_name = "positive bounds"

nondet2 = "x := [-10, 3]"
nondet2_st = Assign "x" (NonDet (Negative 10) (Positive 3))
nondet2_name = "first negative, second positive"

nondet3 = "x := [-10, -30]" -- parser can't recognize this bound error
nondet3_st = Assign "x" (NonDet (Negative 10) (Negative 30))
nondet3_name = "negative bounds"

nondet4 = "x := [neginf, -30]"
nondet4_st = Assign "x" (NonDet NegInf (Negative 30))
nondet4_name = "first neginf, second negative"

nondet5 = "x := [neginf, 30]"
nondet5_st = Assign "x" (NonDet NegInf (Positive 30))
nondet5_name = "first neginf, second positive"

nondet6 = "x := [-10, posinf]"
nondet6_st = Assign "x" (NonDet (Negative 10) PosInf)
nondet6_name = "first negative, second posinf"

nondet7 = "x := [10, posinf]"
nondet7_st = Assign "x" (NonDet (Positive 10) PosInf)
nondet7_name = "first positivem seconf posinf"

nondet8 = "x := [neginf, posinf]"
nondet8_st = Assign "x" (NonDet NegInf PosInf)
nondet8_name = "infinite bounds"

whilePrograms x y name =
  testCase ("NonDet cases semantic tree [" ++ name ++ "]")
    (assertEqual "" x y) -- x:expected, y:result

cases = [nondet1, nondet2, nondet3, nondet4, nondet5, nondet6, nondet7, nondet8]
st_cases = [nondet1_st, nondet2_st, nondet3_st, nondet4_st, nondet5_st, nondet6_st, nondet7_st, nondet8_st]
names = [nondet1_name, nondet2_name, nondet3_name, nondet4_name, nondet5_name, nondet6_name, nondet7_name, nondet8_name]