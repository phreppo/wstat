module WhileProgramSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Parser.Parser
import WhileGrammar

tests = [whilePrograms x (parse y) name | (x, y, name) <- zip3 st_cases cases names]

-- x non det; if x > 0 then 1 else -1
non_det = "x := [-10, 10]; if x > [-1, 1] then y := 1 else y := [-1, -1] endif"
non_det_st = Seq (Assign "x" (NonDet (Negative 10) (Positive 10))) (If
      (ArithmeticBinary Greater (Var "x") (NonDet (Negative 1) (Positive 1)))
      (Assign "y" (IntConst 1))
      (Assign "y" (NonDet (Negative 1) (Negative 1))))

non_det_inf = "x := [neginf, posinf]"
non_det_inf_st = Assign "y" (NonDet NegInf PosInf)

-- bool_op in if operator
bool_op = "if x = 0 and x = 1 then skip else y := -1 endif"
bool_op_st = If
  (BooleanBinary And
    (ArithmeticBinary IsEqual (Var "x") (IntConst 0))
    (ArithmeticBinary IsEqual (Var "x") (IntConst 1)))
  Skip
  (Assign "y" (Neg (IntConst 1)))

-- x!
-- https://coolconversion.com/math/factorial/_700_factorial%3F
-- https://coolconversion.com/math/factorial/What-is-the-factorial-of_7000_%3F
factorial = "y:=1; while not x = 1 do y := y*x; x := x-1 done"
factorial_st = Seq
  (Assign "y" (IntConst 1))
  (While
    (Not (ArithmeticBinary IsEqual (Var "x") (IntConst 1)))
    (Seq
      (Assign "y" (ABinary Multiply (Var "y") (Var "x")))
      (Assign "x" (ABinary Subtract (Var "x") (IntConst 1)))))

-- x % y
modul = "y := 1; r := x; while r >= y do r := r - y done"
modul_st = Seq
  (Assign "y" (IntConst 1))
  (Seq
    (Assign "r" (Var "x"))
    (While
      (ArithmeticBinary GreaterEq (Var "r") (Var "y"))
      (Assign "r" (ABinary Subtract (Var "r") (Var "y")))))

whilePrograms x y name =
  testCase ("while programs semantic tree [" ++ name ++ "]")
    (assertEqual "" x y) -- x:expected, y:result

cases = [non_det, non_det_inf, bool_op, factorial, modul]
st_cases = [non_det_st, non_det_inf_st, bool_op_st, factorial_st, modul_st]
names = ["non_det", "non_det_inf", "bool_op", "factorial", "modul"]