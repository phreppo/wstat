module EquationBasedSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import EquationBased
import WhileGrammar
import Domain

tests = [whilePrograms x (showCFG (cfgBuilder y 1) [state]) name | (x, y, name) <- zip3 expected_cases result_cases names]


-- fake domain -----------------------------------------------------------------
data Tmp = T [(String, Integer)] deriving Show

instance Eq Tmp where
  _ == _ = True

instance Domain Tmp where
  assign _ = id
  cond _ = id

state = T [("x", 0)]
--------------------------------------------------------------------------------

ass_result = Assign "x" (IntConst 2)
ass_expected = ([(1, [state], 2)], 2)
ass_name = "assignments"

seq_result = Seq Skip Skip
seq_expected = ([(1, [state], 2), (2, [state], 3)], 3)
seq_name = "Sequencing Skips"

cond_result = If (BoolConst True) Skip Skip
cond_expected = ([
    (1, [state], 2),
    (1, [state], 4),
    (3, [state], 6),
    (5, [state], 6),
    (2, [state], 3),
    (4, [state], 5)
  ], 6)
cond_name = "conditional operator"

-- while = "while false do skip done"
-- while_st = While (BoolConst False) Skip

-- skip = "skip"
-- skip_st = Skip

-- assert' = "assert true"
-- assert'_st = Assert (BoolConst True)

-- var = " x := x"
-- var_st = Assign "x" (Var "x")

-- negative = "x:=-2"
-- negative_st = Assign "x" (Neg (IntConst 2))

-- aritoperator = "x := 3 / 1"
-- aritoperator_st = Assign "x" (ABinary Division (IntConst 3) (IntConst 0))

whilePrograms x y name =
  testCase ("Equation based [" ++ name ++ "]")
    (assertEqual "" x y) -- x:expected, y:result

result_cases = [ass_result, seq_result, cond_result]
expected_cases = [ass_expected, seq_expected, cond_expected]
names = [ass_name, seq_name, cond_name]