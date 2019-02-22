module EquationBasedSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Equation.CfgBuilder
import Equation.EquationList
import WhileGrammar
import Domain.Domain

tests = [whilePrograms x (showCFG (buildCfg y 1) [state]) name | (x, y, name) <- zip3 expected_cases result_cases names]


-- fake domain -----------------------------------------------------------------
data Tmp = T [(String, Integer)] deriving Show

instance Eq Tmp where
  _ == _ = True

instance Domain Tmp where
  lookupState _ = id
  updateState _ _ = id
  subset _ _ = Nothing
  top = []
  bottom = []
  assign _ = id
  cond _ = id
  join _ = id
  meet _ = id
  widen _ = id


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
    (1, [], 4),
    (3, [state], 6),
    (5, [state], 6),
    (2, [state], 3),
    (4, [state], 5)
  ], 6)
cond_name = "conditional operator"

while_result = While (BoolConst False) Skip
while_expected = ([
    (1, [state], 2),
    (2, [], 3),
    (2, [state], 5),
    (4, [state], 2),
    (3, [state], 4)
  ], 5)
while_name ="while loop"

skip_result = Skip
skip_expected = ([(1, [state], 2)], 2)
skip_name ="skip"

assert_result = Assert (BoolConst True)
assert_expected = ([(1, [state], 2)], 2)
assert_name ="assert"

program_result = If (BoolConst True) (While (BoolConst False) Skip) (Seq Skip Skip)
program_expected = ([
  (1,[state],2),
  (1,[],7),
  (6,[state],10),
  (9,[state],10),
  (2,[state],3),
  (3,[],4),
  (3,[state],6),
  (5,[state],3),
  (4,[state],5),
  (7,[state],8),
  (8,[state],9)],10)
program_name ="a simply program"

whilePrograms x y name =
  testCase ("Equation based [" ++ name ++ "]")
    (assertEqual "" x y) -- x:expected, y:result

result_cases = [ass_result, seq_result, cond_result, while_result, skip_result, assert_result, program_result]
expected_cases = [ass_expected, seq_expected, cond_expected, while_expected, skip_expected, assert_expected, program_expected]
names = [ass_name, seq_name, cond_name, while_name, skip_name, assert_name, program_name]