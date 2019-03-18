module EquationBasedSpec -- (tests)
    where

import Test.Tasty
import Test.Tasty.HUnit

import SyntacticStructure.ControlFlowGraph
import Tools.StateTransitions
import SyntacticStructure.WhileGrammar

tests = [whilePrograms x (applyST (cfg y (const 's') (const 'c')) 1) name | (x, y, name) <- zip3 expected_cases result_cases names]


ass_result = Assign "x" (IntConst 2)
ass_expected = ([(1, 's', 2)], 2)
ass_name = "assignments"

seq_result = Seq Skip Skip
seq_expected = ([(1, 's', 2), (2, 's', 3)], 3)
seq_name = "Sequencing Skips"

cond_result = If (BoolConst True) Skip Skip
cond_expected = ([
    (1,'c',2),
    (1,'c',4),
    (3,'s',6),
    (5,'s',6),
    (2,'s',3),
    (4,'s',5)],6)

cond_name = "conditional operator"

while_result = While (BoolConst False) Skip
while_expected = ([
    (1,'c',2),
    (1,'c',4),
    (3,'s',1),
    (2,'s',3)],4)

while_name ="while loop"

skip_result = Skip
skip_expected = ([(1, 's', 2)], 2)
skip_name ="skip"

assert_result = Assert (BoolConst True)
assert_expected = ([(1, 'c', 2)], 2)
assert_name ="assert"

program_result = If (BoolConst True) (While (BoolConst False) Skip) (Seq Skip Skip)
program_expected = ([
    (1,'c',2),
    (1,'c',6),
    (5,'s',9),
    (8,'s',9),
    (2,'c',3),
    (2,'c',5),
    (4,'s',2),
    (3,'s',4),
    (6,'s',7),
    (7,'s',8)],9)

program_name ="a simply program"

whilePrograms x y name =
  testCase ("Equation based [" ++ name ++ "]")
    (assertEqual "" x y) -- x:expected, y:result

result_cases = [ass_result, seq_result, cond_result, while_result, skip_result, assert_result, program_result]
expected_cases = [ass_expected, seq_expected, cond_expected, while_expected, skip_expected, assert_expected, program_expected]
names = [ass_name, seq_name, cond_name, while_name, skip_name, assert_name, program_name]