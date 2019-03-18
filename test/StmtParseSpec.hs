module StmtParseSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import SyntacticStructure.Parser
import SyntacticStructure.WhileGrammar

tests = [whilePrograms x (parse y) name | (x, y, name) <- zip3 st_cases cases names]

ass = "x := 2"
ass_st = Assign "x" (IntConst 2)

seq' = "skip; skip"
seq'_st = Seq Skip Skip

cond = "if true then skip else skip endif"
cond_st = If (BoolConst True) Skip Skip

while = "while false do skip done"
while_st = While (BoolConst False) Skip

skip = "skip"
skip_st = Skip

assert' = "assert true"
assert'_st = Assert (BoolConst True)

var = " x := x"
var_st = Assign "x" (Var "x")

negative = "x:=-2"
negative_st = Assign "x" (AUnary Neg (IntConst 2))

aritoperator = "x := 3 / 0"
aritoperator_st = Assign "x" (ABinary Division (IntConst 3) (IntConst 0))

whilePrograms x y name =
  testCase ("Simple Statements semantic tree [" ++ name ++ "]")
    (assertEqual "" x y) -- x:expected, y:result

cases = [ass, seq', cond, while, skip, assert', var, negative, aritoperator]
st_cases = [ass_st, seq'_st, cond_st, while_st, skip_st, assert'_st, var_st, negative_st, aritoperator_st]
names = ["ass", "seq", "cond", "while", "skip", "assert", "var", "negative", "aritoperator"]