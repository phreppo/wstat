module WideningPointsSpec -- (tests)
    where

  -- import Test.Tasty
  -- import Test.Tasty.HUnit

  -- import Equation.CfgBuilder
  -- import Equation.WideningPoints
  -- import Equation.EquationList
  -- import WhileGrammar
  -- import Domain.Domain

  -- tests = [whilePrograms x (wideningPoints y 1) name | (x, y, name) <- zip3 expected_cases result_cases names]

  -- --               1                   2      3                 4   5 6    7    8      9                 10     11                12  13 14 15
  -- program_result = If (BoolConst True) (While (BoolConst False) Skip) (Seq Skip (While (BoolConst False) (While (BoolConst False) Skip)))
  -- program_expected = [3, 9, 11]
  -- program_name ="a simply program"

  -- whilePrograms x y name =
  --   testCase ("Widening Points [" ++ name ++ "]")
  --     (assertEqual "" x y) -- x:expected, y:result

  -- result_cases = [program_result]
  -- expected_cases = [program_expected]
  -- names = [program_name]