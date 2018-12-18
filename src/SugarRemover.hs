module SugarRemover 
    ( remove_sugar )
where

import WhileGrammar

--------------------------------------------------------------------------------
--  BExpr sugar:
--      - or
--      - <
--      - >
--      - >=
--      - !=
--  AExpr sugar:
--      - ^ (onl with natural exponent)
--  Stmt sugar:
--      - repeat S until b
--      - for x:=a1 to a2 do S 
--------------------------------------------------------------------------------

remove_sugar :: Stmt -> Stmt
remove_sugar (Seq stmt1 stmt2) = 
    Seq (remove_sugar stmt1) (remove_sugar stmt2)
remove_sugar (Assign identifier aexpr) = 
    Assign identifier (remove_asugar aexpr)
remove_sugar (If bexpr stmt1 stmt2) = 
    If (remove_bsugar bexpr) (remove_sugar stmt1) (remove_sugar stmt2)
remove_sugar (While bexpr body) = 
    While (remove_bsugar bexpr) (remove_sugar body)
remove_sugar Skip =
    Skip
remove_sugar (Repeat body bexpr) =
    Seq body 
        (While (Not sugar_free_bexpr) sugar_free_body)
    where sugar_free_bexpr = remove_bsugar bexpr
          sugar_free_body  = remove_sugar body
remove_sugar (For identifier initial_value final_value body) =
    Seq (Assign identifier sugar_free_initial_value)
        (While 
            (ArithmeticBinary LessEq (Var identifier) sugar_free_final_value) 
            (Seq sugar_free_body (Assign identifier (ABinary Add (Var identifier) (IntConst 1)) )))
    where sugar_free_initial_value = remove_asugar initial_value
          sugar_free_final_value = remove_asugar final_value
          sugar_free_body = remove_sugar body
        

remove_bsugar :: BExpr -> BExpr

remove_bsugar (BoolConst b) = BoolConst b

remove_bsugar (Not bexpr) = Not (remove_bsugar bexpr)

-- simulate OR with AND and NOT: https://en.wikipedia.org/wiki/NAND_logic
remove_bsugar (BooleanBinary Or left right) = 
    make_nand 
        (make_nand sugar_free_left sugar_free_left) 
        (make_nand sugar_free_right sugar_free_right) 
    where sugar_free_left = remove_bsugar left
          sugar_free_right = remove_bsugar right

remove_bsugar (BooleanBinary And left right) = 
    BooleanBinary And sugar_free_left sugar_free_right
    where sugar_free_left = remove_bsugar left
          sugar_free_right = remove_bsugar right
          
remove_bsugar (ArithmeticBinary Less a1 a2) =
    BooleanBinary And 
        (ArithmeticBinary LessEq sugar_free_a1 sugar_free_a2) 
        (Not (ArithmeticBinary IsEqual sugar_free_a1 sugar_free_a2))
    where sugar_free_a1 = remove_asugar a1
          sugar_free_a2 = remove_asugar a2

remove_bsugar (ArithmeticBinary Greater a1 a2) =
    Not (ArithmeticBinary LessEq sugar_free_a1 sugar_free_a2)
    where sugar_free_a1 = remove_asugar a1
          sugar_free_a2 = remove_asugar a2

remove_bsugar (ArithmeticBinary GreaterEq a1 a2) =
    -- not less
    Not (remove_bsugar (ArithmeticBinary Less a1 a2))

remove_bsugar (ArithmeticBinary IsNEqual a1 a2) =
    -- not eq
    Not (remove_bsugar (ArithmeticBinary IsEqual a1 a2))

remove_bsugar (ArithmeticBinary op a1 a2) =
    ArithmeticBinary op sugar_free_a1 sugar_free_a2
    where sugar_free_a1 = remove_asugar a1
          sugar_free_a2 = remove_asugar a2

remove_asugar :: AExpr -> AExpr
remove_asugar (Exp a1 exponent) = 
        build_mult_n_times sugar_free_a1 exponent
        where sugar_free_a1 = remove_asugar a1

remove_asugar (Var v) = Var v
remove_asugar (IntConst i) = IntConst i
remove_asugar (Neg a1) = Neg (remove_asugar a1)
remove_asugar (ABinary op a1 a2) = 
    ABinary op (remove_asugar a1) (remove_asugar a2) 

build_mult_n_times aexpr 0 = IntConst 1
build_mult_n_times aexpr n = 
    ABinary Multiply aexpr (build_mult_n_times aexpr (n-1))


make_nand :: BExpr -> BExpr -> BExpr
make_nand bexpr1 bexpr2 =
    Not (BooleanBinary And bexpr1 bexpr2)