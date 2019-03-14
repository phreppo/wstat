module SyntacticStructure.WhileGrammar where

--------------------------------------------------------------------------------
--                         Domain of the variables
--------------------------------------------------------------------------------

type I = Integer 

type Var = String

-------------------------------------------------------------------------------
--                              While Grammar
-------------------------------------------------------------------------------

data Stmt = Seq Stmt Stmt
          | Assign Var AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
          | Assert BExpr
          deriving (Show,Eq)

data AExpr = Var      String
           | IntConst I
           | AUnary   AArithemticUnaryOperator AExpr
           | ABinary  AArithemticBinOperator AExpr AExpr
           | NonDet   SignedInfiniteInteger SignedInfiniteInteger
           deriving (Show,Eq)

data BExpr = BoolConst Bool
           | BooleanUnary     BArithmeticUnaryOperator BExpr
           | BooleanBinary    BBooleanBinOperator    BExpr BExpr
           | ArithmeticBinary BArithmeticBinOperator AExpr AExpr
           deriving (Show,Eq)

data BBooleanBinOperator = And | Or deriving (Show,Eq)

data BArithmeticUnaryOperator = Not deriving (Show, Eq)

data BArithmeticBinOperator = LessEq
                            | IsEqual
                            | IsNEqual
                            | Less
                            | Greater
                            | GreaterEq
                            deriving (Show,Eq)

data AArithemticUnaryOperator = Neg deriving (Show, Eq)

data AArithemticBinOperator = Add
                            | Subtract
                            | Multiply
                            | Division
                            deriving (Show, Eq, Enum, Bounded)

--------------------------------------------------------------------------------
--                        Non Determistic Bounds
--------------------------------------------------------------------------------

data SignedInfiniteInteger = Positive I
                           | Negative I
                           | PosInf
                           | NegInf
                           deriving (Show, Eq)
