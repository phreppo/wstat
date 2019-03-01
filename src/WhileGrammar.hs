module WhileGrammar
( I,
  Stmt (
    Seq,
    Assign,
    If,
    While,
    Skip,
    Assert),
  BExpr (..),
  BBooleanBinOperator (..),
  BArithmeticBinOperator (..),
  AExpr (..),
  SignedInfiniteInteger (..),
  AArithemticBinOperator (..)
)
where

--------------------------------------------------------------------------------
-- domain of variables
--------------------------------------------------------------------------------

-- as required from homowork 3 the variables domain is simply an Integer
type I = Integer

-------------------------------------------------------------------------------
--                                 GRAMMAR
-------------------------------------------------------------------------------

data Stmt = Seq Stmt Stmt
          | Assign String AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
          | Assert BExpr
          deriving (Show,Eq)

data AExpr = Var      String
           | IntConst I
           | Neg      AExpr
           | ABinary  AArithemticBinOperator AExpr AExpr
           | NonDet   SignedInfiniteInteger SignedInfiniteInteger
           deriving (Show,Eq)

data BExpr = BoolConst Bool
           | Not BExpr -- Sugar with De Morgan rules
           | BooleanBinary    BBooleanBinOperator    BExpr BExpr
           | ArithmeticBinary BArithmeticBinOperator AExpr AExpr
           deriving (Show,Eq)

data BBooleanBinOperator = And | Or deriving (Show,Eq)

data BArithmeticBinOperator = LessEq
                            | IsEqual
                            | IsNEqual
                            | Less
                            | Greater
                            | GreaterEq
                            deriving (Show,Eq)

data AArithemticBinOperator = Add
                            | Subtract
                            | Multiply
                            | Division
                            deriving (Show,Eq)

--------------------------------------------------------------------------------
--                          Non Determinism Bounds
--------------------------------------------------------------------------------

data SignedInfiniteInteger = Positive I
                           | Negative I
                           | PosInf
                           | NegInf
                           deriving (Show, Eq)
