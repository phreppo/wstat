module WhileGrammar
( I,
  V,
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
  BArithmeticUnaryOperator (..),
  AExpr (..),
  SignedInfiniteInteger (..),
  AArithemticBinOperator (..),
  AArithemticUnaryOperator (..)
)
where

--------------------------------------------------------------------------------
-- domain of variables
--------------------------------------------------------------------------------

type I = Integer -- from hw 3 the variables domain is simply an Integer

type V = String

-------------------------------------------------------------------------------
--                                 GRAMMAR
-------------------------------------------------------------------------------

data Stmt = Seq Stmt Stmt
          | Assign V AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
          | Assert BExpr
          deriving (Show,Eq)

data AExpr = Var      V
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
--                          Non Determinism Bounds
--------------------------------------------------------------------------------

data SignedInfiniteInteger = Positive I
                           | Negative I
                           | PosInf
                           | NegInf
                           deriving (Show, Eq)
