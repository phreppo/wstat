module WhileGrammar
( Stmt (
    Seq,
    Assign,
    If,
    While,
    Repeat,
    For,
    Skip),
  BExpr (..),
  BBooleanBinOperator (..),
  BArithmeticBinOperator (..),
  AExpr (..),
  AArithemticBinOperator (..),
)
where

-------------------------------------------------------------------------------
--                                 GRAMMAR
-------------------------------------------------------------------------------

data Stmt = Seq Stmt Stmt
          | Assign String AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
          -- Sugar
          | Repeat Stmt BExpr
          | For String AExpr AExpr Stmt
          deriving (Show,Eq)

data BExpr = BoolConst Bool
           | Not BExpr
           | BooleanBinary    BBooleanBinOperator    BExpr BExpr
           | ArithmeticBinary BArithmeticBinOperator AExpr AExpr
           deriving (Show,Eq)

data BBooleanBinOperator = And
                         -- Sugar
                         | Or
                         deriving (Show,Eq)

data BArithmeticBinOperator = LessEq
                            | IsEqual
                            -- Sugar
                            | IsNEqual
                            | Less
                            | Greater
                            | GreaterEq
                            deriving (Show,Eq)

data AExpr = Var      String
           | IntConst Integer
           | Neg      AExpr
           | ABinary  AArithemticBinOperator AExpr AExpr
           | NonDet   Integer Integer
          --  Sugar
           | Exp      AExpr Integer
           deriving (Show,Eq)

data AArithemticBinOperator = Add
                            | Subtract
                            | Multiply
                            deriving (Show,Eq)