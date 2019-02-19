module WhileGrammar
( Stmt (
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
  AArithemticBinOperator (..),
  AtomicAssign (..),
  AtomicUnaryCond (..),
  bexpr2atomic,
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
          | Assert BExpr
          deriving (Show,Eq)

data BExpr = BoolConst Bool -- not used
           | Not BExpr -- Sugar with De Morgan rules
           | BooleanBinary    BBooleanBinOperator    BExpr BExpr
           | ArithmeticBinary BArithmeticBinOperator AExpr AExpr -- not used
           | ArithmeticUnary  BArithmeticBinOperator AExpr -- not parsed
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

-- TODO: IntConst should be a singleton NonDet
data AExpr = Var      String
           | IntConst Integer -- not used
           | Neg      AExpr
           | ABinary  AArithemticBinOperator AExpr AExpr
           | NonDet   Integer Integer
          --  Sugar
           | Exp      AExpr Integer -- not used
           deriving (Show,Eq)

data AArithemticBinOperator = Add
                            | Subtract
                            | Multiply
                            | Division
                            deriving (Show,Eq)

-- equational based semantic

data AtomicAssign = AtomicAssign String AExpr deriving Show
data AtomicUnaryCond = AtomicUnaryCond BArithmeticBinOperator AExpr deriving Show

-- assign2atomic :: Stmt -> AtomicAssign
-- assign2atomic (Assign var expr) = AtomicAssign var expr

-- TODO: aggiungere conversioni per gli altri casi
bexpr2atomic :: BExpr -> AtomicUnaryCond
bexpr2atomic (ArithmeticUnary op expr) = AtomicUnaryCond op expr
bexpr2atomic (Not expr) = bexpr2atomic expr -- TODO: must rely on De Morgan rules
-- 'new' While doesn't accept all bexpr