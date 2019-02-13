{
module Parser.Parser where
import Data.Char
import WhileGrammar

-- Op precedence:    https://en.cppreference.com/w/cpp/language/operator_precedence
-- Happy precedence: https://www.haskell.org/happy/doc/html/sec-Precedences.html

}

%name while_parse
%tokentype { Token }
%error { parseError }

%left 'or'
%left 'and'

%left '+' '-'
%left '*'
%left '^'
%left NEG 'not'

%left 'do' 'else'
%right ';'         -- ; is stronger than do and else, essential

%token
      int             { TokenInt $$ }
      var             { TokenVar $$ }
      bool            { TokenBoolConst $$ }

      'skip'          { TokenSkip }

      'if'            { TokenIf }
      'then'          { TokenThen }
      'else'          { TokenElse }

      'while'         { TokenWhile }
      'do'            { TokenDo }

      'repeat'        { TokenRepeat }
      'until'         { TokenUntil }

      'for'           { TokenFor }
      'to'            { TokenTo }

      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '^'             { TokenExp }
      '/'             { TokenDivide }

      '['             { TokenNonDetOB }
      ','             { TokenNonDetDel }
      ']'             { TokenNonDetCB }

      '('             { TokenOB }
      ')'             { TokenCB }

      ':='            { TokenAssign }
      ';'             { TokenSemi }

      '='             { TokenEq }
      '!='            { TokenNEq }
      'not'           { TokenNot }
      'and'           { TokenAnd }
      'or'            { TokenOr }

      '<='            { TokenLessEq }
      '>='            { TokenGreaterEq }
      '<'             { TokenLess }
      '>'             { TokenGreater }

%%

Stmt  : '(' Stmt ')'                                    { $2 }
      | var ':=' AExpr                                  { Assign $1 $3 }
      | Stmt ';' Stmt                                   { Seq $1 $3 }
      | 'skip'                                          { Skip }
      | 'if' BExpr 'then' Stmt 'else' Stmt              { If $2 $4 $6 }
      | 'while' BExpr 'do' Stmt                         { While $2 $4 }
      | 'repeat' Stmt 'until' BExpr                     { Repeat $2 $4 }
      | 'for' var ':=' AExpr 'to' AExpr 'do' Stmt       { For $2 $4 $6 $8 }

-- TODO: shoul admit infinite as value
AExpr : '(' AExpr ')'           { $2 }
      | int                     { IntConst $1 }
      | var                     { Var $1}
      | '-' AExpr %prec NEG     { Neg $2}
      | AExpr '+' AExpr         { ABinary Add $1 $3}
      | AExpr '-' AExpr         { ABinary Subtract $1 $3}
      | AExpr '*' AExpr         { ABinary Multiply $1 $3}
      | AExpr '/' AExpr         { ABinary Division $1 $3}
      | AExpr '^' int           { Exp $1 $3 }
      | '[' int ',' int ']'     { NonDet $2 $4 }

BExpr : '(' BExpr ')'           { $2 }
      | bool                    { BoolConst $1 }
      | 'not' BExpr             { Not $2 }
      | BExpr 'and' BExpr       { BooleanBinary And $1 $3 }
      | BExpr 'or' BExpr        { BooleanBinary Or $1 $3 }
      | AExpr '!=' AExpr        { ArithmeticBinary IsNEqual $1 $3 }
      | AExpr '=' AExpr         { ArithmeticBinary IsEqual $1 $3 }
      | AExpr '<=' AExpr        { ArithmeticBinary LessEq $1 $3 }
      | AExpr '>=' AExpr        { ArithmeticBinary GreaterEq $1 $3 }
      | AExpr '<' AExpr         { ArithmeticBinary Less $1 $3 }
      | AExpr '>' AExpr         { ArithmeticBinary Greater $1 $3 }

{

parseError :: [Token] -> a
-- https://www.haskell.org/happy/doc/html/sec-monads.html#sec-exception
parseError _ = error "Parse error"

data Token
    = TokenInt Integer
    | TokenBoolConst Bool
    | TokenVar String
    | TokenAssign
    | TokenPlus
    | TokenMinus
    | TokenTimes
    | TokenDivide
    | TokenEq
    | TokenNonDetOB
    | TokenNonDetDel
    | TokenNonDetCB
    | TokenOB
    | TokenCB
    | TokenSemi
    | TokenSkip
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenNot
    | TokenAnd
    | TokenOr
    | TokenLessEq
    | TokenLess
    | TokenGreaterEq
    | TokenGreater
    | TokenWhile
    | TokenDo
    | TokenRepeat
    | TokenUntil
    | TokenFor
    | TokenTo
    | TokenNEq
    | TokenExp
    deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
        | isSpace c = lexer cs
        | isAlpha c = lexVar (c:cs)
        | isDigit c = lexNum (c:cs)
-- note that is important for pattern matching to have >= upper than >
lexer ('!':'=':cs) = TokenNEq : lexer cs
lexer ('<':'=':cs) = TokenLessEq : lexer cs
lexer ('>':'=':cs) = TokenGreaterEq : lexer cs
lexer ('<':cs) = TokenLess : lexer cs
lexer ('>':cs) = TokenGreater : lexer cs
lexer (':':'=':cs) = TokenAssign : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDivide : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('[':cs) = TokenNonDetOB : lexer cs
lexer (',':cs) = TokenNonDetDel : lexer cs
lexer (']':cs) = TokenNonDetCB : lexer cs
lexer (';':cs) = TokenSemi : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('^':cs) = TokenExp : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
        where (num,rest) = span isDigit cs

lexVar cs =
    case span isAlphaOrDigit cs of
        ("if",rest) -> TokenIf : lexer rest
        ("then",rest) -> TokenThen : lexer rest
        ("else",rest) -> TokenElse : lexer rest
        ("skip",rest) -> TokenSkip : lexer rest
        ("true",rest) -> TokenBoolConst True : lexer rest
        ("false",rest) -> TokenBoolConst False : lexer rest
        ("not",rest) -> TokenNot : lexer rest
        ("and",rest) -> TokenAnd : lexer rest
        ("or",rest) -> TokenOr : lexer rest
        ("while",rest) -> TokenWhile : lexer rest
        ("do",rest) -> TokenDo : lexer rest
        ("repeat",rest) -> TokenRepeat : lexer rest
        ("until",rest) -> TokenUntil : lexer rest
        ("for",rest) -> TokenFor : lexer rest
        ("to",rest) -> TokenTo : lexer rest
        (var,rest)   -> TokenVar var : lexer rest

isAlphaOrDigit c = (isAlpha c) || (isDigit c)

parse string = (while_parse . lexer) string

}