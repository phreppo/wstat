{
module Parser.StateParser
    (parse_state)
where
import Data.Char
import State
import UpdateState
}

%name state_p
%tokentype { TokenState }
%error { parseErrorState }

%token
      int               { TokenStateInt $$ }
      var               { TokenStateVar $$ }

      '-'               { TokenStateNegate }
      '['               { TokenStateOB }
      ']'               { TokenStateCB }
      ','               { TokenStateComma }
      '->'              { TokenStateArrow }
%%

Stat  : '[' VarsOrEmpty ']'         { $2 }

VarsOrEmpty : {- empty -}           { state [] }
            | Vars                  { $1 }

Vars  : var '->' Int          { state [($1,$3)] }
      | Vars ',' var '->' Int { update_entry ($3,$5) $1 }

Int : int       { $1 }
    | '-' int   { -$2 }

{
parseErrorState :: [TokenState] -> a
parseErrorState _ = error "Parse error"

data TokenState
    = TokenStateInt Integer
    | TokenStateVar String
    | TokenStateOB
    | TokenStateCB
    | TokenStateComma
    | TokenStateArrow
    | TokenStateNegate
    deriving Show

lexer_state :: String -> [TokenState]
lexer_state [] = []
lexer_state (c:cs)
        | isSpace c = lexer_state cs
        | isAlpha c = lexVar (c:cs)
        | isDigit c = lexNum (c:cs)
lexer_state ('-':'>':cs) = TokenStateArrow : lexer_state cs
lexer_state ('-':cs) = TokenStateNegate : lexer_state cs
lexer_state ('[':cs) = TokenStateOB : lexer_state cs
lexer_state (']':cs) = TokenStateCB : lexer_state cs
lexer_state (',':cs) = TokenStateComma : lexer_state cs

lexNum cs = TokenStateInt (read num) : lexer_state rest
        where (num,rest) = span isDigit cs

lexVar cs =
    case span isAlphaOrDigit cs of
        (var,rest)   -> TokenStateVar var : lexer_state rest

isAlphaOrDigit c = (isAlpha c) || (isDigit c)

parse_state string = (state_p . lexer_state) string

}