{
module Parser where

import Ast
import Lexer
}

%name expr
%tokentype { Token }
%error { parseError }

%token
    true    { TRUE }
    false   { FALSE }
    neg     { NEG }
    conj    { CONJ }
    disj    { DISJ }
    impl    { IMPL }
    equiv   { EQUIV }
    forall  { FORALL }
    exists  { EXISTS }
    dot     { DOT }
    '('     { LPAREN }
    ')'     { RPAREN }
    VAR     { VAR $$ }

%right equiv
%right impl
%right disj
%right conj
%right neg
%%

Expr : forall VAR dot Expr         { TForall $2 $4 }
     | exists VAR dot Expr         { TExists $2 $4 }
     | Form                        { $1 }

Form : Form equiv Form             { TEquiv $1 $3 }
     | Form impl Form              { TImpl $1 $3 }
     | Form disj Form              { TDisj $1 $3 }
     | Form conj Form              { TConj $1 $3 }
     | Atom                        { $1 }

Atom : true                        { TTrue }
     | false                       { TFalse }
     | VAR                         { TVar $1 }
     | neg Atom                    { TNeg $2 }
     | '(' Expr ')'                { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Term
parseExpr = expr . scanTokens
}
