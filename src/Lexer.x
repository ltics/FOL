{
module Lexer where
import Ast(Name)
}

%wrapper "basic"

$letter = [a-zA-Z]
$eol    = [\n]

tokens :-
       $eol         ;
       $white+      ;
       "#".*        ; --comments
       "⊤"          { \_ -> TRUE }
       "⊥"          { \_ -> FALSE }
       "¬"          { \_ -> NEG }
       "∧"          { \_ -> CONJ }
       "∨"          { \_ -> DISJ }
       "⇒"          { \_ -> IMPL }
       "↔"          { \_ -> EQUIV }
       "∀"          { \_ -> FORALL }
       "∃"          { \_ -> EXISTS }
       "."          { \_ -> DOT }
       "("          { \_ -> LPAREN }
       ")"          { \_ -> RPAREN }
       $letter+     { \s -> VAR s }

{
data Token = TRUE
           | FALSE
           | NEG
           | CONJ
           | DISJ
           | IMPL
           | EQUIV
           | FORALL
           | EXISTS
           | DOT
           | LPAREN
           | RPAREN
           | VAR Name
           deriving(Eq, Show)

scanTokens = alexScanTokens
}
