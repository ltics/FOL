{
module Lexer where
}

%wrapper "basic"

$letter = [a-zA-Z]
$digit  = 0-9
$eol    = [\n]

tokens :-
       $eol         ;
       $white+		;
       "#".*        ; --comments
       "⊤"			{ \_ -> TRUE }
       "⊥"          { \_ -> FALSE }
       "¬"          { \_ -> NEG }
       "∧"          { \_ -> CONJ }
       "∨"          { \_ -> DISJ }
       "⇒"          { \_ -> IMPL }
       "≡"          { \_ -> EQUIV }
       "∀"          { \_ -> FORALL }
       "∃"          { \_ -> EXISTS }
       "."          { \_ -> DOT }
       "("          { \_ -> LPAREN }
       ")"          { \_ -> RPAREN }

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
           deriving(Eq, Show)
}
