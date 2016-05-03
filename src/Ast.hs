{-# OPTIONS_GHC -Wall #-}

module Ast where

import qualified Text.PrettyPrint as PP

type Name = String

data Term = TTrue
          | TFalse
          | TVar Name
          | TNeg Term
          | TConj Term Term
          | TDisj Term Term
          | TImpl Term Term
          | TEquiv Term Term -- if and only if
          | TForall Name Term
          | TExists Name Term
          deriving (Eq)

infix 1 |>

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

parens :: String -> String
parens s = "(" ++ s ++ ")"

instance Show Term where
  showsPrec _ x = shows $ PP.text $ prType x

prType :: Term -> String
prType TTrue = "TRUE"
prType TFalse = "FALSE"
prType (TVar n) = n
prType (TNeg t) = "¬" ++ prType t |> parens
prType (TConj t1 t2) = prType t1 ++ " ∧ " ++ prType t2 |> parens
prType (TDisj t1 t2) = prType t1 ++ " ∨ " ++ prType t2 |> parens
prType (TImpl t1 t2) = prType t1 ++ " ⇒ " ++ prType t2 |> parens
prType (TEquiv t1 t2) = prType t1 ++ " ⇔ " ++ prType t2 |> parens
prType (TForall n t) = "∀" ++ n ++ ". " ++ prType t |> parens
prType (TExists n t) = "∃" ++ n ++ ". " ++ prType t |> parens
