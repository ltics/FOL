{-# OPTIONS_GHC -Wall #-}

module Core where

import Ast

-- use closure to mock envrionment

empty :: Show a => a -> t
empty x = error $ "Undefined variable: " ++ show x

extend :: Eq a => (a -> t) -> a -> t -> a -> t
extend f k v = \x -> if x == k then v else f x

eval :: (Name -> Bool) -> Term -> Bool
eval env term = case term of
                  TTrue -> True
                  TFalse -> False
                  TVar n -> env n
                  TNeg t -> not $ eval env t
                  TConj t1 t2 -> (eval env t1) && (eval env t2)
                  TDisj t1 t2 -> (eval env t1) || (eval env t2)
                  TImpl t1 t2 -> not (eval env t1) || (eval env t2)
                  -- (t1 ⇒ t2) ∧ (t2 ⇒ t1)
                  TEquiv t1 t2 -> (not (eval env t1) || (eval env t2)) && (not (eval env t2) || (eval env t1))
                  TForall n t -> e1 && e2 where
                    e1 = eval (extend env n True) t
                    e2 = eval (extend env n False) t
                  TExists n t -> e1 || e2 where
                    e1 = eval (extend env n True) t
                    e2 = eval (extend env n False) t

toplevel :: Term -> Bool
toplevel = eval empty
