module ParserSpec where

import Ast
import Parser
import qualified Text.PrettyPrint as PP
import Test.Hspec

spec :: Spec
spec = do
  describe "parser test" $
    it "should parse expressions" $ do
      let term1 = parseExpr "∀x. ∃y. x ∨ y"
      let term2 = parseExpr "∀x. ∃y. ∃z. z ∧ ¬ x ∨ y"
      let term3 = parseExpr "∀x. ∃y. ∃z. z ∧ ¬ (x ∨ y)"
      -- right association with same precedence
      let term4 = parseExpr "p ∧ q ∧ r"
      let term5 = parseExpr "p ⇒ q ⇒ r"
      let term6 = parseExpr "p ⇒ q ⇔ r ∧ s ∨ (t ⇔ ¬ ¬ u ∧ v)"
      let term7 = parseExpr "p ⇒ q ⇔ r ∧ s ∨ (t ⇔ ¬ (¬ u ∧ v))"
      term1 `shouldBe` (TForall "x" $ TExists "y" $ TDisj (TVar "x") (TVar "y"))
      term2 `shouldBe` (TForall "x" $ TExists "y" $ TExists "z" $ TDisj (TConj (TVar "z") (TNeg (TVar "x"))) $ TVar "y")
      term3 `shouldBe` (TForall "x" $ TExists "y" $ TExists "z" $ TConj (TVar "z") $ TNeg $ TDisj (TVar "x") $ TVar "y")
      term4 `shouldBe` (TConj (TVar "p") (TConj (TVar "q") $ TVar "r"))
      term5 `shouldBe` (TImpl (TVar "p") (TImpl (TVar "q") $ TVar "r"))
      term6 `shouldBe` (TEquiv (TImpl (TVar "p") $ TVar "q") $ TDisj (TConj (TVar "r") $ TVar "s") $ TEquiv (TVar "t") $ TConj (TNeg $ TNeg $ TVar "u") $ TVar "v")
      term7 `shouldBe` (TEquiv (TImpl (TVar "p") $ TVar "q") $ TDisj (TConj (TVar "r") $ TVar "s") $ TEquiv (TVar "t") $ TNeg $ TConj (TNeg $ TVar "u") $ TVar "v")
      (PP.text . show $ term1) `shouldBe` PP.text "(∀x. (∃y. (x ∨ y)))"
      (PP.text . show $ term2) `shouldBe` PP.text "(∀x. (∃y. (∃z. ((z ∧ (¬x)) ∨ y))))"
      (PP.text . show $ term3) `shouldBe` PP.text "(∀x. (∃y. (∃z. (z ∧ (¬(x ∨ y))))))"
      (PP.text . show $ term4) `shouldBe` PP.text "(p ∧ (q ∧ r))"
      (PP.text . show $ term5) `shouldBe` PP.text "(p ⇒ (q ⇒ r))"
      (PP.text . show $ term6) `shouldBe` PP.text "((p ⇒ q) ⇔ ((r ∧ s) ∨ (t ⇔ ((¬(¬u)) ∧ v))))"
      (PP.text . show $ term7) `shouldBe` PP.text "((p ⇒ q) ⇔ ((r ∧ s) ∨ (t ⇔ (¬((¬u) ∧ v)))))"
