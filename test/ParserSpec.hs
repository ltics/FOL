module ParserSpec where

import Ast
import Parser
import qualified Text.PrettyPrint as PP
import Test.Hspec

spec :: Spec
spec = do
  describe "parser test" $
    it "should parse expressions" $ do
      let expr1 = "∀x. ∃y. x ∨ y"
      let expr2 = "∀x. ∃y. ∃z. z ∧ ¬ x ∨ y"
      let expr3 = "∀x. ∃y. ∃z. z ∧ ¬ (x ∨ y)"
      let term1 = parseExpr expr1
      let term2 = parseExpr expr2
      let term3 = parseExpr expr3
      term1 `shouldBe` (TForall "x" $ TExists "y" $ TDisj (TVar "x") (TVar "y"))
      term2 `shouldBe` (TForall "x" $ TExists "y" $ TExists "z" $ TDisj (TConj (TVar "z") (TNeg (TVar "x"))) $ TVar "y")
      term3 `shouldBe` (TForall "x" $ TExists "y" $ TExists "z" $ TConj (TVar "z") $ TNeg $ TDisj (TVar "x") $ TVar "y")
      (PP.text . show $ term1) `shouldBe` PP.text "(∀x. (∃y. (x ∨ y)))"
      (PP.text . show $ term2) `shouldBe` PP.text "(∀x. (∃y. (∃z. ((z ∧ (¬x)) ∨ y))))"
      (PP.text . show $ term3) `shouldBe` PP.text "(∀x. (∃y. (∃z. (z ∧ (¬(x ∨ y))))))"
