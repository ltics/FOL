module EvalSpec where

import Ast
import Parser
import Core
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
      let term4 = parseExpr "∃p. ∃q. ∃r. p ∧ q ∧ r"
      let term5 = parseExpr "∀p. ∃q. ∃r. p ⇒ q ⇒ r"
      let term6 = parseExpr "∀p. ∃q. ∃r. ∃s. ∃u. ∃v. ∃t. p ⇒ q ⇔ r ∧ s ∨ (t ⇔ ¬ ¬ u ∧ v)"
      let term7 = parseExpr "∀p. ∃q. ∃r. ∃s. ∃u. ∃v. ∃t. p ⇒ q ⇔ r ∧ s ∨ (t ⇔ ¬ (¬ u ∧ v))"
      let term8 = parseExpr "∀x. ∃y. x ⇔ y"
      -- only True ∧ True is True
      let term9 = parseExpr "∀x. ∃y. x ∧ y"
      let term10 = parseExpr "¬ ¬ ⊤ ∧ ⊤"
      let term11 = parseExpr "¬ (¬ ⊤ ∧ ⊤)"
      toplevel term1 `shouldBe` True
      toplevel term2 `shouldBe` True
      toplevel term3 `shouldBe` False
      toplevel term4 `shouldBe` True
      toplevel term5 `shouldBe` True
      toplevel term6 `shouldBe` True
      toplevel term7 `shouldBe` True
      toplevel term8 `shouldBe` True
      toplevel term9 `shouldBe` False
      toplevel term10 `shouldBe` True
      toplevel term11 `shouldBe` True
