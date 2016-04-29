module ParserSpec where

import Ast
import Parser
import qualified Text.PrettyPrint as PP
import Test.Hspec

spec :: Spec
spec = do
  describe "parser test" $
    it "should parse expressions" $ do
      let expr = "∀x. ∃y. x ∨ y"
      let term = parseExpr expr
      term `shouldBe` (TForall "x" $ TExists "y" $ TDisj (TVar "x") (TVar "y"))
      (PP.text . show $ term) `shouldBe` PP.text "(∀x. (∃y. (x ∨ y)))"
