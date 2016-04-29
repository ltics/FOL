module LexerSpec where

import Lexer
import Test.Hspec

spec :: Spec
spec = do
    describe "tokenize test" $ do
      it "should tokenize single token" $ do
        scanTokens "⊤" `shouldBe` [TRUE]
        scanTokens "⊥" `shouldBe` [FALSE]
        scanTokens "¬" `shouldBe` [NEG]
        scanTokens "∧" `shouldBe` [CONJ]
        scanTokens "∨" `shouldBe` [DISJ]
        scanTokens "⇒" `shouldBe` [IMPL]
        scanTokens "≡" `shouldBe` [EQUIV]
        scanTokens "∀" `shouldBe` [FORALL]
        scanTokens "∃" `shouldBe` [EXISTS]
        scanTokens "." `shouldBe` [DOT]
        scanTokens "(" `shouldBe` [LPAREN]
        scanTokens ")" `shouldBe` [RPAREN]
      it "should tokenize series tokens" $ do
        scanTokens "∀x. ∃y. x ∨ y" `shouldBe` [FORALL, VAR "x", DOT, EXISTS, VAR "y", DOT, VAR "x", DISJ, VAR "y"]
