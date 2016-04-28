module LexerSpec where

import Lexer
import Test.Hspec

spec :: Spec
spec = do
    describe "tokenize test" $ do
      it "should tokenize single token" $ do
        alexScanTokens "⊤" `shouldBe` [TRUE]
        alexScanTokens "⊥" `shouldBe` [FALSE]
        alexScanTokens "¬" `shouldBe` [NEG]
        alexScanTokens "∧" `shouldBe` [CONJ]
        alexScanTokens "∨" `shouldBe` [DISJ]
        alexScanTokens "⇒" `shouldBe` [IMPL]
        alexScanTokens "≡" `shouldBe` [EQUIV]
        alexScanTokens "∀" `shouldBe` [FORALL]
        alexScanTokens "∃" `shouldBe` [EXISTS]
        alexScanTokens "." `shouldBe` [DOT]
        alexScanTokens "(" `shouldBe` [LPAREN]
        alexScanTokens ")" `shouldBe` [RPAREN]
      it "should tokenize series tokens" $ do
        alexScanTokens "∀x. ∃y. x ∨ y" `shouldBe` [FORALL, VAR "x", DOT, EXISTS, VAR "y", DOT, VAR "x", DISJ, VAR "y"]
