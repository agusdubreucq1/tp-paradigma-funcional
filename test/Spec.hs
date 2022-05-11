import Test.Hspec
import TP

main :: IO ()
main = hspec $ do
  describe "TP integrador" $ do
    it "Template de test" $ do
      1 + 1 `shouldBe` 2