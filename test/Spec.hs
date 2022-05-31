import Test.Hspec
import TP

autoRojo = Auto "Rojo" 120 50
autoBlanco= Auto "Blanco" 120 45
autoAzul = Auto "Azul" 120 20
autoNegro = Auto "Negro" 120 25

carreraUno = Carrera [autoRojo, autoBlanco, autoAzul, autoNegro]

main :: IO ()
main = hspec $ do
  describe "TP integrador - Punto 1" $ do
    it "El autoRojo esta cerca del autoBlanco" $ do
      estaCerca autoRojo autoBlanco `shouldBe` True
    it "El autoRojo va tranquilo" $ do
      vaTranquilo autoRojo carreraUno `shouldBe` False
    it "El autoNegro va en el Puesto 3" $ do
      puesto autoNegro carreraUno `shouldBe` 3