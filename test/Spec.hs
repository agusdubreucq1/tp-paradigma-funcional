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
  describe "punto 2" $ do
    it "El autoAzul corre durante 30' " $ do
      correrDurante 30 autoAzul `shouldBe` Auto {color = "Azul", velocidad = 120, distancia = 3620}
    it "El autoBlanco baja la velocidad 40 km/h" $ do
      bajarVelocidad 40 autoBlanco `shouldBe` Auto {color = "Blanco", velocidad = 80, distancia = 45}
    it "El autoAzul sube la velocidad 10 km/h" $ do
      alterarVelocidad (+10) autoAzul `shouldBe` Auto {color = "Azul", velocidad = 130, distancia = 20}
  describe "punto 3" $ do
    it "El autoRojo aplica el powerUp terremoto en la carreraUno" $ do
      aplicarPowerUp terremoto autoRojo carreraUno `shouldBe` Carrera {autos = [Auto 
                                                              {color = "Blanco", velocidad = 70, distancia = 45},Auto 
                                                              {color = "Rojo", velocidad = 120, distancia = 50},Auto 
                                                              {color = "Azul", velocidad = 120, distancia = 20},Auto 
                                                              {color = "Negro", velocidad = 120, distancia = 25}]}
    it "El autoNegro aplica el powerUp jetpack por 30' en la carreraUno" $ do
      aplicarPowerUp (jetpack 30) autoNegro carreraUno `shouldBe` Carrera {autos = [Auto {color = "Negro", velocidad = 120, distancia = 7225},Auto 
                                                                                        {color = "Rojo", velocidad = 120, distancia = 50},Auto 
                                                                                        {color = "Blanco", velocidad = 120, distancia = 45},Auto 
                                                                                        {color = "Azul", velocidad = 120, distancia = 20}]}
    it "El autoNegro aplica el powerUp miguelitos en 40 en la carreraUno" $ do
      aplicarPowerUp (miguelitos 40) autoNegro carreraUno `shouldBe` Carrera {autos = [Auto {color = "Azul", velocidad = 80, distancia = 20},Auto 
                                                                                            {color = "Rojo", velocidad = 120, distancia = 50},Auto 
                                                                                            {color = "Blanco", velocidad = 120, distancia = 45},Auto 
                                                                                            {color = "Negro", velocidad = 120, distancia = 25}]}

