import Test.Hspec
import TP

autoRojo = Auto "Rojo" 120 50
autoBlanco= Auto "Blanco" 120 45
autoAzul = Auto "Azul" 120 20
autoNegro = Auto "Negro" 120 25

carreraUno = Carrera [autoRojo, autoBlanco, autoAzul, autoNegro]

autoA = Auto "rojo" 120 20
autoB= Auto "blanco" 120 15
autoC = Auto "azul" 120 35
autoD = Auto "negro" 120 50
carrera1 = Carrera [autoA, autoB, autoC, autoD]

autoAA = Auto "rojo" 120 0
autoBB= Auto "blanco" 120 0
autoCC = Auto "azul" 120 0
autoDD = Auto "negro" 120 0
carreraSimulacion = Carrera [autoAA, autoBB, autoCC, autoDD]


main :: IO ()
main = hspec $ do
  describe "TP integrador - Punto 1" $ do
    it "El autoRojo esta cerca del autoBlanco" $ do
      estaCerca autoRojo autoBlanco `shouldBe` True
    it "El autoRojo va tranquilo" $ do
      vaTranquilo autoRojo carreraUno `shouldBe` False
    it "El autoNegro va en el Puesto 3" $ do
      puesto autoNegro carreraUno `shouldBe` 37
  describe "TP integrador - Punto 4" $ do
    it "simularCarrera con funcion usaPowerUp" $ do
      simularCarrera carreraSimulacion  [correnTodos 30, usaPowerUp (jetpack 3) "azul", 
       usaPowerUp terremoto "blanco", 
       correnTodos 40,usaPowerUp (miguelitos 20) "blanco", 
       usaPowerUp (jetpack 6) "negro", correnTodos 10] `shouldBe` [InfoPuesto{posicion=1,color_info="azul"},
                                                                   InfoPuesto{posicion=2,color_info="blanco"},
                                                                   InfoPuesto{posicion=3,color_info="negro"},
                                                                   InfoPuesto{posicion=4,color_info="rojo"}]
    it "simularCarrera con funcion usaPowerUp'" $ do
      simularCarrera carreraSimulacion [correnTodos 30, usaPowerUp' (jetpack 3) "azul", 
       usaPowerUp' terremoto "blanco", 
       correnTodos 40,usaPowerUp' (miguelitos 20) "blanco", 
       usaPowerUp' (jetpack 6) "negro", correnTodos 10] `shouldBe` [InfoPuesto{posicion=1,color_info="azul"},
                                                                   InfoPuesto{posicion=2,color_info="blanco"},
                                                                   InfoPuesto{posicion=3,color_info="negro"},
                                                                   InfoPuesto{posicion=4,color_info="rojo"}]
    it "Corren todos 20 segundos en la carrera1" $ do
       correnTodos 20 carrera1 `shouldBe` Carrera{autos=[Auto "rojo" 120 2420, Auto "blanco" 120 2415,
                                                         Auto "azul" 120 2435, Auto "negro" 120 2450]}
    it "Auto rojo usa jetpack 5 en carrera1" $ do
        usaPowerUp (jetpack 5) "rojo" carrera1 `shouldBe` Carrera{autos=[Auto "rojo" 120 1220, Auto "blanco" 120 15, 
                                                                         Auto "azul" 120 35, Auto "negro" 120 50]}
    it "Auto rojo usa jetpack' 5 en carrera1" $ do
        usaPowerUp (jetpack' 5) "rojo" carrera1 `shouldBe` Carrera{autos=[Auto "rojo" 120 1220, Auto "blanco" 120 15, 
                                                                          Auto "azul" 120 35, Auto "negro" 120 50]}
    it "Auto negro usa miguelitos 20 en carrera1" $ do
        usaPowerUp (miguelitos 20) "negro" carrera1 `shouldBe` Carrera{autos=[Auto "rojo" 100 20, Auto "blanco" 100 15,  
                                                                              Auto "azul" 100 35, Auto "negro" 120 50]}
    it "Auto blanco usa terremoto en carrera1" $ do
        usaPowerUp terremoto "blanco" carrera1 `shouldBe` Carrera{autos=[Auto "rojo" 70 20, Auto "blanco" 120 15,  
                                                                         Auto "azul" 120 35, Auto "negro" 120 50]
}











