import Test.Hspec
import TP

autoRojo = Auto "Rojo" 120 50
autoBlanco= Auto "Blanco" 120 45
autoAzul = Auto "Azul" 120 20
autoNegro = Auto "Negro" 120 25

carreraUno = Carrera [autoRojo, autoBlanco, autoAzul, autoNegro]

autoROJO = Auto "Rojo" 40 50
autoBLANCO= Auto "Blanco" 120 45
autoAZUL = Auto "Azul" 120 20
autoNEGRO = Auto "Negro" 120 25

carreraDos = Carrera [autoROJO, autoBLANCO, autoAZUL, autoNEGRO]

autoA = Auto "Rojo" 120 20
autoB= Auto "Blanco" 120 15
autoC = Auto "Azul" 120 35
autoD = Auto "Negro" 120 50
carrera1 = Carrera [autoA, autoB, autoC, autoD]

autoAA = Auto "Rojo" 120 0
autoBB= Auto "Blanco" 120 0
autoCC = Auto "Azul" 120 0
autoDD = Auto "Negro" 120 0
carreraSimulacion = Carrera [autoAA, autoBB, autoCC, autoDD]

autoA2 = Auto "Rojo" 45 20
autoB2= Auto "Blanco" 120 15
autoC2 = Auto "Azul" 50 35
autoD2 = Auto "Negro" 120 50
carrera2 = Carrera [autoA2, autoB2, autoC2, autoD2]


main :: IO ()
main = hspec $ do
 describe "TP integrador - Punto 1" $ do
    it "El autoRojo esta cerca del autoBlanco" $ do
      estaCerca autoRojo autoBlanco `shouldBe` True
    it "El autoRojo va tranquilo" $ do
      vaTranquilo' autoRojo carreraUno `shouldBe` False
    it "El autoNegro va en el Puesto 3" $ do
      puesto' autoNegro carreraUno `shouldBe` 3
 describe "punto 2" $ do
    it "El autoAzul corre durante 30' " $ do
      correrDurante 30 autoAzul `shouldBe` Auto {color = "Azul", velocidad = 120, distancia = 3620}
    it "El autoBlanco baja la velocidad 40 km/h" $ do
      bajarVelocidad 40 autoBlanco `shouldBe` Auto {color = "Blanco", velocidad = 80, distancia = 45}
    it "El autoAzul sube la velocidad 10 km/h" $ do
      alterarVelocidad (+10) autoAzul `shouldBe` Auto {color = "Azul", velocidad = 130, distancia = 20}
 describe "punto 3" $ do
    it "El autoRojo aplica el powerUp terremoto en la carreraUno" $ do
      usaPowerUp terremoto "Rojo" carreraUno `shouldBe` Carrera {autos = [Auto 
                                                              {color = "Blanco", velocidad = 70, distancia = 45},Auto 
                                                              {color = "Rojo", velocidad = 120, distancia = 50},Auto 
                                                              {color = "Azul", velocidad = 120, distancia = 20},Auto 
                                                              {color = "Negro", velocidad = 120, distancia = 25}]}
    it "El autoNegro aplica el powerUp jetpack por 30' en la carreraUno" $ do
      usaPowerUp (jetpack 30) "Negro" carreraUno `shouldBe` Carrera {autos = [Auto {color = "Negro", velocidad = 120, distancia = 7225},Auto 
                                                                                        {color = "Rojo", velocidad = 120, distancia = 50},Auto 
                                                                                        {color = "Blanco", velocidad = 120, distancia = 45},Auto 
                                                                                        {color = "Azul", velocidad = 120, distancia = 20}]}
    it "El autoNegro aplica el powerUp miguelitos en 40 en la carreraUno" $ do
      usaPowerUp (miguelitos 40) "Negro" carreraUno `shouldBe` Carrera {autos = [Auto {color = "Azul", velocidad = 80, distancia = 20},Auto 
                                                                                            {color = "Rojo", velocidad = 120, distancia = 50},Auto 
                                                                                            {color = "Blanco", velocidad = 120, distancia = 45},Auto 
                                                                                            {color = "Negro", velocidad = 120, distancia = 25}]}
 describe "TP integrador - Punto 4" $ do
    it "simularCarrera con funcion usaPowerUp" $ do
      simularCarrera carreraSimulacion  [correnTodos 30, usaPowerUp (jetpack 3) "Azul", 
       usaPowerUp terremoto "Blanco", 
       correnTodos 40,usaPowerUp (miguelitos 20) "Blanco", 
       usaPowerUp (jetpack 6) "Negro", correnTodos 10] `shouldBe` [InfoPuesto{posicion=1,color_info="Azul"},
                                                                   InfoPuesto{posicion=2,color_info="Blanco"},
                                                                   InfoPuesto{posicion=3,color_info="Negro"},
                                                                   InfoPuesto{posicion=4,color_info="Rojo"}]
    it "simularCarrera con funcion usaPowerUp'" $ do
      simularCarrera carreraSimulacion [correnTodos 30, usaPowerUp (jetpack 3) "Azul", 
       usaPowerUp terremoto "Blanco", 
       correnTodos 40,usaPowerUp (miguelitos 20) "Blanco", 
       usaPowerUp (jetpack 6) "Negro", correnTodos 10] `shouldBe` [InfoPuesto{posicion=1,color_info="Azul"},
                                                                   InfoPuesto{posicion=2,color_info="Blanco"},
                                                                   InfoPuesto{posicion=3,color_info="Negro"},
                                                                   InfoPuesto{posicion=4,color_info="Rojo"}]
    it "Corren todos 20 segundos en la carrera1" $ do
       correnTodos 20 carrera1 `shouldBe` Carrera{autos=[Auto "Rojo" 120 2420, Auto "Blanco" 120 2415,
                                                         Auto "Azul" 120 2435, Auto "Negro" 120 2450]}
    it "Auto rojo usa jetpack 5 en carrera1" $ do
        usaPowerUp (jetpack 5) "Rojo" carrera1 `shouldBe` Carrera{autos=[Auto "Rojo" 120 1220, Auto "Blanco" 120 15, 
                                                                         Auto "Azul" 120 35, Auto "Negro" 120 50]}
    it "Auto Negro usa miguelitos 20 en carrera1" $ do
        usaPowerUp (miguelitos 20) "Negro" carrera1 `shouldBe` Carrera{autos=[Auto "Rojo" 100 20, Auto "Blanco" 100 15,  
                                                                              Auto "Azul" 100 35, Auto "Negro" 120 50]}
    it "Auto blanco usa terremoto en carrera1" $ do
        usaPowerUp terremoto "Blanco" carrera1 `shouldBe` Carrera{autos=[Auto "Rojo" 70 20, Auto "Blanco" 120 15,  
                                                                         Auto "Azul" 120 35, Auto "Negro" 120 50]}
 describe "TP integrador - Punto 5" $ do
    it "misilTeledirigido a un auto que va ganando y su velocidad es < a 50" $ do
        usaPowerUp (misilTeledirigido "Rojo") "Blanco" carrera2 `shouldBe` Carrera{autos=[Auto "Rojo" 10 25,
                                                                         Auto "Blanco" 120 15,Auto "Azul" 50 35,Auto "Negro" 120 50]} 
    it "misilTeledirigido a un auto que va ganando y su velocidad es > a 50" $ do
        usaPowerUp (misilTeledirigido "Negro") "Rojo" carrera2 `shouldBe` Carrera{autos=[Auto "Negro" 120 55,Auto "Rojo" 45 20,
                                                                         Auto "Blanco" 120 15,Auto "Azul" 50 35]}
    it "misilTeledirigido a un auto que va perdiendo y su velocidad es = 50 " $ do
        usaPowerUp (misilTeledirigido "Azul") "Negro" carrera2 `shouldBe` Carrera{autos=[Auto "Rojo" 45 20,
                                                                          Auto "Blanco" 120 15,Auto "Azul" 50 35,Auto "Negro" 120 50]}
 describe "punto 5" $ do
    it "El autoAZUL usa misilTeledirigido para impactar el autoROJO en la carreraDos" $ do
      usaPowerUp (misilTeledirigido "Rojo") "Azul" carreraDos `shouldBe` Carrera {autos = [Auto 
                                                              {color = "Rojo", velocidad = 10, distancia = 55},Auto 
                                                              {color = "Blanco", velocidad = 120, distancia = 45},Auto 
                                                              {color = "Azul", velocidad = 120, distancia = 20},Auto 
                                                              {color = "Negro", velocidad = 120, distancia = 25}]}

    it "El autoNEGRO usa misilTeledirigido para impactar el autoAZUL en la carreraDos, y esto no produce ningun cambio" $ do
      usaPowerUp (misilTeledirigido "Azul") "Negro" carreraDos `shouldBe` Carrera {autos = [Auto 
                                                              {color = "Rojo", velocidad = 40, distancia = 50},Auto 
                                                              {color = "Blanco", velocidad = 120, distancia = 45},Auto 
                                                              {color = "Azul", velocidad = 120, distancia = 20},Auto 
                                                              {color = "Negro", velocidad = 120, distancia = 25}]}
        




