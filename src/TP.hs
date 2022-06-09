module TP where
import Text.Show.Functions

----------------------
-- Código inicial
----------------------

ordenarPor :: Ord a => (b -> a) -> [b] -> [b]
ordenarPor ponderacion =
  foldl (\ordenada elemento -> filter ((< ponderacion elemento).ponderacion) ordenada
                                  ++ [elemento] ++ filter ((>= ponderacion elemento).ponderacion) ordenada) []

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

--me devuelve una lista con TODOS LOS ELEMENTOS INICIALES PERO CON EFECTOS APLICADOS SOLO A LOS QUE CUMPLEN EL CRITERIO
----------------------------------------------------------------------
-- Definir los tipos de datos y funciones para el TP a partir de acá
----------------------------------------------------------------------

data Auto =  Auto{

  color::String,
  velocidad::Int,
  distancia::Int

}deriving (Show,Eq)

data Carrera = Carrera{

  autos::[Auto]

}deriving (Show,Eq)

{--------------------PARTE 1--------------------}

estaCerca::Auto->Auto->Bool
estaCerca auto1 auto2 = (distintoDe auto1 auto2) && (distanciaEntre auto1 auto2 < 10)

--extra 1
distintoDe::Eq a => a->a->Bool
distintoDe elemento1 = not.(elemento1 ==)

distanciaEntre::Auto->Auto->Int
distanciaEntre auto1 = abs.(distancia auto1-).distancia
---------

vaTranquilo::Auto->Carrera->Bool
vaTranquilo elAuto carrera = (all (not . estaCerca elAuto) (autos carrera)) && (all (vaGanando elAuto) (todosMenosEse elAuto (autos carrera)))

--extra 2
todosMenosEse :: Eq a => a -> [a] -> [a]
todosMenosEse este = filter (distintoDe este)

vaGanando::Auto->Auto->Bool
vaGanando auto1= (distancia auto1>).distancia
---------

puesto::Auto->Carrera->Int
--puesto elAuto carrera = 1 + length (autos carrera) - length (filter (vaGanando elAuto) (autos carrera))
--puesto elAuto carrera = length (filter (not.(vaGanando elAuto)) (autos carrera))
puesto elAuto = (+1).length . filter (flip vaGanando elAuto) . autos 
{-puesto elAuto carrera = 1 + length (filter (not.(vaGanando elAuto)) (autos carrera))
no sirve-}
{-SOLUCION: cantidad total de autos - cantidad de autoas a los elAuto les va ganando-}

{-----------------------------------------------}

{--------------------PARTE 2--------------------}

correrDurante::Int->Auto->Auto
correrDurante tiempo unAuto = unAuto {distancia = distancia unAuto + tiempo*(velocidad unAuto)} 

--alterarVelocidad::(Int->Int->Int)->Int->Auto->Auto
--alterarVelocidad funcion cantidad unAuto = unAuto {velocidad = funcion (velocidad unAuto) cantidad}

alterarVelocidad::(Int->Int) ->Auto->Auto
alterarVelocidad modificador unAuto = unAuto {velocidad = max 0 (modificador (velocidad unAuto))}

--bajarVelocidad::Int->Int->Int
--bajarVelocidad velocidadActual cantidad 
 -- |velocidadActual>cantidad = velocidadActual - cantidad
--  |otherwise = 0

bajarVelocidad::Int -> Auto -> Auto
bajarVelocidad valor = alterarVelocidad ((-)valor)

{-----------------------------------------------}

{--------------------PARTE 3--------------------}

type PowerUp = Auto->Carrera->[Auto]

aplicarPowerUp::PowerUp->Auto->Carrera->Carrera
aplicarPowerUp funcionPowerUp autoTrigger carrera = Carrera (funcionPowerUp autoTrigger carrera)

terremoto::PowerUp
terremoto autoTrigger = afectarALosQueCumplen (estaCerca autoTrigger) (bajarVelocidad 50).autos

miguelitos::Int->PowerUp
miguelitos cantidadABajar autoTrigger = (afectarALosQueCumplen (vaGanando autoTrigger)
                                         (bajarVelocidad cantidadABajar)).autos 

{--jetpack::Tiempo->PowerUp
jetpack tiempo autoTrigger carrera = afectarALosQueCumplen (==autoTrigger) 
                                    (alterarDistancia (bonusJetpack autoTrigger (velocidad autoTrigger) tiempo))
                                    (autos carrera)--}

jetpack::Tiempo->PowerUp
jetpack tiempo autoTrigger carrera = afectarALosQueCumplen (==autoTrigger) 
                                    (alterarVelocidad (`div` 2) . correrDurante tiempo . alterarVelocidad (*2))
                                    (autos carrera)
--extra 3                                 
type Velocidad=Int
type Tiempo = Int
type Distancia = Int
bonusJetpack::Auto->Velocidad->Tiempo->Distancia
bonusJetpack autoAfectado aumentoVelocidad tiempo = ((velocidad autoAfectado)+aumentoVelocidad)*tiempo

alterarDistancia::Distancia->Auto->Auto
alterarDistancia distanciaAgregada unAuto = unAuto {distancia = distancia unAuto + distanciaAgregada}
---------
{-----------------------------------------------}

{--------------------PARTE 4--------------------}
type Evento = Carrera -> Carrera
type Color = String

data InfoPuesto = InfoPuesto {

  posicion::Int,
  color_info::String

}deriving (Show,Eq)

type TablaDePosiciones = [InfoPuesto]


simularCarrera:: Carrera->[Evento]->TablaDePosiciones
simularCarrera carreraInicial = (ordenarPor posicion) . crearTabla . finalCarrera carreraInicial

------extra
finalCarrera :: Carrera->[Evento]->Carrera
finalCarrera carrera [] = carrera
finalCarrera carrera listaEventos = last listaEventos (finalCarrera carrera (init listaEventos))
{-partiendo del estado de carreraInicial (parámetro carrera), aplica los eventos de manera sucesiva 
llegando al estado de finalCarrera-}

crearTabla:: Carrera -> TablaDePosiciones
crearTabla carrera = map (infoPuestoSegunAuto carrera) (autos carrera)
{-recibe por parámetro el estado de una carrera y devuelve una lista de tipos de dato 
InfoPuesto (posicion y color de cada auto)-}

infoPuestoSegunAuto:: Carrera -> Auto -> InfoPuesto
infoPuestoSegunAuto carrera auto = InfoPuesto (puesto auto carrera) (color auto)
----------


correnTodos:: Int -> Evento
correnTodos tiempo = Carrera . map (correrDurante tiempo) . autos

{-///////////////////////////////////////////////////////////////////////////////-}
usaPowerUp:: PowerUp -> Color -> Evento
usaPowerUp power color_auto carrera = Carrera (flip power carrera (autoSegunColor color_auto carrera))

{-Ya existe la funcion aplicarPowerUp-}
{-aplicarPowerUp::PowerUp->Auto->Carrera->Carrera
aplicarPowerUp funcionPowerUp autoTrigger carrera = Carrera (funcionPowerUp autoTrigger carrera)-}

--segunda opcion:
usaPowerUp':: PowerUp -> Color -> Evento
usaPowerUp' power color_auto carrera = aplicarPowerUp power (autoSegunColor color_auto carrera) carrera

{-///////////////////////////////////////////////////////////////////////////////-}


-----extra 2
autoSegunColor:: Color -> Carrera -> Auto
autoSegunColor color_auto = head.filter ((color_auto == ). color) . autos
-----------
{-----------------------------------------------}


{--------------------PARTE 5--------------------}
--misilTeledirigido::Color->PowerUp
--misilTeledirigido colorAuto autoTrigger carrera
--  | velocidad (autoSegunColor colorAuto carrera) < 50 = golpeMisil (autoSegunColor colorAuto carrera) autoTrigger .
--                                                        afectarALosQueCumplen (==autoSegunColor colorAuto carrera) 
--                                                        (alterarVelocidad modificarVelocidad 10) . autos $ carrera
--  | otherwise = autos carrera
 
-----extra 
--modificarVelocidad::Int->Int->Int
--modificarVelocidad _ nuevaVelocidad = nuevaVelocidad

misilTeledirigido::Color->PowerUp
misilTeledirigido colorAuto autoTrigger carrera
  | velocidad (autoSegunColor colorAuto carrera) < 50 = golpeMisil (autoSegunColor colorAuto carrera) autoTrigger .
                                                        afectarALosQueCumplen (==autoSegunColor colorAuto carrera) 
                                                        (dejarVelocidadEn 10) . autos $ carrera
  | otherwise = autos carrera

dejarVelocidadEn :: Int -> Auto -> Auto
dejarVelocidadEn valor auto = auto {velocidad = valor}

{-Requiero de esta funcion con este tipado para que "encaje" con el tipado de la funcion alterarVelocidad.
El primer parámetro de "modificarVelocidad" lo descarto ya que la funcion "alterarVelocidad" me envía la velocidad actual del auto en cuestion.
Para esta funcion ese dato no es relevante, pero para otras sí, por eso no modifico "alterarVelocidad"-}

{----------otra opcion-----
misilTeledirigido'::Color->PowerUp
misilTeledirigido' colorAuto autoTrigger carrera
  | velocidad (autoSegunColor colorAuto carrera) < 50 = golpeMisil (autoSegunColor colorAuto carrera) autoTrigger .
                                                        afectarALosQueCumplen (==autoSegunColor colorAuto carrera) 
                                                        (alterarVelocidad modificarVelocidad 10) . autos $ carrera
  | otherwise = autos carrera
 -}
-----extra 
--llegar
--modificarVelocidad _ nuevaVelocidad = nuevaVelocidad

{----------otra opcion------}
----------
-----extra 2
golpeMisil::Auto->Auto->[Auto]->[Auto]
golpeMisil autoGolpeado autoTrigger autosCarrera
 |vaGanando autoGolpeado autoTrigger = afectarALosQueCumplen ((==color autoGolpeado).color) (alterarDistancia 5) autosCarrera
 |otherwise = autosCarrera

{-Incrementa distancia del autoGolpeado sólo si este le va ganando al que le envió el misil-}
------------
{-----------------------------------------------}

{--------------------PRUEBAS--------------------}
-----para las pruebas
autoA = Auto "rojo" 120 20
autoB= Auto "blanco" 120 15
autoC = Auto "azul" 120 35
autoD = Auto "negro" 120 50
carrera1 = Carrera [autoA, autoB, autoC, autoD]

{-simularCarrera carrera1 [correnTodos 30, usaPowerUp (jetpack 3) "azul", usaPowerUp terremoto "blanco", correnTodos 40,usaPowerUp (miguelitos 20) "blanco", usaPowerUp (jetpack 6) "negro", correnTodos 10] -}
--prueba con funcion usaPowerUp'
{-simularCarrera carrera1 [correnTodos 30, usaPowerUp' (jetpack 3) "azul", usaPowerUp' terremoto "blanco", correnTodos 40,usaPowerUp' (miguelitos 20) "blanco", usaPowerUp' (jetpack 6) "negro", correnTodos 10]-}

--PRUEBA powerUp: misilTeledirigido

carrera2 = Carrera [autoA, autoB, autoC, autoD, autoE]
autoE = Auto "naranja" 30 20
--aplicarPowerUp (misilTeledirigido "naranja") autoA carrera2
{-Retornó el siguiente resultado:
Carrera {autos = [Auto {color = "naranja", velocidad = 10, distancia = 12},Auto {color = "rojo", velocidad = 120, distancia = 0},
Auto {color = "blanco", velocidad = 120, distancia = 0},Auto {color = "azul", velocidad = 120, distancia = 0},
Auto {color = "negro", velocidad = 120, distancia = 0}]}-}


carrera3 = Carrera [autoE, autoF]
autoF = Auto "violeta" 45 15
--aplicarPowerUp (misilTeledirigido "naranja") autoF carrera3
{-Retornó el siguiente resultado:
Carrera {autos = [Auto {color = "naranja", velocidad = 10, distancia = 25},Auto {color = "violeta", velocidad = 45, distancia = 15}]}
-}

{-----------------------------------------------}

{-------------------RESPUESTA-------------------}

{-a. ¿La solución desarrollada hasta este punto permite agregar el nuevo power up o sería necesario
cambiar algo de lo desarrollado en los puntos anteriores? Justificar.

Se pudo implementar un nuevo power up sin necesidad de modificar la solucion desarrollada hasta ese punto.
Se adaptaron las nuevas funciones a la estructura ya establecida como en el caso de uso de la funcion "alterarVelocidad" 

-}

{-----------------------------------------------}