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
vaTranquilo elAuto carrera = (all (estaCerca elAuto) (autos carrera)) && (all (vaGanando elAuto) (autos carrera))

--extra 2
vaGanando::Auto->Auto->Bool
vaGanando auto1 = (distancia auto1>).distancia
---------

puesto::Auto->Carrera->Int
--puesto elAuto carrera = 1 + length (autos carrera) - length (filter (vaGanando elAuto) (autos carrera))
puesto elAuto carrera = length (filter (not.(vaGanando elAuto)) (autos carrera))
{-puesto elAuto carrera = 1 + length (filter (not.(vaGanando elAuto)) (autos carrera))
no sirve-}
{-SOLUCION: cantidad total de autos - cantidad de autoas a los elAuto les va ganando-}

{-----------------------------------------------}

{--------------------PARTE 2--------------------}

correrDurante::Int->Auto->Auto
correrDurante tiempo unAuto = unAuto {distancia = distancia unAuto + tiempo*(velocidad unAuto)} 

alterarVelocidad::(Int->Int->Int)->Int->Auto->Auto
alterarVelocidad funcion cantidad unAuto = unAuto {velocidad = funcion (velocidad unAuto) cantidad}

bajarVelocidad::Int->Int->Int
bajarVelocidad velocidadActual cantidad 
  |velocidadActual>cantidad = velocidadActual - cantidad
  |otherwise = 0
{-----------------------------------------------}

{--------------------PARTE 3--------------------}

type PowerUp = Auto->Carrera->[Auto]

aplicarPowerUp::PowerUp->Auto->Carrera->Carrera
aplicarPowerUp funcionPowerUp autoTrigger carrera = Carrera (funcionPowerUp autoTrigger carrera)

terremoto::PowerUp
terremoto autoTrigger = afectarALosQueCumplen (estaCerca autoTrigger) (alterarVelocidad (bajarVelocidad) 50).autos

miguelitos::Int->PowerUp
miguelitos cantidadABajar autoTrigger = (afectarALosQueCumplen (vaGanando autoTrigger)
                                         (alterarVelocidad bajarVelocidad cantidadABajar)).autos 

jetpack::Tiempo->PowerUp
jetpack tiempo autoTrigger carrera = afectarALosQueCumplen (==autoTrigger) 
                                    (alterarDistancia (bonusJetpack autoTrigger (velocidad autoTrigger) tiempo))
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
simularCarrera carrera = (ordenarPor posicion) . crearTabla . finalCarrera carrera

------extra
finalCarrera :: Carrera->[Evento]->Carrera
finalCarrera carrera [] = carrera
finalCarrera carrera lista = last lista (finalCarrera carrera (init lista))

crearTabla:: Carrera -> TablaDePosiciones
crearTabla carrera = map (infoPuestoSegunAuto carrera) (autos carrera)

infoPuestoSegunAuto:: Carrera -> Auto -> InfoPuesto
infoPuestoSegunAuto carrera auto = InfoPuesto (puesto auto carrera) (color auto)
----------


correnTodos:: Int -> Evento
correnTodos tiempo = Carrera . map (correrDurante tiempo) . autos

usaPowerUp:: PowerUp -> Color -> Evento
usaPowerUp power color_auto carrera = Carrera (flip power carrera (autoSegunColor color_auto carrera))

-----extra 2
autoSegunColor:: Color -> Carrera -> Auto
autoSegunColor color_auto = head.filter ((color_auto == ). color) . autos
-----------

-----para las pruebas
autoA = Auto "rojo" 120 0
autoB= Auto "blanco" 120 0
autoC = Auto "azul" 120 0
autoD = Auto "negro" 120 0
carrera1 = Carrera [autoA, autoB, autoC, autoD]

--simularCarrera carrera1 [correnTodos 30, usaPowerUp (jetpack 3) "azul", usaPowerUp terremoto "blanco", correnTodos 40,
--usaPowerUp (miguelitos 20) "blanco", usaPowerUp (jetpack 6) "negro", correnTodos 10] 
