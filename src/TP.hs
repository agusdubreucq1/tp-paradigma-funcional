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
{--opciones nuevas--}
vaTranquilo'::Auto->Carrera->Bool
vaTranquilo' elAuto carrera = (all (not . estaCerca elAuto) (autos carrera)) && (all (vaGanando elAuto) (todosMenosEse elAuto (autos carrera)))
{--opciones nuevas--}

--extra 2
todosMenosEse :: Eq a => a -> [a] -> [a]
todosMenosEse este = filter (distintoDe este)

vaGanando::Auto->Auto->Bool
vaGanando auto1= (distancia auto1>).distancia
---------
{--opciones nuevas--}
puesto'::Auto->Carrera->Int
puesto' elAuto = (+1).length . filter (flip vaGanando elAuto) . autos 
{--opciones nuevas--}

{-se arma una lista de todos los autos que le van ganando a elAauto, y a esa cantidad se le suma 1 para obtener el
puesto del elAuto-}

{-----------------------------------------------}

{--------------------PARTE 2--------------------}

correrDurante::Int->Auto->Auto
correrDurante tiempo unAuto = unAuto {distancia = distancia unAuto + tiempo*(velocidad unAuto)} 

{--opciones nuevas--}

bajarVelocidad::Cantidad->Auto->Auto
bajarVelocidad cantidad unAuto = alterarVelocidad ((max 0) . (\x->x-cantidad)) unAuto

{--opciones nuevas--}

type Cantidad = Int
type ModificadorVelocidad = Int->Int

alterarVelocidad::ModificadorVelocidad->Auto->Auto
alterarVelocidad modificador unAuto = unAuto {velocidad = modificador (velocidad unAuto)}

{-bajarVelocidad::Cantidad->Auto->Auto
bajarVelocidad cantidad unAuto
  |velocidad(unAuto)>cantidad = alterarVelocidad (\_->velocidad(unAuto)-cantidad) unAuto
  |otherwise = alterarVelocidad (\_->0) unAuto-}


{-----------------------------------------------}

{--------------------PARTE 3--------------------}

type PowerUp = Auto->Carrera->[Auto]

{--aplicarPowerUp::PowerUp->Auto->Carrera->Carrera
aplicarPowerUp funcionPowerUp autoTrigger carrera = Carrera (funcionPowerUp autoTrigger carrera)--}

terremoto::PowerUp
terremoto autoTrigger = afectarALosQueCumplen (estaCerca autoTrigger) (bajarVelocidad 50).autos

miguelitos::Int->PowerUp
miguelitos cantidadABajar autoTrigger = (afectarALosQueCumplen (vaGanando autoTrigger)
                                         (bajarVelocidad cantidadABajar)).autos 


{--opciones nuevas--}
jetpack::Tiempo->PowerUp
jetpack tiempo autoTrigger = afectarALosQueCumplen (==autoTrigger) 
                                    (alterarVelocidad (`div` 2) . correrDurante tiempo . alterarVelocidad (*2)) . autos
{--opciones nuevas--}

--extra 3                                 
type Tiempo = Int
type Distancia = Int


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
{-ANULADO
finalCarrera :: Carrera->[Evento]->Carrera
finalCarrera carrera [] = carrera
finalCarrera carrera listaEventos = last listaEventos (finalCarrera carrera (init listaEventos))-}

finalCarrera :: Carrera->[Evento]->Carrera
finalCarrera carrera listaEventos = foldl ocurreEvento carrera listaEventos
{-partiendo del estado de carreraInicial (parámetro carrera), aplica los eventos de manera sucesiva
llegando al estado de finalCarrera-}

ocurreEvento:: Carrera->Evento->Carrera
ocurreEvento carrera evento = evento carrera

crearTabla:: Carrera -> TablaDePosiciones
crearTabla carrera = map (infoPuestoSegunAuto carrera) (autos carrera)
{-recibe por parámetro el estado de una carrera y devuelve una lista de tipos de dato 
InfoPuesto (posicion y color de cada auto)-}

infoPuestoSegunAuto:: Carrera -> Auto -> InfoPuesto
infoPuestoSegunAuto carrera auto = InfoPuesto (puesto' auto carrera) (color auto)
----------


correnTodos:: Int -> Evento
correnTodos tiempo = Carrera . map (correrDurante tiempo) . autos

{-///////////////////////////////////////////////////////////////////////////////-}
usaPowerUp:: PowerUp -> Color -> Evento
usaPowerUp power color_auto carrera = Carrera (flip power carrera (autoSegunColor color_auto carrera))

{-Ya existe la funcion aplicarPowerUp-}
{-aplicarPowerUp::PowerUp->Auto->Carrera->Carrera
aplicarPowerUp funcionPowerUp autoTrigger carrera = Carrera (funcionPowerUp autoTrigger carrera)-}


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
                                                        (alterarVelocidad (modificarVelocidad 10)) . autos $ carrera
  | otherwise = golpeMisil (autoSegunColor colorAuto carrera) autoTrigger (autos carrera)
 
-----extra 
modificarVelocidad::Cantidad->ModificadorVelocidad
modificarVelocidad nuevaVelocidad _  = nuevaVelocidad

{-equivalente a usar (\_->cantidadDeterminada)-}

{-modifico la velocidad con la cantidad indicada independientemente de la velocidad actual del auto-}
----------
-----extra 2
golpeMisil::Auto->Auto->[Auto]->[Auto]
golpeMisil autoGolpeado autoTrigger autosCarrera
 |vaGanando autoGolpeado autoTrigger = afectarALosQueCumplen ((==color autoGolpeado).color) (alterarDistancia 5) autosCarrera
 |otherwise = autosCarrera

{-Incrementa distancia del autoGolpeado sólo si este le va ganando al que le envió el misil-}
------------
{-----------------------------------------------}



{-------------------RESPUESTA-------------------}

{-a. ¿La solución desarrollada hasta este punto permite agregar el nuevo power up o sería necesario
cambiar algo de lo desarrollado en los puntos anteriores? Justificar.

Se pudo implementar un nuevo power up sin necesidad de modificar la solucion desarrollada hasta ese punto.
Se adaptaron las nuevas funciones a la estructura ya establecida como en el caso de uso de la funcion "alterarVelocidad" 

-}

{-----------------------------------------------}