module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Velocidad = Number
type Distancia = Number
type Color = String
data Auto = Auto{
  color :: Color,
  velocidad :: Velocidad,
  distancia :: Number
}

type Carrera = [Auto]

-- 1
estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = (sonDistintos auto1 auto2 ) && ((<10) . distanciaEntre auto1 $ auto2)

sonDistintos :: Auto -> Auto -> Bool
sonDistintos auto1 auto2 = (/= color auto1) . color $ auto2

distanciaEntre :: Auto -> Auto -> Number
distanciaEntre auto1 auto2 = abs . difDistancia auto1 $ auto2

difDistancia :: Auto -> Auto -> Number
difDistancia auto1 auto2 = (subtract . distancia $ auto1) . distancia $ auto2

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = (not . any (estaCerca auto) $ carrera) && ((==1) . puesto auto $ carrera)

puesto :: Auto -> Carrera -> Number
puesto auto carrera = (+1) . length . filter (estaDelante auto) $ carrera

estaDelante :: Auto -> Auto -> Bool
estaDelante auto1 auto2 = (< 0) . difDistancia auto1 $ auto2

-- 2
-- a
type Tiempo = Number
correr :: Tiempo -> Auto -> Auto
correr tiempo auto = avanzar (recorrer tiempo auto) auto  

avanzar :: Distancia -> Auto -> Auto
avanzar nuevaDistancia auto = auto {distancia = (+ nuevaDistancia) . distancia $ auto}

recorrer :: Tiempo -> Auto -> Number
recorrer tiempo auto = (* tiempo).velocidad $ auto

-- b
alterarVelocidad :: (Number -> Number) -> Auto -> Auto
alterarVelocidad modificador auto = auto { velocidad = max 0 . modificador.velocidad $ auto }

bajarVelocidad :: Number -> Auto -> Auto
bajarVelocidad numero auto = flip alterarVelocidad auto . subtract $ numero

-- 3
type PowerUp = Auto -> Carrera -> Carrera

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista


terremoto :: PowerUp
terremoto auto carrera = afectarALosQueCumplen (estaCerca auto) (alterarVelocidad (subtract 50)) carrera

miguelito :: Velocidad -> PowerUp
miguelito velocidad auto carrera = afectarALosQueCumplen (estaDelante auto) (bajarVelocidad velocidad) carrera

jetPack :: Tiempo -> PowerUp
--jetPack tiempo auto carrera = afectarALosQueCumplen (not sonDistintos auto) (usarJetpack) carrera
jetPack = undefined

-- 4
-- a
type Eventos = [Carrera->Carrera]
type Posiciones = (Number, Color)

simularCarrera :: Carrera -> Eventos -> [Posiciones]
simularCarrera carrera eventos = hacerPodio . foldr ($) carrera $ eventos

hacerPodio :: Carrera -> [Posiciones]
hacerPodio carreraTerminada = map (posicion carreraTerminada) carreraTerminada

posicion :: Carrera -> Auto -> Posiciones
posicion carreraTerminada auto = (puesto auto carreraTerminada, color auto)

-- b
correnTodos :: Tiempo -> Carrera -> Carrera
correnTodos tiempo carrera = map (correr tiempo) carrera

usaPowerUp :: PowerUp -> Color -> Carrera -> Carrera
--usaPowerUp powerUp color carrera = powerUp . filter  
usaPowerUp = undefined

-- c
autoIncial = Auto{
  color = "",
  velocidad = 120,
  distancia = 0
}

rojo = autoIncial{color = "Rojo"}
blanco = autoIncial{color = "blanco"}
azul = autoIncial{color = "Azul"}
negro = autoIncial{color = "Negro"}

eventos = [correnTodos 30, usaPowerUp (jetPack 3) "Azul", terremoto blanco, correnTodos 40, usaPowerUp (miguelito 20) "Blanco", usaPowerUp (jetPack 6) "Negro", correnTodos 10]
unaCarrera = [rojo, blanco, azul, negro]

ejemplo = simularCarrera unaCarrera eventos

-- 5a
-- Es posible ya que la definición de powerUp permite agregarlo. Es solamente declararlo como
-- misilTeledirigido :: Auto -> PowerUp

-- 5b
-- 1b -> VaTranquilo podría llegar a terminar por el lazy evaluation, al encontrar un auto
-- que rompa la búsqueda
-- 1c -> puesto nunca va a terminar porque evaluaría todos los autos de la carrera, y al ser una lista infinita eso nunca pasaría
