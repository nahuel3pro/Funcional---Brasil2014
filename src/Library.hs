module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--1
type Nombre = String
type Grupo = Char
type Lista = [Jugador]
type Cansancio = Number
type Promedio = Number
type Edad = Number
type Habilidad = Number

data Jugador = Jugador {
	nombre :: Nombre,
	edad :: Edad,
	promedioGol :: Promedio,
	habilidad :: Habilidad,
	cansancio :: Cansancio
} deriving(Show, Eq)

data Equipo = Equipo{
	nombreEquipo :: Nombre,
	grupo :: Grupo,
	listaJugadores :: Lista
} deriving(Show, Eq)
-- 1
figurasDeEquipo :: Equipo -> Lista
figurasDeEquipo =  filter (esFigura) . listaJugadores

esFigura :: Jugador -> Bool
esFigura jugador = ((>75). habilidad $ jugador) && ((>0) . promedioGol $ jugador)

-- 2
jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]
tieneFarandulero :: Equipo -> Bool
tieneFarandulero = elem True . map (esFarandulero) . listaJugadores

esFarandulero :: Jugador -> Bool
esFarandulero jugador = elem (nombre jugador) jugadoresFaranduleros

-- 3
{-equipo1 = ("Lo Que Vale Es El Intento", 'F', [martin, juan, maxi])
losDeSiempre = ( "Los De Siempre", 'F', [jonathan, lean, brian])
-}
-- GrupoF = [equipo1, losDeSiempre]
jugadorEsDificl :: Jugador -> Bool
jugadorEsDificl jugador = esFigura jugador && (esJoven jugador) && (not . esFarandulero $ jugador)

esJoven :: Jugador -> Bool
esJoven = (<27) . edad

jugadoresDificilesEnGrupo :: [Equipo] -> [Nombre]
jugadoresDificilesEnGrupo = map nombre . filter jugadorEsDificl . concat . map (listaJugadores)

--4
type Partido = Equipo -> Lista
jugarPartido :: Partido
jugarPartido = map (cansarJugador) . listaJugadores

cansarJugador :: Jugador -> Jugador
cansarJugador jugador
 | (not.esFarandulero $ jugador) && (esJoven jugador) && (esFigura jugador) = jugador {cansancio = 50}
 | esJoven jugador = jugador {cansancio = ((*10).(/100) . cansancio $ jugador) + (cansancio jugador)}
 | (not.esJoven $ jugador) && (esFigura jugador) = jugador {cansancio = (+20) . cansancio $ jugador}
 | otherwise = jugador {cansancio = (*2) . cansancio $ jugador}

 --5
ganador :: Equipo -> Equipo -> Equipo
ganador equipo1 equipo2 = undefined