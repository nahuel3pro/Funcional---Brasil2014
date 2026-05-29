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

  {-5) Empezó el mundial y los partidos se empiezan a jugar. ¿Cómo saber quién gana en cada partido? Cuando se enfrentan
  2 equipos, se seleccionan los primeros 11 jugadores (por equipo) que menos cansados están y se suma su promedio de gol. 
  El que sume un mejor promedio gana el partido. 
Se pide entonces, dados dos equipos, devolver al ganador del partido, con sus jugadores modificados
 por haber jugado el partido.

	Usar take y sort
-}
menosCansados :: Equipo -> [Jugador]
menosCansados = take 11 . quickSort menorCansansio . listaJugadores

promedioGolEnEquipo :: [Jugador] -> Promedio
promedioGolEnEquipo equipo = sum . map . promedioGol

ganador :: Equipo -> Equipo -> Equipo
ganador equipo1 equipo2
 | (< (promedioGolEnEquipo . menosCansados $ equipo1)) . promedioGolEnEquipo . menosCansados $ equipo2 = equipo1
 | otherwise = equipo2

{-
6) Sabiendo ya cómo se decide el ganador de un partido, ahora queremos saber, a partir de un grupo de equipos, 
qué equipo se consagrará campeón del torneo.
¿Cómo se juegan los partidos? 
El primero juega contra el segundo → Ganador1
Ganador1 juega contra tercer equipo → Ganador2
Ganador2 juega contra cuarto equipo → Ganador3

Y así hasta que el ganador del último partido se consagra campeón.
Dar 2 resoluciones diferentes al ejercicio
-}

{-
7) Los días pasaron, las vuvuzelas se escucharon, una nueva Larissa Riquelme se hizo conocida,
y el pulpo Paul volvió a acertar en los resultados. Después de un gran mundial se quiere saber quién va a ser elegido
 como el mejor de todos para entregarle el premio y ser reconocido en todo el mundo como “EL GROSO”.
Para ello se ingresa una lista de equipos, y del equipo elegido ganador (el campeón), se quiere saber el nombre del primer
 jugador que cumpla la condición de ser figura (en todo equipo hay 1 por lo menos).


filter esFigura y tomar el head de esa lista
-}
