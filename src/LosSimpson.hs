{-
Los Simpson:

1: Actividades de los personajes
Existen diferentes personajes de Los Simpson. De cada uno se sabe su nombre, el dinero que tiene y su felicidad (que puede ser mayor o igual a cero pero nunca negativa). 

Los personajes pueden realizan diferentes actividades que provocan cambios en ellos: 
Ir a la escuela elemental de springfield: se le resta 20 a la felicidad del personaje, salvo que sea lisa, en cuyo caso aumenta la felicidad en igual cantidad.
Comer una cierta cantidad de donas: se le suma 10 de felicidad por cada dona comida y se le resta $10.
Ir a trabajar: gana una cantidad de dinero según qué trabajo sea. Si es la "planta nuclear" son 14$ (porque son 14 caracteres).
Ir a trabajar como director: implica ir a a trabajar a "escuela elemental" y restar 20 de felicidad (como todo el que va a dicha escuela) 
Agregar uno inventado que modifique de cierta manera a una persona.

Modelar algunos personajes, implementar las acciones mencionadas y mostrar ejemplos de invocación y respuesta:
homero come una docena de donas
skinner va a trabajar como director
lisa va a la escuela y luego realiza la actividad inventada

2. Logros 
También se definen logros que pueden ser alcanzados por una persona. Modelar los siguientes:
Ser millonario: lo alcanza  si tiene más dinero que el Sr. Burns 
Alegrarse: dado un nivel de felicidad deseado, lo alcanza si su propia felicidad lo supera.
Ir a ver el programa de Krosti: lo  alcanza si tiene al menos $10
Agregar uno inventado por ustedes
                        
Se quiere averiguar si una actividad resulta decisiva para un personaje para alcanzar un logro. Es decir que alguien que no alcanza un logro, pero que cuando realiza la acción indicada sí lo logra.  Por ejemplo, si bart tiene 6$ no puede ir a ver a krosty, pero si va a trabajar para la "mafia" pasaría a tener 11 lo lograría por lo tanto, dicha actividad resulta decisiva.
            
Dada una persona, un logro a alcanzar y una serie de actividades, encontrar la primera de ellas que sea decisiva y hacer que la persona la realice obteniendo cómo queda dicha persona. En caso que no haya ninguna decisiva, la persona permanece igual. 

Construir una lista infinita de actividades y mostrar al menos dos ejemplos de aplicacion de la funcíon anterior donde sucedan diferentes consecuencias. Justificar conceptualmente. 
-}

module LosSimpson where

    -- 1: Actividades de los personajes

data Personaje = Personaje {
    nombre :: String,
    dinero :: Int,
    felicidad :: Int
} deriving Show

-- Personajes
homero :: Personaje
homero = Personaje "Homero" 100 100
lisa :: Personaje
lisa = Personaje "Lisa" 100 100
skinner :: Personaje
skinner = Personaje "Skinner" 100 100
burns :: Personaje
burns = Personaje "Burns" 100000 100
bart :: Personaje
bart = Personaje "Bart" 6 100

-- Actividades
type Actividad = Personaje -> Personaje 

escuela :: Actividad
escuela personaje
    | nombre personaje == "Lisa" = modificarFelicidad 20 personaje
    | otherwise = modificarFelicidad (-20) personaje

-- Ejemplos de consulta: 
-- escuela bart 
-- Personaje {nombre = "Bart", dinero = 100, felicidad = 80}
-- escuela lisa 
-- Personaje {nombre = "Lisa", dinero = 100, felicidad = 120}

comerDonas :: Int -> Actividad
comerDonas cantidad = modificarDinero (-10) . modificarFelicidad (10 * cantidad)

-- Ejemplo de consulta: 
-- comerDonas 12 homero 
-- Personaje {nombre = "Homero", dinero = 90, felicidad = 220}

trabajar :: String -> Actividad
trabajar trabajo = modificarDinero (length trabajo)

-- Ejemplo de consulta: 
-- trabajar "planta nuclear" homero 
-- Personaje {nombre = "Homero", dinero = 114, felicidad = 100}

trabajarDeDirector :: Actividad
trabajarDeDirector = trabajar "Escuela elemental". escuela

-- Ejemplo de consulta: 
-- trabajarDeDirector skinner 
-- Personaje {nombre = "Skinner", dinero = 117, felicidad = 80}


modificarFelicidad :: Int -> Actividad
modificarFelicidad cantidad personaje 
 = personaje {felicidad = max 0 (felicidad personaje + cantidad)}

modificarDinero ::  Int -> Actividad
modificarDinero cantidad personaje 
 = personaje {dinero = dinero personaje + cantidad}


-- 2: Logros

type Logro = Personaje -> Bool

millonario :: Logro
millonario personaje = dinero personaje > dinero burns

alegrarse :: Int -> Logro
alegrarse nivel personaje = felicidad personaje > nivel

verKrusty :: Logro
verKrusty personaje = dinero personaje >= 10

-- A)
decisiva :: Actividad -> Logro -> Personaje -> Bool
decisiva actividad logro personaje = not (logro personaje) && logro (actividad personaje)

-- Ejemplos de consulta: 
-- decisiva (trabajar "mafia") verKrusty bart 
-- True
-- decisiva (trabajar "planta nuclear") millonario homero 
-- False

-- B)
primeraDecisiva :: Personaje -> Logro -> [Actividad] -> Personaje
primeraDecisiva personaje _ [] = personaje
primeraDecisiva personaje logro (actividad:actividades)
    | decisiva actividad logro personaje = actividad personaje
    | otherwise = primeraDecisiva personaje logro actividades

-- Ejemplos de consulta: 
-- primeraDecisiva bart verKrusty [trabajar "no", trabajar "mafia"] 
-- Personaje {nombre = "Bart", dinero = 11, felicidad = 100}
-- primeraDecisiva homero millonario [trabajar "mafia", trabajar "planta nuclear"] 
-- Personaje {nombre = "Homero", dinero = 100, felicidad = 100}

-- C)
infinitasActividades :: [Actividad]
infinitasActividades = escuela : infinitasActividades

-- Ejemplos de consulta:
-- primeraActividadDecisiva lisa (alegrarse 105) listaInfinitaActividades 
-- Personaje {nombre = "Lisa", dinero = 100, felicidad = 120}
-- Por evaluación diferida, encuentra la primera que es decisiva y la aplica
-- primeraActividadDecisiva bart irAVerAKrosty listaInfinitaActividades 
-- Hasta el momento no encontró ninguna decisiva, por lo que no dio ninguna respuesta, pero sigue buscando...