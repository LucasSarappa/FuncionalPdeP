{-
Los Simpson: https://github.com/FedericoEncinazSayago/Enciclopedia-De-Pdep/blob/main/Haskell/Parciales/ParcialLosSimpson.hs

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

    -- Punto 1: Actividades de los personajes

    type Actividad = Personaje -> Personaje

    data Personaje = UnPersonaje {
        nombre :: String,
        felicidad :: Int,
        dinero :: Int
    } deriving Show

    -- Definimos funciones auxiliares para los personajes:

    actualizarFelicidad :: Int -> Personaje -> Personaje
    actualizarFelicidad nuevoNivel personaje = personaje {felicidad = comoQuedaElNivel (felicidad personaje) nuevoNivel}

    comoQuedaElNivel :: Int -> Int -> Int
    comoQuedaElNivel actualNivel variacion
        | actualNivel + variacion > 0 = actualNivel + variacion
        | otherwise = 0

    actualizarDineroDisponible :: Int -> Personaje -> Personaje
    actualizarDineroDisponible nuevoNivel personaje = personaje {dinero = dinero personaje + nuevoNivel}

    -- Definimos las actividades:

    irEscuelaElemental :: Actividad
    irEscuelaElemental personaje
        | siNoEsTal "Lisa Simpson" personaje = actualizarFelicidad (-20) personaje
        | otherwise = actualizarFelicidad 20 personaje

    siNoEsTal :: String -> Personaje -> Bool
    siNoEsTal nombreDeLaPersona personaje = nombre personaje /= nombreDeLaPersona

    comerDona :: Actividad
    comerDona = actualizarFelicidad 10 . actualizarDineroDisponible (-10)

    comerUnaCantidadDeDonas :: Int -> Actividad
    comerUnaCantidadDeDonas cantDeDonasComidas personaje
        | cantDeDonasComidas >= 0 = comerUnaCantidadDeDonas (cantDeDonasComidas - 1) (comerDona personaje)
        | otherwise = personaje

    irAlTrabajo :: String -> Actividad
    irAlTrabajo trabajo = actualizarDineroDisponible (cuantoGano trabajo)

    cuantoGano :: String -> Int
    cuantoGano = length

    serDirector :: Actividad
    serDirector = irEscuelaElemental . irAlTrabajo "Escuela elemental"

    -- Definimos personajes:

    homero, skinner, lisa :: Personaje

    homero = UnPersonaje "Homero Simpson" 50 100
    skinner = UnPersonaje "Skinner" 10 500
    lisa = UnPersonaje "Lisa Simpson" 100 0

    -- Ejemplos de invocacion por terminal:

    {-
        Invocacion: ghci> serDirector skinner 

        Resultado: ghci> UnPersonaje {nombre = "Skinner", felicidad = 0, dinero = 517}
    -}

    {-
        Invocacion: ghci> comerUnaCantidadDeDonas 12 homero

        Resultado: UnPersonaje {nombre = "Homero Simpson", felicidad = 180, dinero = -30}
    -}

    -- Punto 2: Logros

    type Logro = Personaje -> Bool

    -- Definimos el Sr.Burns:

    srBurns :: Personaje
    srBurns = UnPersonaje "Sr.Burns" 0 1000000000

    -- Definimos los logros:

    serMillonario :: Logro
    serMillonario personaje = dinero personaje > dinero srBurns

    alegrarse :: Int -> Logro
    alegrarse nivelFelicidadDeseado personaje = felicidad personaje > nivelFelicidadDeseado

    irAlProgramaDeKrosti :: Logro
    irAlProgramaDeKrosti personaje = dinero personaje >= 10

    -- Funciones auxiliares para los logros:

    unaActividadResultaDecisivaParaLograrUnLogro :: Actividad -> Logro -> Personaje -> Bool
    unaActividadResultaDecisivaParaLograrUnLogro actividad logro personaje
        | logro personaje = False
        | otherwise = (logro . actividad) personaje

    type Actividades = [Actividad]

    encontrarLaActivadadDecisivaParaLograrElLogro :: Actividades -> Logro -> Personaje -> Personaje
    encontrarLaActivadadDecisivaParaLograrElLogro [] logro personaje = personaje
    encontrarLaActivadadDecisivaParaLograrElLogro (actividad : actividades) logro personaje
        | unaActividadResultaDecisivaParaLograrUnLogro actividad logro personaje = actividad personaje
        | otherwise = encontrarLaActivadadDecisivaParaLograrElLogro actividades logro personaje

    -- Definimos lista infita de actividades:

    hacerActividadesInfinitas :: Actividades -> Actividades
    hacerActividadesInfinitas actividades = actividades ++ hacerActividadesInfinitas actividades