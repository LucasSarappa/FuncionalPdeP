{-

Nos contactaron para hacer un sistema que ayude a tomar decisiones sobre series de TV a producir en un nuevo servicio llamado PdePrime Video.

En el sistema vamos a trabajar con series, de las cuales queremos saber cual es el nombre de la misma, quienes actúan en ella (y el orden de importancia), su presupuesto anual,
cantidad de temporadas estimadas, el rating promedio que tiene y si está cancelada o no.

También, de cada actor o actriz conocemos el nombre, cuál es su sueldo pretendido (anual) y qué restricciones tiene a la hora de trabajar. Por ejemplo, sabemos que el sueldo
pretendido de Paul Rudd es de 41 millones al año y que sus restricciones son no actuar en bata y comer ensalada de rúcula todos los días.

Resolver los siguientes requerimientos, maximizando el uso de composición, aplicación parcial y orden superior. Explicar dónde utilizó a cada uno de los conceptos.

    Parte A

    Vamos a conocer un poco a nuestras series.

        1. Saber si la serie estaEnRojo, esto es si el presupuesto no alcanza a cubrir lo que quieren cobrar todos los actores.

        2. Saber si una serie esProblemática, esto ocurre si tienen más de 3 actores o actrices con más de 1 restricción.


    Parte B

    Queremos modelar diferentes tipos de productores, que evalúan qué se hace con las series.

        1. conFavoritismos: elimina a los dos primeros actores de la serie y los reemplaza por sus actores favoritos.

        2. timBurton: es un caso particular de un productor con favoritismos, siempre reemplaza a los primeros dos actores por johnny depp y helena bonham carter, cuyos sueldos
        pretendidos anuales son $20000000 y $15000000 respectivamente, y no tienen ninguna restricción.
        
        3. gatopardeitor: no cambia nada de la serie.
        
        4. estireitor: duplica la cantidad de temporadas estimada de la serie.
        
        5. desespereitor: hace un combo de alguna de las anteriores ideas, mínimo 2.
        
        6. canceleitor: si la serie está en rojo o el rating baja de una cierta cifra, la serie se cancela.


    Parte C

    Lo creas o no las series tienen un bienestar y a veces pueden ser controvertidas.

    1. Calcular el bienestar de una serie, en base a la sumatoria de estos conceptos:
        ● Si la serie tiene estimadas más de 4 temporadas, su bienestar es 5, de lo contrario es (10 - cantidad de temporadas estimadas) * 2.

        ● Si la serie tiene menos de 10 actores, su bienestar es 3, de lo contrario es (10 - cantidad de actores que tienen restricciones), con un mínimo de 2.

        ● Aparte de lo mencionado arriba, si la serie está cancelada, su bienestar es 0 más allá de cómo diesen el bienestar por longitud y por reparto.

    2. Algunos productores buscan llevar bienestar a las series. Para ellos queremos saber si un grupo de grupo de productores sonBeneficiosos para una serie. Esto sucede cuando
    cada uno le da su toque y la serie termina con un bienestar mayor a 4.

    3. Por último queremos saber si una serie esControvertida, esto sucede cuando no necesariamente cada actor cobra más que el siguiente.


    Parte D

    Responder, justificando en cada caso:

    1. ¿Se puede aplicar el productor gatopardeitor cuando tenemos una lista infinita de actores?

    2. ¿Y a uno con favoritismos? ¿De qué depende?
    Aclaraciones
        ● Todas las funciones deberán estar tipadas. (Dar sus tipos)
        ● NO repetir lógica.
        ● Usar composición siempre que sea posible.

-}


module PdePrime where

-- Definiciones base

    data Serie = Serie {
        nombre :: String,
        actores :: [Actor],
        presupuesto :: Float,
        temporadasEstimadas :: Int,
        rating :: Float,
        cancelada :: Bool
    } deriving Show

    data Actor = Actor {
        nombreActor :: String,
        sueldoPretendido :: Float,
        restricciones :: [String]
    } deriving Show


    --series:

    breakingBad = Serie "Breaking Bad" [johnnyDepp, helenaBonhamCarter] 1000000 5 9.5 False
    theOffice = Serie "The Office" [johnnyDepp, helenaBonhamCarter] 1000000 5 9.5 False
    friends = Serie "Friends" [johnnyDepp, helenaBonhamCarter] 1000000 5 9.5 False
    theBigBangTheory = Serie "The Big Bang Theory" [johnnyDepp, helenaBonhamCarter] 1000000 5 9.5 False
    theSimpsons = Serie "The Simpsons" [johnnyDepp, helenaBonhamCarter] 1000000 5 9.5 False
    theWalkingDead = Serie "The Walking Dead" [johnnyDepp, helenaBonhamCarter] 1000000 5 9.5 False
    theMandalorian = Serie "The Mandalorian" [johnnyDepp, helenaBonhamCarter] 1000000 5 9.5 False
    theCrown = Serie "The Crown" [johnnyDepp, helenaBonhamCarter] 1000000 5 9.5 False
    theUmbrellaAcademy = Serie "The Umbrella Academy" [johnnyDepp, helenaBonhamCarter] 1000000 5 9.5 False
    theHauntingOfHillHouse = Serie "The Haunting of Hill House" [johnnyDepp, helenaBonhamCarter] 1000000 5 9.5 False
    metegol = Serie "Metegol" [ricardoFort, ricardoDarin, juanDarthes, lionelMessi] 1000000 5 9.5 False

    --actores:

    johnnyDepp = Actor "Johnny Depp" 20000000 []
    helenaBonhamCarter = Actor "Helena Bonham Carter" 15000000 []
    paulRudd = Actor "Paul Rudd" 41000000 ["bata", "ensalada de rúcula"]
    jenniferAniston = Actor "Jennifer Aniston" 50000000 []
    lisaKudrow = Actor "Lisa Kudrow" 50000000 []
    courteneyCox = Actor "Courteney Cox" 50000000 []
    mattLeBlanc = Actor "Matt LeBlanc" 50000000 []
    matthewPerry = Actor "Matthew Perry" 50000000 []
    davidSchwimmer = Actor "David Schwimmer" 50000000 []
    ricardoFort = Actor "Ricardo Fort" 8 ["bata","ensalada de rúcula","no actuar en el mismo capítulo que Paul Rudd"]
    ricardoDarin = Actor "Ricardo Darín" 5 ["bata","ensalada de rúcula","no actuar en el mismo capítulo que Paul Rudd"]
    juanDarthes = Actor "Juan Darthés" 6 ["bata","ensalada de rúcula","no actuar en el mismo capítulo que Paul Rudd"]
    lionelMessi = Actor "Lionel Messi" 7 ["bata","ensalada de rúcula","no actuar en el mismo capítulo que Paul Rudd"]


-- Parte A

{-
 Parte A

    Vamos a conocer un poco a nuestras series.
        1. Saber si la serie estaEnRojo, esto es si el presupuesto no alcanza a cubrir lo que quieren cobrar todos los actores.
        2. Saber si una serie esProblemática, esto ocurre si tienen más de 3 actores o actrices con más de 1 restricción.
-}

    estaEnRojo :: Serie -> Bool
    estaEnRojo serie = (sum . map sueldoPretendido . actores) serie > presupuesto serie

    esProblematica :: Serie -> Bool
    esProblematica serie = (length . filter ((>1) . length . restricciones) . actores) serie > 3

    -- para probar:
    -- esProblematica breakingBad me devuelve False
    -- estaEnRojo breakingBad me devuelve False
    -- esProblematica metegol me devuelve True
    -- estaEnRojo metegol me devuelve False



-- Parte B
{-
    Queremos modelar diferentes tipos de productores, que evalúan qué se hace con las series.

        1. conFavoritismos: elimina a los dos primeros actores de la serie y los reemplaza por sus actores favoritos.
        2. timBurton: es un caso particular de un productor con favoritismos, siempre reemplaza a los primeros dos actores por johnny depp y helena bonham carter, cuyos sueldos
        pretendidos anuales son $20000000 y $15000000 respectivamente, y no tienen ninguna restricción.
        3. gatopardeitor: no cambia nada de la serie.
        4. estireitor: duplica la cantidad de temporadas estimada de la serie.
        5. desespereitor: hace un combo de alguna de las anteriores ideas, mínimo 2.
        6. canceleitor: si la serie está en rojo o el rating baja de una cierta cifra, la serie se cancela.

-}

    conFavoritismos :: [Actor] -> [Actor] -> [Actor]
    conFavoritismos actoresFavoritos = (actoresFavoritos ++) . drop 2

    timBurton :: Serie -> Serie
    timBurton serie = serie { actores = conFavoritismos [johnnyDepp, helenaBonhamCarter] (actores serie) }

    gatopardeitor :: Serie -> Serie
    gatopardeitor = id

    estireitor :: Serie -> Serie
    estireitor serie = serie { temporadasEstimadas = temporadasEstimadas serie * 2 }

    desespereitor :: Serie -> Serie
    desespereitor = estireitor . timBurton

    canceleitor :: Float -> Serie -> Serie
    canceleitor ratingMinimo serie
        | estaEnRojo serie || rating serie < ratingMinimo = serie { cancelada = True }
        | otherwise = serie 

    
    -- para probar:
    -- timBurton breakingBad me devuelve Serie "Breaking Bad" [Actor {nombreActor = "Johnny Depp", sueldoPretendido = 20000000.0, restricciones = []},Actor {nombreActor = "Helena Bonham Carter", sueldoPretendido = 15000000.0, restricciones = []}] 1000000.0 5 9.5 False
    
-- Parte C

    bienestar :: Serie -> Int
    bienestar serie
        | cancelada serie = 0
        | otherwise = max 0 $ min 5 $ bienestarPorTemporadas serie + bienestarPorActores serie

    bienestarPorTemporadas :: Serie -> Int
    bienestarPorTemporadas serie
        | temporadasEstimadas serie > 4 = 5
        | otherwise = (10 - temporadasEstimadas serie) * 2

    bienestarPorActores :: Serie -> Int
    bienestarPorActores serie
        | length (actores serie) < 10 = 3
        | otherwise = max 2 $ 10 - length (filter ((>0) . length . restricciones) (actores serie))

    sonBeneficiosos :: [Serie -> Serie] -> Serie -> Bool
    sonBeneficiosos productores serie = bienestar (foldl (flip ($)) serie productores) > 4

    esControvertida :: Serie -> Bool
    esControvertida serie = all (\(a1, a2) -> sueldoPretendido a1 > sueldoPretendido a2) $ zip (actores serie) (tail $ actores serie)

    -- para probar:
    -- bienestar breakingBad me devuelve 5


    
    
-- Parte D

    -- 1. ¿Se puede aplicar el productor gatopardeitor cuando tenemos una lista infinita de actores?
    -- No, ya que gatopardeitor no modifica la serie, por lo que no tiene sentido aplicarlo a una lista infinita de actores.

    -- 2. ¿Y a uno con favoritismos? ¿De qué depende?
    -- Sí, ya que conFavoritismos toma los dos primeros actores de la serie y los reemplaza por los actores favoritos, por lo que no importa la cantidad de actores que tenga la serie.

-- Explicar donde se uso composición, aplicación parcial y orden superior

    -- Se utilizó composición en la función sonBeneficiosos, donde se aplica foldl (flip ($)) serie productores, que es equivalente a aplicar cada productor a la serie en orden.
    -- Se utilizó aplicación parcial en la función conFavoritismos, donde se aplica actoresFavoritos a la función conFavoritismos, y luego se aplica el resultado a drop 2.
    -- Se utilizó orden superior en la función sonBeneficiosos, donde se recibe una lista de funciones y se aplica cada una a la serie en orden.