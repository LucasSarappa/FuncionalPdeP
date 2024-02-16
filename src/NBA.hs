{-
NBA.COM

La NBA contrata al mejor recurso humano que hay en la Argentina, nuestra cátedra, paramodelar "El juego de las Estrellas" mediante el Paradigma Funcional.

En la NBA hay equipos por todas las ciudades de E.E.U.U. que tienen un nombre y los basquetbolistas que juegan. De cada jugador se sabe su nombre, su altura, posición dentro
de la cancha, nacionalidad y además se anota por cada jugador cuántos puntos, cuántos rebotes y cuántas asistencias realizó en los 3 últimos partidos.

El torneo en si es una lista de equipos que participan del mismo. En cada equipo seencuentran todos los jugadores que podrían jugar los partidos que se organizan dentro deltorneo.

    1. Modelá a los basquetbolistas y los equipos y da ejemplos para las siguientessituaciones:

        Kyrie Irving juega en Boston Celtics, mide 1.9 m, juega de Alero, australiano
        Ginobili Emanuel juega en San Antonio Spurs, mide 1.98 m, juega de Escolta, argentino.
        Fabricio Oberto, juega en San Antonio Spurs, mide 2,08 m, juega de Pivot,argentino.
        Masón Plumlee, Brooklyn Nela,mide 2.11 m, juega de pivot, EEUU.
        Khem Birch, Orlando Magic, juega de ala_pivot, mide 2.06 m, Canadiense.

Armar las funciones que se solicitan a continuación utilizando por lo menos una vez

        composición de funciones
        aplicación parcial
        funciones de orden superior
        recursividad
        listas definidas por comprensión
        expresiones lambda



Para los basquetbolista:

    2. La estadística de un jugador es el promedio de sus cualidades puntos, rebotes yasistencias.

    3. La NBA necesita saber qué jugador es Estrella, para lo que se requiere tener másde 35 puntos y el promedio de rebotes y asistencias mayor que 0.

    4. Se conoce que tras un partido del torneo de las Estrellas, los jugadores se cansandebido al extenso calendario que tuvieron durante el año y a que muchos ya estánprontos a su retiro. 
    Analíticamente el cansancio se mide en función a la estadísticaobtenida en el punto 2. 
    Definir la función cansancio, la cual dado un jugadorinforme que valor de cansancio total le queda según los siguientes criterios:

        Si el jugador no es extranjero, es bajo y estrella, su cansancio total pasa aser 50 unidades.

        Para el resto de los jugadores extranjeros, su cansancio aumenta un 10% de lo hallado en la estadística.

        Si el jugador no es bajo y es estrella del equipo se incrementa en 20 unidades su cansancio.

        En cualquier otro caso, el cansancio se duplica.

    Un jugador es extranjero si NO nació en EE.UU. y es bajo si mide menos de 2 metros

Para los equipos:

    5. La NBA se enteró que en los mundiales de fútbol la compañía Panini crea un álbumde figuritas y por ello le pidió que cree un álbum para el torneo en cuestión.
    Siempre en los álbumes hay figuritas dificiles, por lo tanto, debemos informar la listade los nombres de los jugadores que tendrían que ser las figuritas dificiles (y hasta a
    veces brillantes).

    Para cumplir la condición deser difícil, el jugador tiene que cumplir simultáneamente:

                Ser estrella

                Ser bajo

                Ser extranjero.

NOTA: Hacer la función lista con listas por comprensión y además función de orden superior.

    6. Un equipo es apto para jugar cuando tiene más de 5 jugadores y entre losjugadores que tiene todos son de diferentes posiciones dentro de la cancha. UsarRecursividad para resolverio.

Para el Torneo:

El torneo de las Estrellas es el que organiza la NBA. Está formada por una lista deEquipos participantes.

    7. Empezó el torneo de la NBA y los partidos se empiezan a jugar. ¿Cómo saber quiéngana en cada partido? Cuando se enfrentan 2 equipos, se seleccionan los primeros5 jugadores 
    (por equipo) que menos cansancio tienen y se suma su promedio depuntos. El que sume un mejor promedio gana el partido.Se pide entonces, dados dos equipos, devolver al ganador del partido.


    Nota: Existe la funcion sortBy

    8. Se quiere hacer una seleccion representativa del Torneo. Solo puede haber un jugador por equipo. La lista de jugadores Super Estrellas esta formada por los primeros jugadores de cada 
    equipo. El primero de cada equipo es seleccionado con las siguientes cualidades: ser estrella y ser el menos cansado de cada equipo. 
    OJO: El jugador menos cansado puede que no sea estrella


    9. Que pasaria si al resolver el punto 8 los equipos que forman el torneo fueran creados con infinitos jugadores? Justificar


-}

module NBA where


    import Data.List (sortBy)

{-
En la NBA hay equipos por todas las ciudades de E.E.U.U. que tienen un nombre y losbasquetbolistas que juegan. De cada jugador se sabe su nombre, su altura, posición dentro
de la cancha, nacionalidad y además se anota por cada jugador cuántos puntos, cuántosrebotes y cuántas asistencias realizó en los últimos 3 partidos.

El torneo en si es una lista de equipos que participan del mismo. En cada equipo seencuentran todos los jugadores que podrían jugar los partidos que se organizan dentro deltorneo.

-}    

    data Jugador = UnJugador {
        nombre :: String,
        altura :: Float,
        posicion :: String,
        nacionalidad :: String,
        puntos :: [Int],
        rebotes :: [Int],
        asistencias :: [Int]
    }

    data Equipo = UnEquipo {
        nombreEquipo :: String,
        jugadores :: [Jugador]
    }


    data Torneo = UnTorneo {
        equipos :: [Equipo]
    }

    instance Show Jugador where
        show jugador = "Nombre: " ++ nombre jugador ++ " Altura: " ++ show (altura jugador) ++ " Posicion: " ++ posicion jugador ++ " Nacionalidad: " ++ nacionalidad jugador

    instance Show Equipo where
        show equipo = "Nombre: " ++ nombreEquipo equipo ++ " Jugadores: " ++ show (jugadores equipo)

    instance Show Torneo where
        show torneo = "Equipos: " ++ show (equipos torneo)
    



-- PUNTO 1:
{-

    1. Modelá a los basquetbolistas y los equipos y da ejemplos para las siguientessituaciones:

        Kyrie Irving juega en Boston Celtics, mide 1.9 m, juega de Alero, australianoGinobili Emanuel juega en San Antonio Spurs, mide 1.98 m, juega de Escolta,
        argentino.
        Fabricio Oberto, juega en San Antonio Spurs, mide 2,08 m, juega de Pivot,argentino.
        Masón Plumlee, Brooklyn Nela,mide 2.11 m, juega de pivot, EEUU.
        Khem Birch, Orlando Magic, juega de ala_pivot, mide 2.06 m, Canadiense.

-}


    -- Modelo a los jugadores:

    kyrieIrving = UnJugador "Kyrie Irving" 1.9 "Alero" "australiano" [10, 20, 30] [5, 10, 15] [5, 10, 15]
    ginobiliEmanuel = UnJugador "Ginobili Emanuel" 1.98 "Escolta" "argentino" [10, 20, 30] [5, 10, 15] [5, 10, 15]
    fabricioOberto = UnJugador "Fabricio Oberto" 2.08 "Pivot" "argentino" [10, 20, 30] [5, 10, 15] [5, 10, 15]
    masonPlumlee = UnJugador "Masón Plumlee" 2.11 "Pivot" "EEUU" [10, 20, 30] [5, 10, 15] [5, 10, 15]
    khemBirch = UnJugador "Khem Birch" 2.06 "Ala Pivot" "Canadiense" [10, 20, 30] [5, 10, 15] [5, 10, 15]

    -- Modelo a los equipos:

    bostonCeltics = UnEquipo "Boston Celtics" [kyrieIrving]
    sanAntonioSpurs = UnEquipo "San Antonio Spurs" [ginobiliEmanuel, fabricioOberto]
    brooklynNets = UnEquipo "Brooklyn Nets" [masonPlumlee]
    orlandoMagic = UnEquipo "Orlando Magic" [khemBirch]

    -- Modelo al torneo:

    torneo = UnTorneo [bostonCeltics, sanAntonioSpurs, brooklynNets, orlandoMagic]


{-

Armar las funciones que se solicitan a continuación utilizando por lo menos una vez

        composición de funciones
        aplicación parcial
        funciones de orden superior
        recursividad
        listas definidas por comprensión
        expresiones lambda

-}
-- PUNTO 2:

    -- 2. La estadística de un jugador es el promedio de sus cualidades puntos, rebotes y asistencias.

    estadistica :: Jugador -> Float
    estadistica jugador = (promedioPuntos jugador + promedioRebotes jugador + promedioAsistencias jugador) / 3

    promedioPuntos :: Jugador -> Float
    promedioPuntos jugador = fromIntegral (sum (puntos jugador)) / fromIntegral (length (puntos jugador))

    promedioRebotes :: Jugador -> Float
    promedioRebotes jugador = fromIntegral (sum (rebotes jugador)) / fromIntegral (length (rebotes jugador))

    promedioAsistencias :: Jugador -> Float
    promedioAsistencias jugador = fromIntegral (sum (asistencias jugador)) / fromIntegral (length (asistencias jugador))



    -- Para probar:

    -- estadistica kyrieIrving, me devuelve 13.333333


-- PUNTO 3:

    -- 3. La NBA necesita saber qué jugador es Estrella, para lo que se requiere tener más de 35 puntos y el promedio de rebotes y asistencias mayor que 0.

    esEstrella :: Jugador -> Bool
    esEstrella jugador = (sum (puntos jugador) > 35) && (promedioRebotes jugador > 0) && (promedioAsistencias jugador > 0)

    
    -- Para probar:

    -- esEstrella kyrieIrving, me devuelve True
    -- esEstrella ginobiliEmanuel, me devuelve True
    -- esEstrella fabricioOberto, me devuelve True
    -- esEstrella masonPlumlee, me devuelve True
    -- esEstrella khemBirch, me devuelve True


-- PUNTO 4:

{-

  4. Se conoce que tras un partido del torneo de las Estrellas, los jugadores se cansan debido al extenso calendario que tuvieron durante el año y a que muchos ya estánprontos a su retiro. 
    Analíticamente el cansancio se mide en función a la estadística obtenida en el punto 2. 

    Definir la función cansancio, la cual dado un jugador informe que valor de cansancio total le queda según los siguientes criterios:

        Si el jugador no es extranjero, es bajo y estrella, su cansancio total pasa a ser 50 unidades.

        Para el resto de los jugadores extranjeros, su cansancio aumenta un 10% de lo hallado en la estadística.

        Si el jugador no es bajo y es estrella del equipo se incrementa en 20 unidades su cansancio.

        En cualquier otro caso, el cansancio se duplica.

    Un jugador es extranjero si NO nació en EE.UU. y es bajo si mide menos de 2 metros


-}

    cansancio :: Jugador -> Float
    cansancio jugador | not (esExtranjero jugador) && esBajo jugador && esEstrella jugador = 50
                      | esExtranjero jugador = estadistica jugador * 1.1
                      | not (esBajo jugador) && esEstrella jugador = estadistica jugador + 20
                      | otherwise = estadistica jugador * 2


    esExtranjero :: Jugador -> Bool
    esExtranjero jugador = nacionalidad jugador /= "EEUU"

    esBajo :: Jugador -> Bool
    esBajo jugador = altura jugador < 2

    -- Para probar: (Originalmente, kyrieIrving es estrella, no es extranjero y es bajo)

    -- cansancio kyrieIrving, me devuelve 14.666666

{-
    Para los equipos:

    5. La NBA se enteró que en los mundiales de fútbol la compañía Panini crea un álbum de figuritas y por ello le pidió que cree un álbum para el torneo en cuestión.
    Siempre en los álbumes hay figuritas dificiles, por lo tanto, debemos informar la listade los nombres de los jugadores que tendrían que ser las figuritas dificiles (y hasta a
    veces brillantes).

    Para cumplir la condición de ser difícil, el jugador tiene que cumplir simultáneamente:

                Ser estrella
                Ser bajo
                Ser extranjero.

    NOTA: Hacer la función lista con listas por comprensión y además función de orden superior.

-}


-- Hacer la función lista con listas por comprensión y además función de orden superior.

    listaFiguritasDificiles :: Torneo -> [String]
    listaFiguritasDificiles torneo = map nombre (filter esDificil (concatMap jugadores (equipos torneo)))
    
    esDificil :: Jugador -> Bool
    esDificil jugador = esEstrella jugador && esBajo jugador && esExtranjero jugador

    -- Para probar:

    -- esDificil kyrieIrving, me devuelve False
    -- esDificil ginobiliEmanuel, me devuelve True

    -- listaFiguritasDificiles torneo, me devuelve ["Ginobili Emanuel", "Kyrie Irving"]


-- PUNTO 6:

    -- 6. Un equipo es apto para jugar cuando tiene más de 5 jugadores y entre losjugadores que tiene todos son de diferentes posiciones dentro de la cancha. UsarRecursividad para resolverio.

    equipoAptoParaJugar :: Equipo -> Bool
    equipoAptoParaJugar equipo = length (jugadores equipo) > 5 && todosDiferentesPosiciones (jugadores equipo)

    todosDiferentesPosiciones :: [Jugador] -> Bool
    todosDiferentesPosiciones [] = True
    todosDiferentesPosiciones (x:xs) = not (elem (posicion x) (map posicion xs)) && todosDiferentesPosiciones xs

    -- Para probar:

    -- equipoAptoParaJugar bostonCeltics, me devuelve False
    -- equipoAptoParaJugar sanAntonioSpurs, me devuelve True
    -- equipoAptoParaJugar brooklynNets, me devuelve True
    -- equipoAptoParaJugar orlandoMagic, me devuelve True



-- PUNTO 7:
{-
Para el Torneo:

El torneo de las Estrellas es el que organiza la NBA. Está formada por una lista deEquipos participantes.

    7. Empezó el torneo de la NBA y los partidos se empiezan a jugar. ¿Cómo saber quiéngana en cada partido? Cuando se enfrentan 2 equipos, se seleccionan los primeros 5 jugadores 
    (por equipo) que menos cansancio tienen y se suma su promedio depuntos. El que sume un mejor promedio gana el partido. Se pide entonces, dados dos equipos, devolver al ganador del partido.


-}

    ganadorPartido :: Equipo -> Equipo -> String
    ganadorPartido equipo1 equipo2 | promedioPuntosEquipo equipo1 > promedioPuntosEquipo equipo2 = nombreEquipo equipo1
                                   | otherwise = nombreEquipo equipo2

    promedioPuntosEquipo :: Equipo -> Float
    promedioPuntosEquipo equipo = (sum (map estadistica (take 5 (sortByCansancio (jugadores equipo)))) / 5)

    sortByCansancio :: [Jugador] -> [Jugador]
    sortByCansancio = sortBy (\jugador1 jugador2 -> compare (cansancio jugador1) (cansancio jugador2))

    -- Para probar:

    -- ganadorPartido bostonCeltics sanAntonioSpurs, me devuelve "San Antonio Spurs"
    -- ganadorPartido sanAntonioSpurs brooklynNets, me devuelve "San Antonio Spurs"
    -- ganadorPartido brooklynNets orlandoMagic, me devuelve "Orlando Magic"
    -- ganadorPartido orlandoMagic bostonCeltics, me devuelve "Orlando Magic"


-- PUNTO 8:

{-
    8. Se quiere hacer una seleccion representativa del Torneo. Solo puede haber un jugador por equipo. La lista de jugadores Super Estrellas esta formada por los primeros jugadores de cada 
    equipo. El primero de cada equipo es seleccionado con las siguientes cualidades: ser estrella y ser el menos cansado de cada equipo. 
    OJO: El jugador menos cansado puede que no sea estrella

-}

    superEstrellas :: Torneo -> [Jugador]
    superEstrellas torneo = map (jugadorMenosCansado . jugadores) (equipos torneo)

    jugadorMenosCansado :: [Jugador] -> Jugador
    jugadorMenosCansado = head . sortByCansancio

    -- Para probar:

    -- superEstrellas torneo, me devuelve [kyrieIrving, ginobiliEmanuel, masonPlumlee, khemBirch]

