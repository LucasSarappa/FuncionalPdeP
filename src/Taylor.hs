{-
Nos piden desarrollar un sistema para administrar las escuchas de canciones que se realizan semanalmente de las playlist mas exitosas del mundo.

Para eso necesitamos modelar el perfil de cada oyente que participa, asi como tambien las caracteristicas de las diversas categorias y el modo en que estas afectan los sentimientos de las personas.

De cada Persona nos interesa registrar de que artista es fanatico, el tiempo que tiene disponible para escuchar musica y su estado de animo.

Tambien sabemos que las playlist se componen de canciones de distintas categorias, cada una de las cuales pertenece a distintas categorias. La categoria de la cancion influencia en 
gran medida la forma en que cada persona puede escucharla.

Para representar estos datos, elegimos utilizar las sigueintes estructuras:

data Persona = Persona {
    tiempoLibre :: Int,
    estadoDeAnimo :: String,
    fanDe :: [Artista]
} deriving Show

type Artista = String

type Playlist = [Cancion]

data Cancion = Cancion {
    duracion :: Int,
    categoria :: Categoria
} deriving Show


Teniendo en cuenta lo descrito anteriormente, se pide modelar los siguientes puntos explicitando el tipo de cada funcion y utilizando los conceptos de composicion y aplicacion parcial en cada caso:

1. Modelar una funcion para que una persona disminuya en minutos su tiempo libre, y otra para que, dada una funcion Int -> Int que representa un cambio de estado de animo, actualice el estado de una persona
con el resultado de aplicar el cambio al esatdo de animo actual.

2. Definir el tipo Categoria, de forma tal que resulte adecuado para representar los diversos estados de animo que las personas atraviesan al escuchar una cancion.

    a. Modelar las siguientes categorias:

        musicaClasica: Esta categoria es ideal para escuchar ya que mantienen serena a las personas, y no las impacta al animo de ninguna forma.

        pop: Los ritmos energicos y pegadizos de este genero obligan a las personas a subir su estado de animo un 25%

        desamor: Puede afectar a las personas de diversas formas. Si la intensidad de la categoria es menor de 50 puntos, las personas son capaces de escucharla perdiendo dos puntos
        de animo por cada punto de intensidad. Por otro lado, si la intensidad es mayor, los oyentes no se animan a escucharla y, en cambio pierden 5 minutos de tiempo libre buscando otra cosa
        que escuchar. Cabe destacar que estas penalidades no afectan a las perosnas felices, que penden esuichar cualquier tipo de cancion sin sufrir. Una persona feliz es aquella que
        tiene mas de 100 putnos de estado de animo.

        taylorSwift: Mezlcan todos los aspectos de las categorias de desamor y las pop. Lo impacta igual que la categoria pop y la de desamor con 20 puntos de intensidad (en ese orden). Ademas
        de eso, fana un punto extra de estado de animo, solo porque esta catefgoria es muy conmovedora.

    
    b. Modelar una funcion escucharCanion que, dada una persona y una cancion, haga que la persona disminuya su tiempo libre escuchando la cancion, luego de haber sido afectada 
    por la categoria de la cancion.

    c. Dar un ejemplo de una playlist que contenga canciones con todas las categorias descritas anteriormente y una mas, inventada por vos, con un comentario que describa que cambios significa



3. Definir funciones que, dada una lista de Personas:

    a. Indique cual es el mayor tiempo libre de las personas que todavia pueden escuchar canciones (es decir, aquellas cuyo estado de animo es > 0)

    b. Dada una lista de artistas, indique si todas las personas tienen todavia mas de treinta minutos libres son fanaticos de algun artista de la lista

    c. Dado una cancion y una persona ociosa, indique cantas de las personas de la lista pueden escuchar dicha cancion y terminar con mas tiempo libre que dicha persona.


4. Modelar la funicon escucharPlaylist :: Playlist -> [Persona] -> [Persona] , que reciba una lista de personas y una playlist y retorne el podio resultante de la escucha, es decir,
los tres oyentes que escuchar todas las canciones de la playulist (en orden) y tienen el mayor estado de animo.

-}


module Taylor where

    import Data.List (sortOn)
    import Data.List (intersect)
    import Data.List (maximumBy)
    import Data.Ord (comparing)
    import Data.List (minimumBy)


    data Persona = UnaPersona{
        fanDe :: [Artista],
        tiempoLibre :: Int ,
        estadoAnimico :: Int
    }


    type Artista = String

    type Playlist = [Cancion]

    type Categoria= Persona -> Persona

    data Cancion = UnaCancion{
        duracion :: Int,
        categoria :: Categoria
    }


    instance Show Persona where
        show persona = "Fan de: " ++ show (fanDe persona) ++ " | Tiempo libre: " ++ show (tiempoLibre persona) ++ " | Estado animico: " ++ show (estadoAnimico persona)


-- Persona de ejemplo para pruebas
    lucas :: Persona
    lucas = UnaPersona ["Taylor Swift"] 100 100

    alex :: Persona
    alex = UnaPersona ["Taylor Swift"] 50 50

    nicolas :: Persona
    nicolas = UnaPersona ["Taylor Swift"] 25 1

    eugenia :: Persona
    eugenia = UnaPersona ["Taylor Swift"] 18 2


-- En todos los puntos se debe cumplir con los conceptos de composicion y aplicacion parcial
-- Punto 1: 

{-
    1. Modelar una funcion para que una persona disminuya en minutos su tiempo libre, y otra para que, dada una funcion Int -> Int que representa un cambio de estado de animo, 
    actualice el estado de una persona con el resultado de aplicar el cambio al esatdo de animo actual.
-}

    -- Funcion para que una persona disminuya en minutos su tiempo libre:

    disminuirTiempoLibre :: Int -> Persona -> Persona
    disminuirTiempoLibre minutos = modificarTiempoLibre (subtract minutos)

    modificarTiempoLibre :: (Int -> Int) -> Persona -> Persona
    modificarTiempoLibre cambio persona = persona {tiempoLibre = cambio (tiempoLibre persona)}

    -- Funcion para que, dada una funcion Int -> Int que representa un cambio de estado de animo, actualice el estado de una persona con el resultado de aplicar el cambio al estado 
    -- de animo actual.

    actualizarEstadoAnimo :: (Int -> Int) -> Persona -> Persona 
    actualizarEstadoAnimo cambio = modificarEstadoAnimo cambio

    modificarEstadoAnimo :: (Int -> Int) -> Persona -> Persona
    modificarEstadoAnimo cambio persona = persona {estadoAnimico = cambio (estadoAnimico persona)}


    -- Para probar:
    -- disminuirTiempoLibre 10 lucas, me devuelve: UnaPersona {fanDe = ["Taylor Swift"], tiempoLibre = 90, estadoAnimico = 100}

    -- actualizarEstadoAnimo (subtract 10) lucas, me devuelve: UnaPersona {fanDe = ["Taylor Swift"], tiempoLibre = 100, estadoAnimico = 90
    -- Otra forma de probar actualizarEstadoAnimo es con: actualizarEstadoAnimo (+10) lucas, me devuelve: UnaPersona {fanDe = ["Taylor Swift"], tiempoLibre = 100, estadoAnimico = 110}

{-
    2. Definir el tipo Categoria, de forma tal que resulte adecuado para representar los diversos estados de animo que las personas atraviesan al escuchar una cancion.

    a. Modelar las siguientes categorias:

        musicaClasica: Esta categoria es ideal para escuchar ya que mantienen serena a las personas, y no las impacta al animo de ninguna forma.
        pop: Los ritmos energicos y pegadizos de este genero obligan a las personas a subir su estado de animo un 25%
        desamor: Puede afectar a las personas de diversas formas. Si la intensidad de la categoria es menor de 50 puntos, las personas son capaces de escucharla perdiendo dos puntos
        de animo por cada punto de intensidad. Por otro lado, si la intensidad es mayor, los oyentes no se animan a escucharla y, en cambio pierden 5 minutos de tiempo libre buscando otra cosa
        que escuchar. Cabe destacar que estas penalidades no afectan a las perosnas felices, que penden esuichar cualquier tipo de cancion sin sufrir. Una persona feliz es aquella que
        tiene mas de 100 putnos de estado de animo.
        taylorSwift: Mezlcan todos los aspectos de las categorias de desamor y las pop. Lo impacta igual que la categoria pop y la de desamor con 20 puntos de intensidad (en ese orden). Ademas
        de eso, gana un punto extra de estado de animo, solo porque esta catefgoria es muy conmovedora.

-}

    -- Modelar las siguientes categorias:


    musicaClasica :: Categoria
    musicaClasica = disminuirTiempoLibre 0 -- Aca estamos aplicando point free
    -- Otra forma de hacerlo es: musicaClasica persona = disminuirTiempoLibre 0 persona, sin embargo, es mas prolijo hacerlo de la forma anterior.
    -- (lo mismo pasa en las otras categorias, pero no lo voy a repetir en todas)

    -- la categoria pop obliga a las personas a subir su estado de animo un 25%
    pop :: Categoria
    pop = actualizarEstadoAnimo (round . (*1.25) . fromIntegral)  -- esto es porque la funcion Int -> Int espera un Int de vuelta y aca estoy multiplicando por un Float.

    -- la categoria desamor en resumen es: si la intensidad de la categoria es menor de 50 puntos, las personas son capaces de escucharla perdiendo dos puntos. 
    -- Pero si la intensidad es mayor, los oyentes no se animan a escucharla y, en cambio pierden 5 minutos de tiempo libre buscando otra cosa que escuchar.
    -- Cabe destacar que estas penalidades no afectan a las perosnas felices, que penden esuichar cualquier tipo de cancion sin sufrir. 
    -- Una persona feliz es aquella que tiene mas de 100 putnos de estado de animo.

    desamor :: Int -> Categoria
    desamor intensidad persona
        | estadoAnimico persona >= 100 = actualizarEstadoAnimo id persona
        | intensidad < 50 = actualizarEstadoAnimo (subtract (2 * intensidad)) persona
        | otherwise = disminuirTiempoLibre 5 persona
    
    -- la categoria taylorSwift en resumen es: 
    -- Mezlcan todos los aspectos de las categorias de desamor y las pop. Lo impacta igual que la categoria pop y la de desamor con 20 puntos de intensidad (en ese orden). 
    -- Ademas de eso, gana un punto extra de estado de animo, solo porque esta catefgoria es muy conmovedora.

    taylorSwift :: Int -> Categoria
    taylorSwift intensidad =  (pop . desamor 20 ). actualizarEstadoAnimo (+1) -- Lo que hace esto es aplicar la categoria pop, luego la categoria desamor con 20 de intensidad y por ultimo le suma 1 al estado de animo.
    -- Por ejemplo, si la intensidad es 20, primero se aplica la categoria pop, que le suma un 25% al estado de animo, luego se aplica la categoria desamor con 20 de intensidad, 
    -- que le resta 40 al estado de animo, y por ultimo se le suma 1 al estado de animo. 

    -- lucas tiene: UnaPersona {fanDe = ["Taylor Swift"], tiempoLibre = 100, estadoAnimico = 100}
    -- Para probar:

    -- pop lucas, me devuelve: UnaPersona {fanDe = ["Taylor Swift"], tiempoLibre = 100, estadoAnimico = 125}

    -- desamor 40 lucas, me devuelve: UnaPersona {fanDe = ["Taylor Swift"], tiempoLibre = 100, estadoAnimico = 100} ya que la intensidad es 100 por lo tanto es feliz y no le afecta la categoria desamor.
    -- desamor 40 alex, me devuelve: UnaPersona {fanDe = ["Taylor Swift"], tiempoLibre = 50, estadoAnimo= -30} ya que la intensidad es 40 por lo tanto le resta 80 al estado de animo.
    
    -- taylorSwift 20 lucas, me devuelve: UnaPersona {fanDe = ["Taylor Swift"], tiempoLibre = 100, estadoAnimico = 126} -- desamor no le afecta ya que es feliz.
    -- taylorSwift 20 alex, me devuelve: UnaPersona {fanDe = ["Taylor Swift"], tiempoLibre = 50, estadoAnimico = 14} -- primero se aplica 

{-
    b. Modelar una funcion escucharCanion que, dada una persona y una cancion, haga que la persona disminuya su tiempo libre escuchando la cancion, luego de haber sido afectada 
    por la categoria de la cancion.

-}

    -- Modelar una funcion escucharCancion que, dada una persona y una cancion, haga que la persona disminuya su tiempo libre escuchando la cancion, luego de haber sido afectada 
    -- por la categoria de la cancion.

    escucharCancion :: Persona -> Cancion -> Persona
    escucharCancion persona cancion = flip disminuirTiempoLibre persona (duracion cancion)

    -- Canciones de ejemplo para probar:
    cancion1 :: Cancion
    cancion1 = UnaCancion 10 musicaClasica

    cancion2 :: Cancion
    cancion2 = UnaCancion 10 pop

    cancion3 :: Cancion
    cancion3 = UnaCancion 10 (desamor 40)

    -- para probar:
    -- escucharCancion lucas cancion1, me devuelve: UnaPersona {fanDe = ["Taylor Swift"], tiempoLibre = 90, estadoAnimico = 100}


{-
    c. Dar un ejemplo de una playlist que contenga canciones con todas las categorias descritas anteriormente y una mas, inventada por vos, con un comentario que describa que cambios significa
-}

    -- Playlist de ejemplo:
    playlist :: Playlist
    playlist = [cancion1, cancion2, cancion3, cancion4]

    cancion4 :: Cancion
    cancion4 = UnaCancion 10 (taylorSwift 20)

    -- La cancion4 es de la categoria taylorSwift con 20 de intensidad, por lo tanto, primero se aplica la categoria pop, que le suma un 25% al estado de animo, luego se aplica la categoria desamor con 20 de intensidad, 
    -- que le resta 40 al estado de animo, y por ultimo se le suma 1 al estado de animo. 

    -- Para probar:
    -- escucharCancion lucas cancion4, me devuelve: UnaPersona {fanDe = ["Taylor Swift"], tiempoLibre = 90, estadoAnimico = 126}

{-
    3. Definir funciones que, dada una lista de Personas:

    a. Indique cual es el mayor tiempo libre de las personas que todavia pueden escuchar canciones (es decir, aquellas cuyo estado de animo es > 0)

-}

    mayorTiempoLibre :: [Persona] -> Int
    mayorTiempoLibre = maximum . map tiempoLibre . filter ((>0) . estadoAnimico)

    -- Para probar:
    -- mayorTiempoLibre [lucas, alex], me devuelve: 100

    -- mayorTiempoLibre [nicolas, eugenia], me devuelve: 0


{-

    b. Dada una lista de artistas, indique si todas las personas tienen todavia mas de treinta minutos libres son fanaticos de algun artista de la lista
-}


    fanaticosDeArtistas2 :: [Artista] -> [Persona] -> Bool
    fanaticosDeArtistas2 artistas  = all tieneMasDe30MinutosLibres . filter (esFanaticoDeAlgunArtista artistas)

    tieneMasDe30MinutosLibres :: Persona -> Bool
    tieneMasDe30MinutosLibres = (>30) . tiempoLibre

    esFanaticoDeAlgunArtista :: [Artista] -> Persona -> Bool
    esFanaticoDeAlgunArtista artistas = flip elem artistas . head . fanDe


    artistas1 :: [Artista]
    artistas1 = ["Taylor Swift", "Ariana Grande"]

    artistas2 :: [Artista]
    artistas2 = ["Taylor Swift"]

    personas1 :: [Persona]
    personas1 = [lucas, alex, nicolas, eugenia]

    personas2 :: [Persona]
    personas2 = [lucas, alex]

    -- Para probar:
    -- fanaticosDeArtistas listaDeArtistas listaDePersonas, me devuelve: False, ya que no todas las personas tienen mas de 30 minutos libres y son fanaticos de algun artista de la lista.
    -- fanaticosDeArtistas listaDeArtistas2 listaDePersonas2, me devuelve: True, ya que todas las personas tienen mas de 30 minutos libres y son fanaticos de algun artista de la lista.


{-
    c. Dado una cancion y una persona ociosa, indique cuantas de las personas de la lista pueden escuchar dicha cancion y terminar con mas tiempo libre que dicha persona.
-}
    
    personasQuePuedenEscuchar :: Cancion -> Persona -> [Persona] -> Int
    personasQuePuedenEscuchar cancion persona = length . filter (terminarConMasTiempoLibreQue persona) . map (flip escucharCancion cancion)

    terminarConMasTiempoLibreQue :: Persona -> Persona -> Bool
    terminarConMasTiempoLibreQue persona1 persona2 = tiempoLibre persona2 > tiempoLibre persona1


    -- recordemos que lucas tiene 100 de tiempo libre, alex 50, nicolas 25 y eugenia 18
    -- si escuchan la cancion1, termina con 90 de tiempo libre, alex con 40, nicolas con 15 y eugenia con 8



    -- Para probar:
    -- personasQuePuedenEscuchar cancion1 lucas personas1, me devuelve: 0, ya que ninguna persona puede escuchar la cancion1 y terminar con mas tiempo libre que lucas.
    -- personasQuePuedenEscuchar cancion1 euqenia personas2, me devuelve: 2, ya que 2 personas pueden escuchar la cancion1 y terminar con mas tiempo libre que eugenia (18), que son lucas y alex.


{-

    4. Modelar la funicon escucharPlaylist :: Playlist -> [Persona] -> [Persona] , que reciba una lista de personas y una playlist y retorne el podio resultante de la escucha, es decir,
    los tres oyentes que escuchar todas las canciones de la playlist (en orden) y tienen el mayor estado de animo.

-}

    escucharPlaylist :: Playlist -> [Persona] -> [Persona]
    escucharPlaylist playlist = take 3 . sortOn (negate . estadoAnimico) . filter (escuchoTodasLasCanciones playlist)

    escuchoTodasLasCanciones :: Playlist -> Persona -> Bool
    escuchoTodasLasCanciones playlist persona = all (\cancion -> escuchoCancion persona playlist cancion) playlist

    escuchoCancion :: Persona -> Playlist -> Cancion -> Bool
    escuchoCancion persona playlist cancion = estadoAnimico (escucharCancion persona cancion) > 0

    -- Para probar:

    -- escucharPlaylist playlist [lucas, alex, nicolas, eugenia], me devuelve:
    -- [ Fan de: ["Taylor Swift"] | Tiempo libre: 100 | Estado animico: 100, (este seria Lucas)
    --   Fan de: ["Taylor Swift"] | Tiempo libre: 50 | Estado animico: 50, (este seria Alex)
    --   Fan de: ["Taylor Swift"] | Tiempo libre: 18 | Estado animico: 2  (este seria eugenia)      ]