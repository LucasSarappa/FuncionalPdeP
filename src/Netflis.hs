{-
Netflis: Sacando series y películas buenas del catálogo desde 1997.
 

A lo largo de los años, los streamings multimedia se han hecho más y más famosos, llegando incluso a superar la misma televisión. 
Netflis se ha consagrado como la empresa más importante de este rubro gracias a su gran catálogo y económico precio. 
En esta ocasión nos han pedido que modelemos las series que ofrecerán en su catálogo, para así darle una buena experiencia al usuario.

Sabemos que una serie se compone de un nombre, un género, una duración total en horas, una cantidad de temporadas, una calificación (en una escala de 5 estrellas) 
y si es original de Netflis.

Tenemos las siguientes definiciones sobre las cuales partir:

data Serie = UnaSerie {
    nombre :: String,
    genero :: String,
    duracion :: Int,
    cantTemporadas :: Int,
    calificaciones :: [Int],
    esOriginalDeNetflis :: Bool
} deriving (Eq, Show)
  

tioGolpetazo = UnaSerie {
    nombre = "One punch man",
    genero = "Monito chino",
    duracion = 24,
    cantTemporadas = 1,
    calificaciones = [5],
    esOriginalDeNetflis = False
}
 
cosasExtranias = UnaSerie {
    nombre = "Stranger things",
    genero = "Misterio",
    duracion = 50,
    cantTemporadas = 2,
    calificaciones = [3,3],
    esOriginalDeNetflis = True
}

dbs = UnaSerie {
    nombre = "Dragon ball supah",
    genero = "Monito chino",
    duracion = 150,
    cantTemporadas = 5,
    calificaciones = [],
    esOriginalDeNetflis = False
}


espejoNegro = UnaSerie {
    nombre = "Black mirror",
    genero = "Suspenso",
    duracion = 123,
    cantTemporadas = 4,
    calificaciones = [2],
    esOriginalDeNetflis = True
}

rompiendoMalo = UnaSerie {
    nombre = "Breaking Bad",
    genero = "Drama",
    duracion = 200,
    cantTemporadas = 5,
    calificaciones = [],
    esOriginalDeNetflis = False
}

treceRazonesPorque = UnaSerie {
    nombre = "13 reasons why",
    genero = "Drama",
    duracion = 50,
    cantTemporadas = 1,
    calificaciones = [3,3,3],
    esOriginalDeNetflis = True
}

Parte 1: Listas básicas
     
1) Crear una maratón con los ejemplos dados. Una maratón es una colección de series.

2) Saber la cantidad de series del maratón

3) Saber si una serie es popular: una serie se considera popular si recibió 3 o más calificaciones.

4) Averiguar si una serie vale la pena, es decir, si tiene más de una temporada y tiene 3 o más calificaciones.

5) Saber si una maratón vale la pena: una maratón vale la pena si la primera y la última serie de la maratón valen la pena, o bien si está el drama "Breaking Bad", con 5 temporadas y 200 minutos, sin calificar, que no es original de netflis.

6) Averiguar si una maratón repunta al final, lo que sucede si la primera mitad de las series constituyen una maratón que no valdría la pena, pero la segunda mitad sí. (si es cantidad impar de series de la maratón, la segunda mitad tiene una serie más que la primera)

7) Calcular la calificación de una serie. Es el promedio de las calificaciones recibidas, (redondeado hacia abajo) 

8) Obtener la dispersión de las calificaciones de la serie, que es la diferencia entre la mejor y peor calificación. (Si todas las calificaciones son coincidentes, se deduce que la dispersión es 0), 

9) Calificar una serie, que significa agregar una nueva calificación al final de las anteriores.

10) Hypear una serie: cuando se hypea una serie, se alteran la primer y última calificación recibida, aumentándola en 2 estrellas 
(recordá que la escala de calificación es de 5 estrellas máximo). Si la serie recibió alguna calificación de 1 estrella, no se puede hypear.

Parte 2: Jugando más con listas

1) Obtener todas las series que sean de monitos chinos (accion, suspenso) 

2) Obtener las series buenas no originales de Netflis

3) Obtener las series que tengan una cantidad n de temporadas

4) Saber si una maratón (una lista de series) es flojita: una maratón se considera flojita cuando todas sus series son de 1 temporada

5) Dada una maratón, saber cuánto tiempo se tarda en ver esa maratón completa

6) Actualizar la forma de saber si una maratón vale la pena: una maratón vale la pena si al menos una serie de la maratón vale la pena; o bien, si Breaking Bad forma parte de la maratón. 

7) Dada una maratón de series, saber la calificación más alta que se le dio a una serie original de Netflis

8) Dada una maratón de series, hypear las series que corresponda. Una serie debe ser hypeada si es de drama o de suspenso 

Parte 3: Alto orden
1) Se quieren obtener más promedios, en todos los casos, redondeados hacia abajo:

    a.  Obtener el promedio de duración de las series de una maratón (considerar duración por cantidad de temporadas).

    b.  Obtener la calificación de una maratón, que es el promedio de calificaciones de las series que componen la maratón. 

    c.  Obtener el promedio de calificaciones de una lista de maratones.

2) También se buscan algunos records:

    a.  La serie mejor calificada de una maratón

    b.  La serie más larga de una maratón

    c.  Dada una lista de maratones, encontrar la mejor de todas, que es la maratón que tiene la mejor calificación.

3) Aparecen los críticos de series, que se especializan en analizar y establecer calificaciones. Cada crítico tiene su preferencia respecto de cuáles series calificar y cómo hacerlo. 
Por ejemplo, están los siguientes

    a. D. Moleitor: Se especializa en series flojitas, elimina todas sus calificaciones mayores a 3, si hubiera alguna, y agrega una calificacion 1 al final. 

    b. Hypeador: A todas las series que se pueden hypear, las hypea (como se explicó anteriormente).

    c. Exquisito: Prefiere las series que valen la pena. Sustittuye todas sus calificaciones recibidas por un nueva lista con una única calificación, que es el promedio 
    de calificaciones más uno. 

    d. CualquierColectivoLoDejaBien: A todas las series les agrega una calificación de 5. 

    e. CríticoAnónimo: No le gusta hypear series, por lo que a todas las series que se pueden hypear, les resta 1 estrella.

    f. CríticoDeLaGente: A todas las series les agrega una calificación de 3.
-}

module Netflis where

    data Serie = UnaSerie {
        nombre :: String,
        genero :: String,
        duracion :: Int,
        cantTemporadas :: Int,
        calificaciones :: [Int],
        esOriginalDeNetflis :: Bool
    } deriving (Eq, Show)
  



    tioGolpetazo = UnaSerie {
        nombre = "One punch man",
        genero = "Monito chino",
        duracion = 24,
        cantTemporadas = 1,
        calificaciones = [5],
        esOriginalDeNetflis = False
    }
    
    cosasExtranias = UnaSerie {
        nombre = "Stranger things",
        genero = "Misterio",
        duracion = 50,
        cantTemporadas = 2,
        calificaciones = [3,3],
        esOriginalDeNetflis = True
    }

    dbs = UnaSerie {
        nombre = "Dragon ball supah",
        genero = "Monito chino",
        duracion = 150,
        cantTemporadas = 5,
        calificaciones = [],
        esOriginalDeNetflis = False
    }


    espejoNegro = UnaSerie {
        nombre = "Black mirror",
        genero = "Suspenso",
        duracion = 123,
        cantTemporadas = 4,
        calificaciones = [2],
        esOriginalDeNetflis = True
    }

    rompiendoMalo = UnaSerie {
        nombre = "Breaking Bad",
        genero = "Drama",
        duracion = 200,
        cantTemporadas = 5,
        calificaciones = [],
        esOriginalDeNetflis = False
    }

    treceRazonesPorque = UnaSerie {
        nombre = "13 reasons why",
        genero = "Drama",
        duracion = 50,
        cantTemporadas = 2,
        calificaciones = [3,3,3],
        esOriginalDeNetflis = True
    }

    -- Crear una maraton con los ejemplos dados. Una maraton es una coleccion de series:

    maraton :: [Serie]
    maraton = [tioGolpetazo, cosasExtranias, dbs, espejoNegro, rompiendoMalo, treceRazonesPorque]

    -- Saber la cantidad de series del maraton

    cantidadDeSeries :: [Serie] -> Int
    cantidadDeSeries = length -- seria cantidadDeSeries maraton = length maraton pero se saca maraton de ambos lados por el punto free.

    -- Saber si una serie es popular: una serie se considera popular si recibio 3 o mas calificaciones.
    esPopular :: Serie -> Bool
    esPopular serie = (length (calificaciones serie)) >= 3

    -- Averiguar si una serie vale la pena, es decir, si tiene más de una temporada y tiene 3 o más calificaciones.

    valeLaPena :: Serie -> Bool
    valeLaPena serie = (cantTemporadas serie) > 1 && (esPopular serie)

    -- Saber si una maraton vale la pena: una maraton vale la pena si la primera y la ultima serie de la maraton valen la pena, 
    -- o bien si esta el drama "Breaking Bad", con 5 temporadas y 200 minutos, sin calificar, que no es original de netflis.

    maratonValeLaPena :: [Serie] -> Bool
    maratonValeLaPena maraton = (valeLaPena (head maraton) && valeLaPena (last maraton)) || (tieneBreakingBad maraton)

    tieneBreakingBad :: [Serie] -> Bool
    tieneBreakingBad maraton = any esBreakingBad maraton

    esBreakingBad :: Serie -> Bool
    esBreakingBad serie = (nombre serie) == "Breaking Bad" && (cantTemporadas serie) == 5 && (duracion serie) == 200 && (length (calificaciones serie)) == 0 && (esOriginalDeNetflis serie) == False

-- Aveirguar si una maraton repunta al final, lo que sucede si la primera mitad de las series constituyen una maraton que no valdria la pena, pero la segunda mitad si.
-- (si es cantidad impar de series de la maraton, la segunda mitad tiene una serie mas que la primera)

    maratonRepuntaAlFinal :: [Serie] -> Bool
    maratonRepuntaAlFinal maraton = (valeLaPena (head (take (div (length maraton) 2) maraton)) == False) && (valeLaPena (last (take (div (length maraton) 2) maraton)) == True)

    -- Calcular la calificacion de una serie. Es el promedio de las calificaciones recibidas, (redondeado hacia abajo)

    calificacion :: Serie -> Int
    calificacion serie = div (sum (calificaciones serie)) (length (calificaciones serie))

    -- Obtener la dispersion de las calificaciones de la serie, que es la diferencia entre la mejor y peor calificacion. (Si todas las calificaciones son coincidentes, se deduce que la dispersion es 0)

    dispersion :: Serie -> Int
    dispersion serie = maximum (calificaciones serie) - minimum (calificaciones serie)

    -- Calificar una serie, que significa agregar una nueva calificacion al final de las anteriores.

    calificar :: Serie -> Int -> Serie
    calificar serie nuevaCalificacion = serie {calificaciones = (calificaciones serie) ++ [nuevaCalificacion]}

    -- Hypear una serie: cuando se hypea una serie, se alteran la primer y ultima calificacion recibida, aumentandola en 2 estrellas 
    -- (recorda que la escala de calificacion es de 5 estrellas maximo). Si la serie recibio alguna calificacion de 1 estrella, no se puede hypear.

    hypear :: Serie -> Serie
    hypear serie = serie {calificaciones = (hypearCalificaciones (calificaciones serie))}

    hypearCalificaciones :: [Int] -> [Int]
    hypearCalificaciones calificaciones = if (any (==1) calificaciones) then calificaciones else (hypearCalificacionesAux calificaciones)

    hypearCalificacionesAux :: [Int] -> [Int]
    hypearCalificacionesAux calificaciones = [(head calificaciones) + 2] ++ (init (tail calificaciones)) ++ [(last calificaciones) + 2]

-- PARTE 2:

    -- Obtener todas las series que sean de monitos chinos (accion, suspenso)

    seriesDeMonitosChinos :: [Serie] -> [Serie]
    seriesDeMonitosChinos maraton = filter esMonitoChino maraton

    esMonitoChino :: Serie -> Bool
    esMonitoChino serie = (genero serie) == "Monito chino"

    -- Obtener las series buenas no originales de Netflis

    seriesBuenasNoOriginalesDeNetflis :: [Serie] -> [Serie]
    seriesBuenasNoOriginalesDeNetflis maraton = filter esBuenaNoOriginalDeNetflis maraton

    esBuenaNoOriginalDeNetflis :: Serie -> Bool
    esBuenaNoOriginalDeNetflis serie = (valeLaPena serie) && (esOriginalDeNetflis serie == False)


    -- Obtener las series que tengan una cantidad n de temporadas

    seriesConNTemporadas :: Int -> [Serie] -> [Serie]
    seriesConNTemporadas n maraton = filter (tieneNTemporadas n) maraton

    tieneNTemporadas :: Int -> Serie -> Bool
    tieneNTemporadas n serie = (cantTemporadas serie) == n

    -- Saber si una maraton (una lista de series) es flojita: una maraton se considera flojita cuando todas sus series son de 1 temporada

    maratonFlojita :: [Serie] -> Bool
    maratonFlojita maraton = all esFlojita maraton

    esFlojita :: Serie -> Bool
    esFlojita serie = (cantTemporadas serie) == 1

    -- Dada una maraton, saber cuanto tiempo se tarda en ver esa maraton completa

    tiempoDeMaraton :: [Serie] -> Int
    tiempoDeMaraton maraton = sum (map duracion maraton)

    -- Actualizar la forma de saber si una maraton vale la pena: una maraton vale la pena si al menos una serie de la maraton vale la pena; o bien, si Breaking Bad forma parte de la maraton.

    maratonValeLaPena2 :: [Serie] -> Bool
    maratonValeLaPena2 maraton = any valeLaPena maraton || tieneBreakingBad maraton

    -- Dada una maraton de series, saber la calificacion mas alta que se le dio a una serie original de Netflis

    calificacionMasAltaOriginalDeNetflis :: [Serie] -> Int
    calificacionMasAltaOriginalDeNetflis maraton = maximum (map calificacion (filter esOriginalDeNetflis maraton))

    -- Dada una maraton de series, hypear las series que corresponda. Una serie debe ser hypeada si es de drama o de suspenso

    hypearMaraton :: [Serie] -> [Serie]
    hypearMaraton maraton = map hypear (filter esDramaOSuspenso maraton)

    esDramaOSuspenso :: Serie -> Bool
    esDramaOSuspenso serie = (genero serie) == "Drama" || (genero serie) == "Suspenso"

-- PARTE 3:

    -- Se quieren obtener mas promedios, en todos los casos, redondeados hacia abajo:

    -- a. Obtener el promedio de duracion de las series de una maraton (considerar duracion por cantidad de temporadas).

    promedioDeDuracion :: [Serie] -> Int
    promedioDeDuracion maraton = div (sum (map duracion maraton)) (sum (map cantTemporadas maraton))

    -- b. Obtener la calificacion de una maraton, que es el promedio de calificaciones de las series que componen la maraton.

    calificacionDeMaraton :: [Serie] -> Int
    calificacionDeMaraton maraton = div (sum (map calificacion maraton)) (length maraton)

    -- c. Obtener el promedio de calificaciones de una lista de maratones.

    promedioDeCalificacionesDeMaratones :: [[Serie]] -> Int
    promedioDeCalificacionesDeMaratones maratones = div (sum (map calificacionDeMaraton maratones)) (length maratones)

    -- Tambien se buscan algunos records:
{-
    -- a. La serie mejor calificada de una maraton

    mejorCalificada :: [Serie] -> Serie
    mejorCalificada maraton = maximum (map calificacion maraton)

    
    
    -- b. La serie mas larga de una maraton

    masLarga :: [Serie] -> Serie
    masLarga maraton = maximum (map duracion maraton)


    
    -- c. Dada una lista de maratones, encontrar la mejor de todas, que es la maraton que tiene la mejor calificacion.

    mejorMaraton :: [[Serie]] -> [Serie]
    mejorMaraton maratones = maximum (map calificacionDeMaraton maratones)
-}    
    -- Aparecen los criticos de series, que se especializan en analizar y establecer calificaciones. Cada critico tiene su preferencia respecto de cuales series calificar y como hacerlo.

    -- a. D. Moleitor: Se especializa en series flojitas, elimina todas sus calificaciones mayores a 3, si hubiera alguna, y agrega una calificacion 1 al final.

    moleitor :: [Serie] -> [Serie]
    moleitor maraton = map calificarConMoleitor (filter esFlojita maraton)

    calificarConMoleitor :: Serie -> Serie
    calificarConMoleitor serie = serie {calificaciones = (calificarConMoleitorAux (calificaciones serie))}

    calificarConMoleitorAux :: [Int] -> [Int]
    calificarConMoleitorAux calificaciones = if (any (>3) calificaciones) then [1] else calificaciones

    -- b. Hypeador: A todas las series que se pueden hypear, las hypea (como se explico anteriormente).

    hypeador :: [Serie] -> [Serie]
    hypeador maraton = map hypear (filter sePuedeHypear maraton)

    sePuedeHypear :: Serie -> Bool
    sePuedeHypear serie = (genero serie) == "Drama" || (genero serie) == "Suspenso"

    -- c. Exquisito: Prefiere las series que valen la pena. Sustituye todas sus calificaciones recibidas por un nueva lista con una unica calificacion, que es el promedio de calificaciones mas uno.

    exquisito :: [Serie] -> [Serie]
    exquisito maraton = map calificarConExquisito (filter valeLaPena maraton)

    calificarConExquisito :: Serie -> Serie
    calificarConExquisito serie = serie {calificaciones = [((calificacion serie) + 1)]}

    -- d. CualquierColectivoLoDejaBien: A todas las series les agrega una calificacion de 5.

    cualquierColectivoLoDejaBien :: [Serie] -> [Serie]
    cualquierColectivoLoDejaBien maraton = map calificarConCualquierColectivoLoDejaBien maraton

    calificarConCualquierColectivoLoDejaBien :: Serie -> Serie
    calificarConCualquierColectivoLoDejaBien serie = serie {calificaciones = (calificaciones serie) ++ [5]}

    -- e. CriticoAnonimo: No le gusta hypear series, por lo que a todas las series que se pueden hypear, les resta 1 estrella.

    criticoAnonimo :: [Serie] -> [Serie]
    criticoAnonimo maraton = map restarUnaEstrella (filter sePuedeHypear maraton)

    restarUnaEstrella :: Serie -> Serie
    restarUnaEstrella serie = serie {calificaciones = (restarUnaEstrellaAux (calificaciones serie))}

    restarUnaEstrellaAux :: [Int] -> [Int]
    restarUnaEstrellaAux calificaciones = map (restarUno) calificaciones

    restarUno :: Int -> Int
    restarUno calificacion = calificacion - 1

    -- f. CriticoDeLaGente: A todas las series les agrega una calificacion de 3.

    criticoDeLaGente :: [Serie] -> [Serie]
    criticoDeLaGente maraton = map calificarConTres maraton

    calificarConTres :: Serie -> Serie
    calificarConTres serie = serie {calificaciones = (calificaciones serie) ++ [3]}


