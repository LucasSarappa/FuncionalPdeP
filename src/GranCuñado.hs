{-
GRAN CUÑADO - Paradigmas de Programación - Parcial Funcional 10/06/2023 - Curso K2033

¿Qué hay más divertido que encerrar 18 personas en una casa sin internet ni forma de comunicarse con la gente de afuera? Se nos ocurren muchas cosas, pero no
nos pagan para eso, nos pagan para hacer un sistema para simular quién será el ganador.

Lo que sabemos es que los participantes tienen un nombre, una edad, un nivel de atractivo, un nivel de personalidad y un nivel de inteligencia 
(todos números decimales en un principio). 

Además, los participantes tienen un criterio de voto, para determinar a quién nominar.

Las pruebas semanales son eventos que tienen un criterio para ser superadas, pero también tienen un índice de éxito (decimal) que es un número entre 0 y 100 y 
determina qué tan bien se supera la prueba. Si la prueba no se supera, el índice de éxito es 0.


    1. Las pruebas que conocemos son:

        a. baileDeTikTok: requiere una personalidad de 20. El índice de éxito se calcula como la personalidad + el doble del atractivo del participante.

        b. botonRojo: requiere una personalidad de 10 y una inteligencia de 20. El índice de éxito es siempre 100.

        c. cuentasRapidas: requiere una inteligencia de 40. El índice de exito se calcula como la inteligencia + la personalidad - el atractivo.


    2. Para superar una prueba se tiene que cumplir con el criterio requerido por cada una. Pero además se puede saber con qué índice de éxito se supera cada prueba. 
    Se desea saber:

        a. Dado un grupo de participantes y una prueba, quiénes la superan.

        b. Dado un grupo de participantes y una prueba, el promedio del índice de éxito.

        c. Dado un participante y un conjunto de pruebas, saber si ese participante es favorito, esto se cumple cuando supera todas las pruebas con un índice mayor a 50.


    3. Los criterios de voto determinan a quién va a votar un participante, dada una lista de participantes, según el criterio se debe votar a una sola persona. 
    Por ahora, conocemos tres criterios

        a. Menos inteligente: el que menos nivel de inteligencia tenga

        b. Más atractivo: el que más nivel de atractivo tenga

        c. Más viejo: el que más edad tenga.


    4. Conozcamos a nuestros participantes:

        a. Javier Tulei que tiene 52 años, 30 de nivel de atractivo, 70 de personalidad y 35 de inteligencia. Como criterio vota al menos inteligente.

        b. Mínimo Kirchner que tiene 46 años, 0 de nivel de atractivo, 40 de personalidad y 50 de inteligencia. Como criterio vota al más atractivo.

        c. Horacio Berreta que tiene 57 años, 10 de nivel de atractivo, 60 de personalidad y 50 de inteligencia. Como criterio vota al más atractivo.

        d. Myriam Bregwoman que tiene 51 años, 40 de nivel de atractivo, 40 de personalidad y 60 de inteligencia. Como criterio vota al más viejo.

        

    5. Luego de votar, nos interesa saber quiénes están en placa, esos son todos los participantes que, al menos, una persona votó.


    6. Con la placa ya publicada nos interesa saber para un participante:

        a. Está en el horno, esto se cumple cuando recibió tres votos o más.

        b. Hay algo personal, cuando es la única persona en placa.

        c. Zafó, que se cumple cuando no está en placa.


Tip:
instance Eq Participante where
(==) participante otroParticipante = ...

Notas:
● Definir el tipo de todas las funciones principales.
● No duplicar lógica.
● No utilizar recursividad a menos que se lo indique.
● Utilizar e identificar (al menos una vez) adecuadamente los siguientes
conceptos:
○ Composición
○ Aplicación Parcial
○ Orden Superior

-}

module GranCuñado where

    import Data.List (maximumBy)
    import Data.List (minimumBy)
    import Data.List (nub)
    import Data.Ord (comparing)
   
   {-
   
   Lo que sabemos es que los participantes tienen un nombre, una edad, un nivel de atractivo, un nivel de personalidad y un nivel de inteligencia 
   (todos números decimales en un principio). 
   
   Además, los participantes tienen un criterio de voto, para determinar a quién nominar.
   
   Las pruebas semanales son eventos que tienen un criterio para ser superadas, pero también tienen un índice de éxito (decimal) que es un número entre 0 y 100 y 
   determina qué tan bien se supera la prueba. Si la prueba no se supera, el índice de éxito es 0.
      
   -}
    data Participante = Participante {
        nombre :: String,
        edad :: Int,
        atractivo :: Int,
        personalidad :: Int,
        inteligencia :: Int,
        criterioVoto :: [Participante] -> Participante        
    } 

    
    data Prueba = Prueba {
        criterioSuperacion :: Participante -> Bool,
        indiceExito :: Participante -> Int        
    }


    instance Show Participante where
        show participante = "Participante: " ++ nombre participante ++ " - Edad: " ++ show (edad participante) ++ " - Atractivo: " ++ show (atractivo participante) ++ " - Personalidad: " ++ show (personalidad participante) ++ " - Inteligencia: " ++ show (inteligencia participante)
    instance Eq Participante where
        (==) participante otroParticipante = nombre participante == nombre otroParticipante
    
    {-
    4. Conozcamos a nuestros participantes:

        a. Javier Tulei que tiene 52 años, 30 de nivel de atractivo, 70 de personalidad y 35 de inteligencia. Como criterio vota al menos inteligente.

        b. Mínimo Kirchner que tiene 46 años, 0 de nivel de atractivo, 40 de personalidad y 50 de inteligencia. Como criterio vota al más atractivo.

        c. Horacio Berreta que tiene 57 años, 10 de nivel de atractivo, 60 de personalidad y 50 de inteligencia. Como criterio vota al más atractivo.

        d. Myriam Bregwoman que tiene 51 años, 40 de nivel de atractivo, 40 de personalidad y 60 de inteligencia. Como criterio vota al más viejo.

    -}
        
    javierTulei :: Participante
    javierTulei = Participante {
        nombre = "Javier Tulei",
        edad = 52,
        atractivo = 30,
        personalidad = 70,
        inteligencia = 35,
        criterioVoto = masViejo
    
    }

    minimoKirchner :: Participante
    minimoKirchner = Participante {
        nombre = "Mínimo Kirchner",
        edad = 46,
        atractivo = 0,
        personalidad = 40,
        inteligencia = 50,
        criterioVoto = masViejo
    }

    horacioBerreta :: Participante
    horacioBerreta = Participante {
        nombre = "Horacio Berreta",
        edad = 57,
        atractivo = 10,
        personalidad = 60,
        inteligencia = 50,
        criterioVoto = masViejo
    }

    myriamBregwoman :: Participante
    myriamBregwoman = Participante {
        nombre = "Myriam Bregman",
        edad = 51,
        atractivo = 40,
        personalidad = 40,
        inteligencia = 60,
        criterioVoto = masViejo
    }




{-
        1. Las pruebas que conocemos son:

        a. baileDeTikTok: requiere una personalidad de 20. El índiceExito se calcula como la personalidad + el doble del atractivo del participante.

        b. botonRojo: requiere una personalidad de 10 y una inteligencia de 20. El índice de éxito es siempre 100.

        c. cuentasRapidas: requiere una inteligencia de 40. El índice de exito se calcula como la inteligencia + la personalidad - el atractivo.

-}

    baileDeTikTok :: Prueba
    baileDeTikTok = Prueba {
        criterioSuperacion = superacionBaile,
        indiceExito = calcularExitoBaile
    }

    botonRojo :: Prueba
    botonRojo = Prueba {
        criterioSuperacion = superacionBoton,
        indiceExito = calcularExitoBoton
    }

    cuentasRapidas :: Prueba
    cuentasRapidas = Prueba {
        criterioSuperacion = superacionCuentas,
        indiceExito = calcularExitoCuentas
    }

    superacionBaile :: Participante -> Bool
    superacionBaile participante = personalidad participante > 20

    calcularExitoBaile :: Participante -> Int
    calcularExitoBaile participante = personalidad participante + (atractivo participante * 2)

    calcularExitoBoton :: Participante -> Int
    calcularExitoBoton participante = 100
    
    superacionBoton :: Participante -> Bool
    superacionBoton participante = personalidad participante > 10 && inteligencia participante > 20

    superacionCuentas :: Participante -> Bool
    superacionCuentas participante = inteligencia participante > 40

    calcularExitoCuentas :: Participante -> Int
    calcularExitoCuentas participante = inteligencia participante + personalidad participante - atractivo participante


    {-
    
    2. Para superar una prueba se tiene que cumplir con el criterio requerido por cada una. Pero además se puede saber con qué índice de éxito se supera cada prueba. 
    Se desea saber:

        a. Dado un grupo de participantes y una prueba, quiénes la superan.

        b. Dado un grupo de participantes y una prueba, el promedio del índice de éxito.

        c. Dado un participante y un conjunto de pruebas, saber si ese participante es favorito, esto se cumple cuando supera todas las pruebas con un índice mayor a 50.
    
    
    -}

    superanPrueba :: [Participante] -> Prueba -> [Participante]
    superanPrueba participantes prueba = filter (criterioSuperacion prueba) participantes

    promedioExito :: [Participante] -> Prueba -> Int
    promedioExito participantes prueba = div (sum (map (indiceExito prueba) (superanPrueba participantes prueba))) (length (superanPrueba participantes prueba))

    esFavorito :: Participante -> [Prueba] -> Bool
    -- esFavorito participante pruebas = all (\prueba -> (indiceExito prueba participante) > 50) pruebas 

    -- Otra forma de hacer el esFavorito sin usar lambda

    esFavorito participante pruebas = all (flip esFavoritoAux participante) pruebas

    esFavoritoAux :: Prueba -> Participante -> Bool
    esFavoritoAux prueba participante = (indiceExito prueba participante) > 50

    {-

    -- Para probar:
    -- esFavorito javierTulei [baileDeTikTok, botonRojo, cuentasRapidas] -- True
    -- esFavorito minimoKirchner [baileDeTikTok, botonRojo, cuentasRapidas] -- False
    -- esFavorito horacioBerreta [baileDeTikTok, botonRojo, cuentasRapidas] -- False
    -- esFavorito myriamBregwoman [baileDeTikTok, botonRojo, cuentasRapidas] -- False
    -- esFavorito javierTulei [baileDeTikTok, botonRojo] -- False
    -- esFavorito javierTulei [baileDeTikTok] -- True
    -- esFavorito javierTulei [botonRojo] -- True
    -- esFavorito javierTulei [cuentasRapidas] -- True
    -- superanPrueba [javierTulei, minimoKirchner, horacioBerreta, myriamBregwoman]  -- baileDeTikTok 
        -- [Participante: Javier Tulei - Edad: 52.0 - Atractivo: 30.0 - Personalidad: 70.0 - Inteligencia: 35.0: Participante: Myriam Bregman - Edad: 51.0 - Atractivo: 40.0 - Personalidad: 40.0 - Inteligencia: 60.0: Participante: Horacio Berreta - Edad: 57.0 - Atractivo: 10.0 - Personalidad: 60.0 - Inteligencia: 50.0:]



    -}


{-
    3. Los criterios de voto determinan a quién va a votar un participante, dada una lista de participantes, según el criterio se debe votar a una sola persona. 
    Por ahora, conocemos tres criterios

        a. Menos inteligente: el que menos nivel de inteligencia tenga

        b. Más atractivo: el que más nivel de atractivo tenga

        c. Más viejo: el que más edad tenga.

-}


    menosInteligente :: [Participante] -> Participante
    menosInteligente participante = minimumBy (comparing inteligencia) participante

    masAtractivo :: [Participante] -> Participante
    masAtractivo participante = maximumBy (comparing atractivo) participante

    masViejo :: [Participante] -> Participante
    masViejo participantes = maximumBy (comparing edad) participantes

{-
    realizarVotacion :: Participante -> [Participante] -> Participante
    realizarVotacion participante participantes = criterioVoto participante participantes

-}

    -- otra opcion para realizarVotacion. Dada una lista de participantes que van a votar y una lista de participantes a votar, devuelve una lista con los participantes votados por cada votante.

    realizarVotacion :: [Participante] -> [Participante] -> [Participante]
    realizarVotacion votantes participantes = map (\votante -> criterioVoto votante participantes) votantes

    votantes :: [Participante]
    votantes = [javierTulei, minimoKirchner, horacioBerreta, myriamBregwoman]

    participantes :: [Participante]
    participantes = [javierTulei, minimoKirchner, horacioBerreta, myriamBregwoman]

    -- Para probar:
    -- realizarVotacion votantes participantes y devuelve:
    -- [Participante: Horacio Berreta - Edad: 57 - Atractivo: 10 - Personalidad: 60 - Inteligencia: 50,Participante: Horacio Berreta - Edad: 57 - Atractivo: 10 - Personalidad: 60 - Inteligencia: 50,Participante: Horacio Berreta - Edad: 57 - Atractivo: 10 - Personalidad: 60 - Inteligencia: 50,Participante: Horacio Berreta - Edad: 57 - Atractivo: 10 - Personalidad: 60 - Inteligencia: 50]









{-

    5. Luego de votar, nos interesa saber quiénes están en placa, esos son todos los participantes que, al menos, una persona votó.

-}

    enPlaca :: [Participante] -> [Participante] -> [Participante]
    enPlaca participantes votantes = nub (realizarVotacion votantes participantes)

    -- Ejemplo de uso:
    -- enPlaca participantes votantes y devuelve: [Participante: Horacio Berreta - Edad: 57 - Atractivo: 10 - Personalidad: 60 - Inteligencia: 50]


{-
    6. Con la placa ya publicada nos interesa saber para un participante:

        a. Está en el horno, esto se cumple cuando recibió tres votos o más.

        b. Hay algo personal, cuando es la única persona en placa.

        c. Zafó, que se cumple cuando no está en placa.


    
-}

    estaEnElHorno :: Participante -> [Participante] -> Bool
    estaEnElHorno participante votantes = length (filter (== participante) participantesVotados) >= 3
        where
            participantesVotados = realizarVotacion votantes participantes

    -- Ejemplo de uso:
    -- estaEnElHorno horacioBerreta votantes y devuelve: True porque fue votado 3 veces o mas.
    -- estaEnElHorno javierTulei votantes y devuelve: False porque no fue votado 3 veces o mas.

    hayAlgoPersonal :: Participante -> [Participante] -> Bool
    hayAlgoPersonal participante votantes = length (enPlaca participantes votantes) == 1

    zafo :: Participante -> [Participante] -> Bool
    zafo participante votantes = not (elem participante (enPlaca participantes votantes))