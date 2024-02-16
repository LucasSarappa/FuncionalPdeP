{- 
En este contexto tan atípico la gente buscó nuevas maneras de divertirse, pero cuando nada parece funcionar siempre están los viejos y confiables libros. 
A raíz del renovado interés de la gente por la lectura, la conocida empresa Amazin' nos pidió que le ayudemos a desarrollar parte de sus funcionalidades.


Parte A:

    De las personas que tienen una cuenta en Amazin' conocemos su nick, su índice de felicidad, los libros que adquirió y los libros que efectivamente leyó.

    De cada libro conocemos su título, quien lo escribió, su cantidad de páginas y cómo afecta el género a las personas que lo lean.

        1. Modelar les usuaries.
        2. Modelar los libros.
        3. Da un ejemplo de usuarie.
        4. Da un ejemplo de libro.

Parte B:
    Como te contamos más arriba, el género de cada libro produce distintas reacciones en
    quien los lea:
    ● Las comedias dependen de su tipo de humor:
        ○ Las comedias dramáticas no alteran a quien las lee.
        ○ Las comedias absurdas aumentan en 5 el índice de felicidad.
        ○ Las comedias satíricas duplican el índice de felicidad.
        ○ El resto de comedias le suman 10 al índice de felicidad.
    ● Los de ciencia ficción tienen un impacto muy especial, ya que las personas que los
    leen quieren un nombre raro por lo que invierten los caracteres de su nick.
    ● Los de terror hacen huir con pavor a quienes los leen, por lo que regalan todos sus
    libros adquiridos, haciendo que abandonen la lectura... PARA SIEMPRE MUAJAJA.

Parte C:
    Pero qué sería de una aplicación de libros si no hacemos la lógica para registrar y sacar
    conclusiones sobre la lectura de los mismos:

    ● Cuando una persona lee un libro el mismo pasa a formar parte de sus libros leídos y
    además ocurren los efectos del género.
    ● Cuando una persona se pone al día lee todos los libros adquiridos que no haya leído
    previamente. Decimos que una persona leyó un libro si entre los libros que leyó hay
    alguno con el mismo título que haya sido escrito por la misma persona.
    ● Algunas personas se fanatizan con quienes escriben los libros, es por ello que
    queremos saber si una persona es fanática de un escritor o escritora; esto sucede
    cuando todos los libros que leyó fueron escritos por esa autora o autor.
    ● ¿Puede una persona ponerse al día si adquirió una cantidad infinita de libros?
    Justificar.

Parte D:
    Amazin' tiene que clasificar los libros para facilitar las búsquedas, es por ello que decimos
    que:
    ● Los libros con menos de 100 páginas son cuentos.
    ● Los libros que tengan entre 100 y 200 páginas son novelas cortas.
    ● Los libros con más de 200 páginas son novelas.

Para finalizar queremos poder saber los títulos de los libros que una persona adquirió dado
un tipo de libro en específico (cuentos, novelas cortas o novelas).

-}

module Amazing where

{-
    Parte A:

        De las personas que tienen una cuenta en Amazin' conocemos su nick, su índice de felicidad, los libros que adquirió y los libros que efectivamente leyó.

        De cada libro conocemos su título, quien lo escribió, su cantidad de páginas y cómo afecta el género a las personas que lo lean.

            1. Modelar les usuaries.
            2. Modelar los libros.
            3. Da un ejemplo de usuarie.
            4. Da un ejemplo de libro.
-}

-- 1. Modelar les usuaries.

    data Usuario = Usuario {
        nick :: String,
        indiceDeFelicidad :: Int,
        librosAdquiridos :: [Libro],
        librosLeidos :: [Libro]
    } deriving (Show)


-- 2. Modelar los libros.

    data Libro = Libro {
        titulo :: String,
        autor :: String,
        cantidadDePaginas :: Int,
        genero :: Genero
    } deriving (Show)


-- 3. Da un ejemplo de usuarie.

    pepe :: Usuario
    pepe = Usuario {
        nick = "pepito",
        indiceDeFelicidad = 10,
        librosAdquiridos = [elPrincipito, terminator2, elSenorDeLosAnillos],
        librosLeidos = []
    }

    jose :: Usuario
    jose = Usuario {
        nick = "josesito",
        indiceDeFelicidad = 10,
        librosAdquiridos = [elPrincipito, terminator2],
        librosLeidos = []
    }


    lucas :: Usuario
    lucas = Usuario {
        nick = "luquitas",
        indiceDeFelicidad = 5,
        librosAdquiridos = [terminator2],
        librosLeidos = [elPrincipito,elSenorDeLosAnillos]
    }



 -- 4. Da un ejemplo de libro.

    manuelita :: Libro
    manuelita = Libro {
        titulo = "manuelita la tortuga",
        autor = "Maria Elena Walsh",
        cantidadDePaginas = 1000,
        genero = OtraComedia
    }


    elPrincipito :: Libro
    elPrincipito = Libro {
        titulo = "El principito",
        autor = "Antoine de Saint-Exupery",
        cantidadDePaginas = 100,
        genero = ComediaSatirica
    }

    terminator2 :: Libro
    terminator2 = Libro {
        titulo = "Terminator 2",
        autor = "James Cameron",
        cantidadDePaginas = 100,
        genero = CienciaFiccion
    }

    elSenorDeLosAnillos :: Libro
    elSenorDeLosAnillos = Libro {
        titulo = "El senor de los anillos",
        autor = "J.R.R. Tolkien",
        cantidadDePaginas = 1000,
        genero = Terror
    }


{-
    Parte B:
        Como te contamos más arriba, el género de cada libro produce distintas reacciones en
        quien los lea:
-}

    data Genero = CienciaFiccion | ComediaDramatica | ComediaAbsurda | ComediaSatirica | OtraComedia | Terror deriving (Show, Eq)



{-
    ● Las comedias dependen de su tipo de humor:
            ○ Las comedias dramáticas no alteran a quien las lee.
            ○ Las comedias absurdas aumentan en 5 el índice de felicidad.
            ○ Las comedias satíricas duplican el índice de felicidad.
            ○ El resto de comedias le suman 10 al índice de felicidad.
        
-}

    impactoComedia :: Genero -> Int -> Int
    impactoComedia ComediaDramatica indice = indice
    impactoComedia ComediaAbsurda indice = indice + 5
    impactoComedia ComediaSatirica indice = indice * 2
    impactoComedia _ indice = indice + 10

    
    aplicarImpactoComedia :: Libro -> Usuario -> Usuario
    aplicarImpactoComedia libro usuario = usuario { indiceDeFelicidad = impactoComedia (genero libro) (indiceDeFelicidad usuario) }

    
{-
    ● Los de ciencia ficción tienen un impacto muy especial, ya que las personas que los
        leen quieren un nombre raro por lo que invierten los caracteres de su nick.
-}


    invertirNick :: Genero -> String -> String
    invertirNick CienciaFiccion nick = reverse nick
    invertirNick _ nick = nick

    aplicarImpactoCienciaFiccion :: Libro -> Usuario -> Usuario
    aplicarImpactoCienciaFiccion libro usuario = usuario { nick = invertirNick (genero libro) (nick usuario) }
    

    

{-
    ● Los de terror hacen huir con pavor a quienes los leen, por lo que regalan todos sus
        libros adquiridos, haciendo que abandonen la lectura... PARA SIEMPRE MUAJAJA.

-}

    abandonarLectura :: Genero -> Usuario -> Usuario
    abandonarLectura Terror usuario = usuario { librosAdquiridos = []}

    aplicarImpactoTerror :: Libro -> Usuario -> Usuario
    aplicarImpactoTerror libro usuario = abandonarLectura (genero libro) usuario

-- Ahora, creo una funcion general que reciba un Libro y un Usuario, y que devuelva el Usuario pero modificando lo que corresponda dependiendo de los generos

    aplicarReaccion :: Libro -> Usuario -> Usuario
    aplicarReaccion libro usuario
        | genero libro == CienciaFiccion = aplicarImpactoCienciaFiccion libro usuario
        | genero libro == Terror = aplicarImpactoTerror libro usuario
        | otherwise = aplicarImpactoComedia libro usuario


    -- PARA PROBAR:
    -- aplicarReaccion elPrincipito lucas, me devuelve un usuario con el nick "luquitas" y un indice de felicidad de 10
    -- aplicarReaccion elSenorDeLosAnillos lucas, me devuelve un usuario con el nick "luquitas" y un indice de felicidad de 5, y con los libros adquiridos y leidos vacios
    -- aplicarReaccion terminator2 lucas, me devuelve un usuario con el nick "sitauqul" y un indice de felicidad de 5

{-

    Parte C:
        Pero qué sería de una aplicación de libros si no hacemos la lógica para registrar y sacar
        conclusiones sobre la lectura de los mismos:


-}

{-
        ● Cuando una persona lee un libro el mismo pasa a formar parte de sus libros leídos y
        además ocurren los efectos del género.
-}

    leerLibro :: Libro -> Usuario -> Usuario
    leerLibro libro usuario = aplicarReaccion libro usuario { librosLeidos = libro : librosLeidos usuario }

    -- PARA PROBAR:
    -- leerLibro elPrincipito lucas, me devuelve un usuario con el nick "luquitas" y un indice de felicidad de 10, y con el libro "El principito" en la lista de libros leidos
    -- leerLibro elSenorDeLosAnillos lucas, me devuelve un usuario con el nick "luquitas" y un indice de felicidad de 5, y con el libro "El señor de los anillos" en la lista de libros leidos
    -- leerLibro terminator2 lucas, me devuelve un usuario con el nick "sitauqul" y un indice de felicidad de 5, y con el libro "Terminator 2" en la lista de libros leidos

{-
        ● Cuando una persona se pone al día lee todos los libros adquiridos que no haya leído
        previamente. 
-}

    sePoneAlDia :: Usuario -> Usuario
    sePoneAlDia usuario = usuario { librosLeidos = librosAdquiridos usuario ++ librosLeidos usuario, librosAdquiridos = [] }

{-
    Decimos que una persona leyó un libro si entre los libros que leyó hay
    alguno con el mismo título que haya sido escrito por la misma persona.
-}



    esMismoLibro :: Libro -> Libro -> Bool
    esMismoLibro libro1 libro2 = titulo libro1 == titulo libro2 && autor libro1 == autor libro2

    leyoUnLibro :: Libro -> Usuario -> Bool
    leyoUnLibro libro usuario = any (esMismoLibro libro) (librosLeidos usuario)

    -- PARA PROBAR:
    -- leyoUnLibro elPrincipito lucas, me devuelve False
    -- leyoUnLibro elSenorDeLosAnillos lucas, me devuelve False
    -- leyoUnLibro terminator2 lucas, me devuelve False


    
  


{-
        ● Algunas personas se fanatizan con quienes escriben los libros, es por ello que
        queremos saber si una persona es fanática de un escritor o escritora; esto sucede
        cuando todos los libros que leyó fueron escritos por esa autora o autor.
-}


    esFanatica :: Usuario -> String -> Bool
    esFanatica usuario autor = all (esDelAutor autor) (librosLeidos usuario)

    esDelAutor :: String -> Libro -> Bool
    esDelAutor autorLibro libro = autor libro == autorLibro

{-
        ● ¿Puede una persona ponerse al día si adquirió una cantidad infinita de libros?
        Justificar.
-}


    -- No, porque la funcion sePoneAlDia lee todos los libros adquiridos que no haya leído previamente, y si adquirió una cantidad infinita de libros, 
    -- nunca va a terminar de leerlos todos.

{-
    Parte D:
        Amazin' tiene que clasificar los libros para facilitar las búsquedas, es por ello que decimos
        que:

        ● Los libros con menos de 100 páginas son cuentos.
        ● Los libros que tengan entre 100 y 200 páginas son novelas cortas.
        ● Los libros con más de 200 páginas son novelas.
-}

    tipoDeLibro :: Libro -> String
    tipoDeLibro libro
        | cantidadDePaginas libro <= 100 = "Cuento"
        | cantidadDePaginas libro > 100 && cantidadDePaginas libro <= 200 = "Novela corta"
        | otherwise = "Novela"

    -- PARA PROBAR:
    -- tipoDeLibro manuelita, me devuelve "Novela"
    -- tipoDeLibro elPrincipito, me devuelve "Cuento"
    -- tipoDeLibro terminator2, me devuelve "Cuento"
    -- tipoDeLibro elSenorDeLosAnillos, me devuelve "Novela"

{-
    Para finalizar queremos poder saber los títulos de los libros que una persona adquirió dado
    un tipo de libro en específico (cuentos, novelas cortas o novelas).

-}
    
    titulosDeLibrosAdquiridos :: Usuario -> String -> [String]
    titulosDeLibrosAdquiridos usuario tipo = map titulo $ filter (esDelTipo tipo) (librosAdquiridos usuario)

    esDelTipo :: String -> Libro -> Bool
    esDelTipo tipo libro = tipoDeLibro libro == tipo
    
        -- PARA PROBAR:
        -- titulosDeLibrosAdquiridos pepe "Cuento", me devuelve ["El principito", "Terminator 2"]
        -- titulosDeLibrosAdquiridos pepe "Novela", me devuelve ["El senor de los anillos"]
        -- titulosDeLibrosAdquiridos pepe "Novela corta", me devuelve []
    
        -- titulosDeLibrosAdquiridos jose "Cuento", me devuelve ["El principito", "Terminator 2"]
        -- titulosDeLibrosAdquiridos jose "Novela", me





