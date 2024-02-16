---------------------------------------------------------------------------------------- PARADIGMA FUNCIONAL:---------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------ CLASE 2.2: INTRO A FUNCIONAL: https://www.youtube.com/watch?v=wtODGk8J0Ng&list=PL2xYJ49ov_dc1hCGcRMvu8VU3jexRUjf3&index=3----------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------

module Clase2 where

    doble :: Int -> Int
    doble numero = 2 * numero


    divisionConComa :: Float -> Float -> Float
    divisionConComa dividendo divisor = dividendo / divisor

    
    mitad numero = numero / 2



    -- esMayor: dada una edad da true si es mayor de edad

    esMayor :: Int -> Bool -- Bool es el tipo de dato que devuelve la funcion
    esMayor edad = edad >= 18

    -- esMenor: lo opuesto a esMayor
    esMenor :: Int -> Bool -- Bool es el tipo de dato que devuelve la funcion
    esMenor edad = not (esMayor edad)

    -- Para ejecutar el programa y probar:
    -- 1° Abro la terminal de ghci
    -- 2° Situado en: Lucas Sarappa@DESKTOP-P69S701 MINGW64 ~/proyecto-test (master) hago un ls, y veo que esta la carpeta src. Hago un cd src
    -- 3° Situado en: Lucas Sarappa@DESKTOP-P69S701 MINGW64 ~/proyecto-test/src Hago otro ls y entro en Clase1.hs. Hago un ghci Clase1.hs
    -- 4° si dentro hago esMayor 15 me da false, si hago esMayor 18 me da true.

    -- Si hago :t esMayor, me dice de que tipo es la expresion esMayor
    -- Con :r recargo el archivo

    -- Ejercicio: dado un nombre y un apellido, devolver el nombre completo con el formato: Apellido, Nombre
    nombreFormateado :: String -> String -> String
    nombreFormateado nombre apellido = apellido ++ ", " ++ nombre



------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------- CLASE 2.3: PATTERN MATCHING Y GUARDAS: https://www.youtube.com/watch?v=TIo7c4hWZi0&list=PL2xYJ49ov_dc1hCGcRMvu8VU3jexRUjf3&index=4------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------

-- -------------------- PATTERN MATCHING Y VARIABLE ANONIMA: --------------------

--         *Chequedo estructural.
--         *Permite declarar para distintas formas que es lo que queremos que pase.

-- Ejemplo: Definimos una funcion f --> 
--      f(1) = Recibi 1, 
--      f(0) = Recibi 0, 

-- De que tipo es esta funcion? Independientemente de cuantas formas se contemplen, siempre es del mismo tipo. En este caso, Int -> String

-- Cosas que NO FUNCIONAN:

-- Ejemplo 1:
--      f :: ??? -> String
--      f (1) = "Recibi 1"
--      f ("2") = "Recibi un string"

-- Ejemplo 2:
--      f :: Int -> ???
--      f (1) = "Recibi 1"
--      f (0) = False

-- Problema a resolver: Queremos implementar la conjuncion, pero no podemos usar las funciones que ya vienen en Haskell. Entonces, como lo hacemos?

    conjuncion :: Bool -> Bool -> Bool -- Recibe 2 parametros Booleanos y devuelve otro Booleano.
    conjuncion True True = True  -- Si recibe True y True, devuelve True.
    conjuncion _ _ = False -- Si recibe cualquier otra cosa, devuelve False. 
-- El _ es un comodin (variable anonima), que significa cualquier cosa. Siempre hay que poner los casos mas generales al final. 

-- -------------------- GUARDAS (Es un caso de Pattern Matching que nos permite agregar condiciones): --------------------
-- Analogo a las funciones por partes. Es una funcion, pero termina teniendo distintas funciones en base a una condicion o criterio.

-- Para formar las guardas, hay que dejar un espacio al principio y una linea por cada caso: | condicion (Tiene que ser SI O SI una expresion booleana) = definicion.

    f :: Int -> Int
    f x 
        | x < -1 = -1
        | -1 <= x && x <= 1 = 1
        | x > 1 = -1        

-- Otra cosa que se puede agregar, es para generalizar una guarda, el uso de otherwise.
-- Se evalua de arriba hacia abajo, y se queda con la primera que cumple la condicion.

    g :: Int -> Int
    g x 
        | x < -1 = -1
        | otherwise = -1

-- Guardas Innecesarias: 
-- Cuando queremos definir una funcion, que dada una condicion retrona un booleano, lo mas probable es que esa condicion sea todo lo que queriamos hacer, 
-- no hace falta hacer una guarda. 
-- Ejemplo:
    h :: Int -> Bool
    h x 
        |x > 0 = True
        |otherwise = False

-- Sino que directamente podemos hacer:
    j :: Int -> Bool
    j x = x > 0





