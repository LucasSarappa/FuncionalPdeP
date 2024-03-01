module Chocobos where

-- A. Se pide declarar las pistas, los chocobos y los jinetes como crea necesario.
    -- Definicion de types:
    type Chocobo = (Int, Int, Int)
    type Pista = (Int, Chocobo -> Int)
    type Corredor = (String, Chocobo)

    -- Definicion de funciones de correcion de velocidad:
    f1 :: Chocobo -> Int
    f1 chocobo = velocidad chocobo * 2

    f2 :: Chocobo -> Int
    f2 chocobo = velocidad chocobo + fuerza chocobo

    f3 :: Chocobo -> Int
    f3 chocobo = velocidad chocobo `div` peso chocobo

    -- Definicion de funciones de acceso a los datos de un Chocobo:
    fuerza :: Chocobo -> Int
    fuerza (f,_,_) = f

    peso :: Chocobo -> Int
    peso (_,p,_) = p

    velocidad :: Chocobo -> Int
    velocidad (_,_,v) = v

    -- Definicion de variables:
    bosqueTenebroso, pantanoDelDestino :: [Pista]
    bosqueTenebroso = [(100, f1), (50, f2), (120, f2), (200, f1), (80, f3)]
    pantanoDelDestino = [(40, f2), (90, \(f,p,v) -> f + p + v), (120, fuerza), (20, fuerza)]

    amarillo , negro, blanco, rojo :: Chocobo
    amarillo = (5, 3, 3)
    negro = (4, 4, 4)
    blanco = (2, 3, 6)
    rojo = (3, 3, 4)

    apocalipsis :: [Corredor]
    apocalipsis = [("Leo", amarillo), ("Gise", blanco), ("Mati", negro), ("Alf",rojo)]

    -- Defincion de funciones de auxiliares: 
    quickSort :: (a -> a -> Bool) -> [a] -> [a]
    quickSort _ [] = []
    quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs ++ [x] ++ (quickSort criterio . filter (criterio x)) xs

    ordenarPorTuplas :: Ord a1 => [(a0, a1)] -> [(a0, a1)]
    ordenarPorTuplas = quickSort (\(_,t1) (_,t2) -> t2 > t1)

    mapearPorTiempoTotal :: [Pista] -> [Corredor] -> [(Chocobo, Int)]
    mapearPorTiempoTotal pistas = map (\(x,y) -> (y, tiempoTotal pistas y))

    mapearPorTiempo :: Pista -> [Corredor] -> [(Chocobo, Int)]
    mapearPorTiempo pista = map (\(x,y) -> (y, tiempo y pista))

    losMejoresChocobos :: [Pista] -> [Corredor] -> [(Chocobo,Int)]
    losMejoresChocobos pistas corredores = take 3 (ordenarPorTuplas (mapearPorTiempoTotal pistas corredores))

    hacerListaDelPodio :: [Pista] -> [Corredor] -> [Corredor]
    hacerListaDelPodio pistas corredores = filter (\(x,y) -> (y, tiempoTotal pistas y) `elem` losMejoresChocobos pistas corredores) corredores

    elMejorCorredor :: Pista -> [Corredor] -> [(Chocobo, Int)]
    elMejorCorredor pista corredores = take 1 (ordenarPorTuplas (mapearPorTiempo pista corredores))

    hacerListaDelMejorCorredor :: Pista -> [Corredor] -> [Corredor]
    hacerListaDelMejorCorredor pista corredores = filter (\(x,y) -> (y, tiempo y pista) `elem` elMejorCorredor pista corredores) corredores

-- Parte B:
    
-- Funciones del punto 1: 

-- 1. Definir dos funciones mayorSegun y menorSegun que, dados una función y dos valores, nos dice si el resultado de  
-- evaluar la función para el primer valor es mayor / menor que el resultado de evaluar la función para el segundo.

    mayorSegun :: Ord a => (t -> a) -> t -> t -> Bool
    mayorSegun funcionAplicada primerValor segundoValor = funcionAplicada primerValor > funcionAplicada segundoValor

    menorSegun :: Ord a => (t -> a) -> t -> t -> Bool
    menorSegun funcionAplicada primerValor segundoValor = not (mayorSegun funcionAplicada primerValor segundoValor)

    -- Para probar:
    -- mayorSegun length [1,2,3] [1,2,3,4] me devuelve False
    -- menorSegun length [1,2,3] [1,2,3,4] me devuelve True


-- Funciones del punto 2:
-- 2. Definir las siguientes funciones:

    -- 2.1 Saber el tiempo que tarda un chocobo en recorrer un tramo. El mismo está dado por la distancia del tramo  dividido 
    -- por la velocidad corregida para el chocobo.
        
    tiempo :: Chocobo -> Pista -> Int
    tiempo chocobo (distancia, correcciónDeVelocidad) = distancia `div` correcciónDeVelocidad chocobo

    -- Para probar:
    -- tiempo amarillo (100, f1) me devuelve 50

    -- 2.2 Determinar el tiempoTotal de un chocobo en una carrera.

    tiempoTotal :: [Pista] -> Chocobo -> Int
    tiempoTotal pistas chocobo = sum (map (tiempo chocobo) pistas)

    -- Para probar:
    -- tiempoTotal bosqueTenebroso amarillo me devuelve 150    
    -- tiempoTotal bosqueTenebroso rojo me devuelve 141
    -- tiempoTotal bosqueTenebroso blanco me devuelve 85
    -- tiempoTotal bosqueTenebroso negro 138


-- Funciones del punto 3:

{-
    3. Obtener el podio de una carrera, representado por una lista ordenada de los 3 primeros puestos de la misma, en  base 
    a una lista de jinetes y una pista. El puesto está dado por el tiempo total, de menor a mayor y se espera  obtener una 
    lista de jinetes.
-}
    podio :: [Pista] -> [Corredor] -> [Corredor]
    podio = hacerListaDelPodio


-- Funciones del punto 4:

{-
4.
    
    4.1 Realizar una función elMejorDelTramo que dado un tramo y una lista de jinetes, retorna el nombre de aquel que lo 
    recorrió en  el menor tiempo.
        
    4.2 Desarrollar elMasWinner donde dada una pista y una lista de jinetes, saber el nombre del jinete que ganó más tramos 
    (que no quiere decir  que haya ganado la carrera).
-}
    elMejorDelTramo :: Pista -> [Corredor] -> String
    elMejorDelTramo pista corredores = fst (head (hacerListaDelMejorCorredor pista corredores))

    -- Para probar:
    -- elMejorDelTramo bosqueTenebroso apocalipsis

    elMasWinner :: [Pista] -> [Corredor] -> String
    elMasWinner pistas corredores = head (quickSort (==) (map (`elMejorDelTramo` corredores) pistas))

    -- Para probar:
    -- elMasWinner bosqueTenebroso apocalipsis me devuelve "Leo"



-- Funciones del punto 5:
{-
5. En la función quienesPueden se informa los nombres de los jinetes (lista)  que pueden hacer un tramo dado en un tiempo 
indicado máximo
-}
    quienesPueden :: Pista -> Int -> [Corredor]  -> [String]
    quienesPueden pista tiempoMax corredores = [x | (x, y) <- corredores, tiempo y pista <= tiempoMax]

    -- Para probar:

    -- quienesPueden (100, f1) 50 apocalipsis me devuelve ["Lucas", "Ono", "Wini", "Alejandro"]
    -- quienesPueden (100, f1) 10 apocalipsis me devuelve ["Ono"]

-- Funciones del punto 6:
{-
6. Obtener las estadísticas de una carrera dando la pista y la lista de jinetes. Estas estadísticas deben estar  
representadas por una lista que muestran información del jinete: su nombre, tramosGanados, tiempoTotal. 
(si es necesario generar algún tipo de dato)
-}
    estadisticas :: [Pista] -> [Corredor] -> [(String, Int, Int)]
    estadisticas pistas corredores = map (\(x,y) -> (x, length (hacerListaDelMejorCorredor (head pistas) corredores), tiempoTotal pistas y)) corredores

    -- Para probar:
    -- estadisticas bosqueTenebroso apocalipsis me devuelve [("Lucas",1,150),("Ono",1,85),("Wini",1,138),("Alejandro",1,141)]

-- Funciones del punto 7:
{-
7. Saber si una carrera fuePareja. Esto es así si cada chocobo tuvo un tiempo total de hasta 10% menor que el que
llegó a continuación.
-}
-- En este punto, tengo que hacer una funcion que me devuelva True si todos los chocobos tuvieron un tiempo total de hasta 10% menor que el que le sigue.

    fuePareja :: [Pista] -> [Corredor] -> Bool
    fuePareja pistas corredores = and [tiempoTotal pistas chocobo <= fromIntegral (tiempoTotal pistas chocobo) * (div 90 100) | (x, chocobo) <- corredores, (y, chocobo) <- corredores, x /= y]


-- Funciones del punto 8:
{-
8. Definir un chocobo plateado que tenga las mejores características de los otros (mayor fuerza, menor peso, mayor
velocidad), teniendo en cuenta que no sea necesario cambiar su definición si se altera un valor de los  anteriores


-}

-- Intuyo que el chocobo plateado es un chocobo con las mejores caracteristicas de los otros chocobos, es decir, 
-- el chocobo con mayor fuerza, menor peso y mayor velocidad.

    plateado :: Chocobo
    plateado = (mayorFuerza listaDeChocobos, menorPeso listaDeChocobos, mayorVelocidad listaDeChocobos)

    mayorFuerza :: [Chocobo] -> Int
    mayorFuerza = maximum . map fuerza

    menorPeso :: [Chocobo] -> Int
    menorPeso = minimum . map peso

    mayorVelocidad :: [Chocobo] -> Int
    mayorVelocidad = maximum . map velocidad

    listaDeChocobos :: [Chocobo]
    listaDeChocobos = [amarillo, rojo, blanco, negro]


    -- Para probar:
    -- plateado me devuelve (5,3,6) que es el chocobo blanco
