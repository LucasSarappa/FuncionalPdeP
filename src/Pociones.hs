{-
Se pide desarrollar un programa Haskell que ayude a regular el consumo de las pociones que se enseñan a los alumnos del colegio 
Hogwarts de Magia y Hechicería.
Las pociones, producidas al combinar ingredientes exóticos que causan efectos diversos, pueden ser consumidas con el fin de alterar 
los niveles de suerte, inteligencia y fuerza de quien las bebe.


Para representar este modelo contamos con las siguientes definiciones:

data Persona = Persona {
  nombrePersona :: String,
  suerte :: Int,
  inteligencia :: Int,
  fuerza :: Int
} deriving (Show, Eq)

data Pocion = Pocion {
  nombrePocion :: String,
  ingredientes :: [Ingrediente]
}

type Efecto = Persona -> Persona

data Ingrediente = Ingrediente {
  nombreIngrediente :: String,
  efectos :: [Efecto]
}

nombresDeIngredientesProhibidos = [
 "sangre de unicornio",
 "veneno de basilisco",
 "patas de cabra",
 "efedrina"]

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun _ [ x ] = x
maximoSegun  f ( x : y : xs)
  | f x > f y = maximoSegun f (x:xs)
  | otherwise = maximoSegun f (y:xs)


Se pide resolver los siguientes puntos utilizando los conceptos aprendidos del paradigma funcional: 
composición, aplicación parcial y orden superior.


    1. Dada una persona definir las siguientes funciones para cuantificar sus niveles de suerte, inteligencia y fuerza sin repetir código:
        a. sumaDeNiveles que suma todos sus niveles.
        b. diferenciaDeNiveles es la diferencia entre el nivel más alto y más bajo.
        c. nivelesMayoresA n, que indica la cantidad de niveles de la persona que están por encima del valor dado.


    2. Definir la función efectosDePocion que dada una poción devuelve una lista con los efectos de todos sus ingredientes.


    3. Dada una lista de pociones, consultar:
        a. Los nombres de las pociones hardcore, que son las que tienen al menos 4 efectos.
        b. La cantidad de pociones prohibidas, que son aquellas que tienen algún ingrediente cuyo nombre figura en la lista de ingredientes prohibidos.
        c. Si son todas dulces, lo cual ocurre cuando todas las pociones de la lista tienen algún ingrediente llamado “azúcar”.


    4. Definir la función tomarPocion que recibe una poción y una persona, y devuelve como quedaría la persona después de tomar la poción. 
    Cuando una persona toma una poción, se aplican todos los efectos de esta última, en orden.

    5. Definir la función esAntidotoDe que recibe dos pociones y una persona, y dice si tomar la segunda poción revierte los cambios que se producen 
    en la persona al tomar la primera.

    6. Definir la función personaMasAfectada que recibe una poción, una función cuantificadora (es decir, una función que dada una persona retorna un número) 
    y una lista de personas, y devuelve a la persona de la lista que hace máxima el valor del cuantificador. Mostrar un ejemplo de uso utilizando los 
    cuantificadores definidos en el punto 1.

-}
module Pociones where

    data Persona = Persona {
        nombrePersona :: String,
        suerte :: Int,
        inteligencia :: Int,
        fuerza :: Int
    } deriving (Show, Eq)

    data Pocion = Pocion {
        nombrePocion :: String,
        ingredientes :: [Ingrediente]
    }

    type Efecto = Persona -> Persona

    data Ingrediente = Ingrediente {
        nombreIngrediente :: String,
        efectos :: [Efecto]
    }

    nombresDeIngredientesProhibidos = ["sangre de unicornio", "veneno de basilisco", "patas de cabra", "efedrina"]

    maximoSegun :: Ord b => (a -> b) -> [a] -> a
    maximoSegun _ [ x ] = x
    maximoSegun  f ( x : y : xs)
        | f x > f y = maximoSegun f (x:xs)
        | otherwise = maximoSegun f (y:xs)

    {-
    1. Dada una persona definir las siguientes funciones para cuantificar sus niveles de suerte, inteligencia y fuerza sin repetir código:
            a. sumaDeNiveles que suma todos sus niveles.
            b. diferenciaDeNiveles es la diferencia entre el nivel más alto y más bajo.
            c. nivelesMayoresA n, que indica la cantidad de niveles de la persona que están por encima del valor dado.

    -}


    niveles :: Persona -> [Int] -- creo una funcion personas para que dada una persona me devuelva una lista con los valores de sus niveles
    niveles persona = [suerte persona, inteligencia persona, fuerza persona]

-- 1.a  sumaDeNiveles que suma todos sus niveles.

    sumaDeNiveles :: Persona -> Int
    sumaDeNiveles = sum . niveles 

    -- Para probarlo en consola: sumaDeNiveles (Persona "Juan" 10 20 30)

    -- Otra forma pero repitiendo codigo:
    -- sumaDeNiveles` persona = fuerza persona + inteligencia persona + suerte persona



-- 1.b diferenciaDeNiveles es la diferencia entre el nivel más alto y más bajo.

    diferenciaDeNiveles :: Persona -> Int
    diferenciaDeNiveles persona = maximoNivel persona - minimoNivel persona

    maximoNivel :: Persona -> Int
    maximoNivel = maximum . niveles

    minimoNivel :: Persona -> Int
    minimoNivel = minimum . niveles

    -- Para probarlo en consola: diferenciaDeNiveles (Persona "Juan" 10 20 30) 


    -- Otra forma pero repitiendo codigo:

    diferenciaDeNiveles' :: Persona -> Int
    diferenciaDeNiveles' persona = maximoNivel persona - minimoNivel persona
    
    maximoNivel' :: Persona -> Int
    maximoNivel' persona = fuerza persona `max` inteligencia persona `max` suerte persona

    minimoNivel' :: Persona -> Int
    minimoNivel' persona = fuerza persona `min` inteligencia persona `min` suerte persona




--1.c nivelesMayoresA n, que indica la cantidad de niveles de la persona que están por encima del valor dado.

    nivelesMayoresA :: Int -> (Persona -> Int)
    nivelesMayoresA n = length . filter (>n) . niveles


    
    -- Para probarlo en consola: nivelesMayoresA 15 (Persona "Juan" 10 20 30) 



-- 2. Definir la función efectosDePocion que dada una poción devuelve una lista con los efectos de todos sus ingredientes.

    efectosDePocion :: Pocion -> [Efecto]
    efectosDePocion = concat . map efectos . (ingredientes)

-- 3. Dada una lista de pociones, consultar:
-- a. Los nombres de las pociones hardcore, que son las que tienen al menos 4 efectos.

    pocionesHardcore :: [Pocion] -> [String]
    pocionesHardcore = map nombrePocion . (filter ((>=4) . length . efectosDePocion))

-- b. La cantidad de pociones prohibidas, que son aquellas que tienen algún ingrediente cuyo nombre figura en la lista de ingredientes prohibidos.

    pocionesProhibidas :: [Pocion] -> Int
    pocionesProhibidas = length . (filter esProhibida)

    esProhibida :: Pocion -> Bool
    esProhibida = any (flip elem nombresDeIngredientesProhibidos . nombreIngrediente) . ingredientes 
    -- any es para ver si alguno cumple la condicion, y con el elem veo si el nombre del ingrediente esta en la lista de prohibidos y 
    -- con el flip lo que hago es que el elem reciba primero el nombre del ingrediente y despues la lista de prohibidos             
    
-- c. Si son todas dulces, lo cual ocurre cuando todas las pociones de la lista tienen algún ingrediente llamado “azúcar”.

    sonTodasDulces :: [Pocion] -> Bool
    sonTodasDulces = all (ingredienteAzucar)

    ingredienteAzucar :: Pocion -> Bool
    ingredienteAzucar = any ((=="azúcar") . nombreIngrediente) . ingredientes 
    -- all es para ver si todos cumplen la condicion, y con el any veo si alguno cumple la condicion, y con el flip lo que hago es que el elem reciba primero el nombre del
    -- ingrediente y despues la lista de prohibidos

{-
4. Definir la función tomarPocion que recibe una poción y una persona, y devuelve como quedaría la persona después de tomar la poción. 
    Cuando una persona toma una poción, se aplican todos los efectos de esta última, en orden.
-}

    tomarPocion :: Pocion -> Efecto -- Efecto = Persona -> Persona
    tomarPocion pocion personaInicial =         (foldl (...). efectosDePocion) pocion