{-

Gimnasia Artística 

Un club nos contrató para hacer un sistema de seguimiento de sus socios que practican gimnasia artística,  de los cuales  se conoce sus deportistas y los ejercicios que realizan.

De cada gimnasta se sabe su nombre, su nivel de energía, el nivel de equilibrio, la flexibilidad,  el nivel de fuerza física y las habilidades que posee, es decir los ejercicios 
que sabe realizar muy bien.  De cada ejercicio se conoce los efectos que produce en el gimnasta.

    medialuna aumenta en 5 unidades el nivel de equilibrio del gimnasta.
    rolAdelante dependiendo de la velocidad con que lo realiza,  aumenta  la energía del gimnasta en la mitad de la velocidad.
    vertical aumenta en 7 unidades la fuerza física del gimnasta.
    saltoConSoga, disminuye la energía en la mitad de la cantidad de saltos  y aumenta la fuerza física tanto como la cantidad de saltos que realiza.
    saltoMortal aumenta la fuerza física del gimnasta en tanto como la altura del mismo y la flexibilidad en la mitad del nivel del impulso del salto.

Aprovechando los conceptos que vimos en clase de aplicación parcial, composición de funciones, orden superior, listas por comprensión definir tanto las funciones principales 
como todas las auxiliares que permitan conocer:

Punto 1:
   Modelar utilizando la estructura más conveniente el tipo de dato gimnasta.

Punto 2: 
    Modelar a Sonia que tiene un nivel de energía de 90, un nivel de equilibrio de 60,  una flexibilidad de 40, un nivel de fuerza física de 50.  Y tiene habilidad para realizar la media luna,  
    el rol adelante con una velocidad de 20 y el salto mortal con una altura de 40 y un impulso de 15.
    
    Modelar a Pedro que tiene un nivel de energía de 70, un equilibrio de 50,  una flexibilidad de 50, un nivel de fuerza física de 60.  Tiene habilidad para realizar hasta 150 saltos seguidos, 
    la vertical y el rol adelante con una velocidad de 30.

Punto 3: 
    
    Para aprender una nueva habilidad se necesita mucho tiempo de práctica. Definir la función ejercitar que dado una cantidad de minutos,  un ejercicio, hacer que el gimnasta realice dicho 
    ejercicio cada 2 minutos y por último lo aprenda. 
    
    Existen diferentes rutinas que realizan los gimnastas, de cada una se conocen la cantidad de veces que se repite y todos los ejercicios que se deben realizar en cada vuelta.
        
        Modelar de la manera más conveniente el tipo rutina.

        Modelar la rutina entrada en calor que consiste en realizar 2 veces los siguientes ejercicios en este orden:  2 rol adelante de 10 de velocidad, 4 medialunas,  50 saltos de soga,  
        1 salto mortal de altura 20 y nivel de impulso de 15.

        Modelar la rutina diaria  consiste en hacer 3 veces los siguientes ejercicios: 1 rol delante de 20 de velocidad,  30 saltos de soga,  1 vertical, 1 media luna y 10 saltos de soga.

    Definir la función entrenar que dado un gimnasta y una rutina, hacer que el gimnasta realice dicha rutina.

    Conocer a los gimnastas que tienen potencial, es decir que luego de realizar la rutina diaria, hacen los ejercicios que saben hacer muy bien y quedan con un nivel de  fortaleza mayor a n. 
    El nivel de fortaleza es la suma entre la energía y la fuerza.

Punto 4:
    Dado un conjunto de gimnastas, conocer el nombre del gimnasta que luego de realizar la rutina diaria sean máximo según:
        La fuerza física
        El mínimo entre la flexibilidad y la fortaleza física.
        La cantidad de habilidades que posee luego de ejercitar durante 10 minutos un ejercicio dado.

Punto 5:
    Definir el tipo de la expresión h:

    h e g = any((\x->x). (== e)) . map g

Punto 6:
    En la función del punto 3 a)  ¿se podría agregar una validación en  la función ejercitar para verificar si el ejercicio ya lo sabe antes de que lo aprenda?.
    ¿La función h podría aplicarla a una lista infinita de socios y lograr que termine?. Justifique y dar un ejemplo de aplicación y respuesta.
    -}

module Gimnasia where

    -- Punto 1: Modelar utilizando la estructura más conveniente el tipo de dato gimnasta.
    
    data Gimnasta = Gimnasta {
        nombre :: String,
        energia :: Int,
        equilibrio :: Int,
        flexibilidad :: Int,
        fuerzaFisica :: Int,
        habilidades :: [Ejercicio]
    }


    data Ejercicio = Ejercicio {
        nombreEjercicio :: String,
        efecto :: Gimnasta -> Gimnasta
    }

    instance Show Gimnasta where
        show gimnasta = "Nombre: " ++ nombre gimnasta ++ " | Energia: " ++ show (energia gimnasta) ++ " | Equilibrio: " ++ show (equilibrio gimnasta) ++ " | Flexibilidad: " ++ show (flexibilidad gimnasta) ++ " | Fuerza Fisica: " ++ show (fuerzaFisica gimnasta) ++ "| Habilidades: " ++ show (habilidades gimnasta)

    instance Show Ejercicio where
        show ej = nombreEjercicio ej



    -- Punto 2: Modelar a Sonia y Pedro

    sonia = Gimnasta "Sonia" 90 60 40 50 [medialuna, rolAdelante 20, saltoMortal 40 15]
    pedro = Gimnasta "Pedro" 70 50 50 60 [saltoConSoga 150, vertical, rolAdelante 30]

    -- Ejercicios
    medialuna = Ejercicio "medialuna" (aumentarEquilibrio 5)
    rolAdelante velocidad = Ejercicio "rolAdelante" (aumentarEnergia velocidad)
    vertical = Ejercicio "vertical" (aumentarFuerzaFisica 7)
    saltoConSoga saltos = Ejercicio "saltoConSoga" (disminuirEnergia saltos . aumentarFuerzaFisica saltos)
    saltoMortal altura impulso = Ejercicio "saltoMortal" (aumentarFuerzaFisica altura . aumentarFlexibilidad (impulso `div` 2))

    aumentarEquilibrio :: Int -> Gimnasta -> Gimnasta
    aumentarEquilibrio cantidad gimnasta = gimnasta {equilibrio = equilibrio gimnasta + cantidad}

    aumentarEnergia :: Int -> Gimnasta -> Gimnasta
    aumentarEnergia velocidad gimnasta = gimnasta {energia = energia gimnasta + (velocidad `div` 2)}

    aumentarFuerzaFisica :: Int -> Gimnasta -> Gimnasta
    aumentarFuerzaFisica cantidad gimnasta = gimnasta {fuerzaFisica = fuerzaFisica gimnasta + cantidad}

    disminuirEnergia :: Int -> Gimnasta -> Gimnasta
    disminuirEnergia saltos gimnasta = gimnasta {energia = energia gimnasta - (saltos `div` 2)}

    aumentarFlexibilidad :: Int -> Gimnasta -> Gimnasta
    aumentarFlexibilidad impulso gimnasta = gimnasta {flexibilidad = flexibilidad gimnasta + (impulso `div` 2)}

    -- Punto 3:
    {-
    Para aprender una nueva habilidad se necesita mucho tiempo de práctica. Definir la función ejercitar que dado una cantidad de minutos,  un ejercicio, hacer que el gimnasta realice dicho 
    ejercicio cada 2 minutos y por último lo aprenda. 
    -}

    ejercitar :: Int -> Ejercicio -> Gimnasta -> Gimnasta
    ejercitar minutos ejercicio gimnasta
        | nombreEjercicio ejercicio `elem` (map nombreEjercicio $ habilidades gimnasta) = gimnasta
        | otherwise = (realizarEjercicio minutos ejercicio) . (aprenderEjercicio ejercicio) $ gimnasta


    realizarEjercicio :: Int -> Ejercicio -> Gimnasta -> Gimnasta
    realizarEjercicio minutos ejercicio gimnasta = foldl (flip ($)) gimnasta (replicate (minutos `div` 2) (efecto ejercicio))


    aprenderEjercicio :: Ejercicio -> Gimnasta -> Gimnasta
    aprenderEjercicio ejercicio gimnasta = gimnasta {habilidades = ejercicio : habilidades gimnasta}

    -- Para probar:
    -- ejercitar 2 medialuna sonia
    -- me devuelve a sonia con los mismos valores, ya que ya sabe hacer la medialuna
    -- ejemplo con saltoConSoga
    -- ejercitar 10 (saltoConSoga 100) sonia
    -- me devuelve a sonia con los valores de energia y fuerzaFisica modificados: Sonia | Energia: -160 | Equilibrio: 60 | Flexibilidad: 40 | Fuerza Fisica: 550| Habilidades: [saltoConSoga,medialuna,rolAdelante,saltoMortal]


    {-
    Existen diferentes rutinas que realizan los gimnastas, de cada una se conocen la cantidad de veces que se repite y todos los ejercicios que se deben realizar en cada vuelta.
        Modelar de la manera más conveniente el tipo rutina.
        Modelar la rutina entrada en calor que consiste en realizar 2 veces los siguientes ejercicios en este orden:  2 rol adelante de 10 de velocidad, 4 medialunas,  50 saltos de soga,  
        1 salto mortal de altura 20 y nivel de impulso de 15.
        Modelar la rutina diaria  consiste en hacer 3 veces los siguientes ejercicios: 1 rol delante de 20 de velocidad,  30 saltos de soga,  1 vertical, 1 media luna y 10 saltos de soga.
    -}

    data Rutina = Rutina {
        repeticiones :: Int,
        ejercicios :: [Ejercicio]
    }

    entradaEnCalor = Rutina 2 [rolAdelante 10, rolAdelante 10, medialuna, medialuna, medialuna, medialuna, saltoConSoga 50, saltoMortal 20 15]

    rutinaDiaria = Rutina 3 [rolAdelante 20, saltoConSoga 30, vertical, medialuna, saltoConSoga 10]


    -- Definir la función entrenar que dado un gimnasta y una rutina, hacer que el gimnasta realice dicha rutina.

    entrenar :: Gimnasta -> Rutina -> Gimnasta
    entrenar gimnasta rutina = foldl (flip ($)) gimnasta (replicate (repeticiones rutina) (realizarRutina rutina))

    realizarRutina :: Rutina -> Gimnasta -> Gimnasta
    realizarRutina rutina gimnasta = foldl (flip ($)) gimnasta (concat $ replicate (repeticiones rutina) (map efecto (ejercicios rutina)))

    -- Para probar:
    -- entrenar sonia entradaEnCalor
    -- me devuelve a sonia con los valores de energia y fuerzaFisica modificados: Nombre: Sonia | Energia: 30 | Equilibrio: 140 | Flexibilidad: 52 | Fuerza Fisica: 330| Habilidades: [medialuna,rolAdelante,saltoMortal]


    -- Conocer a los gimnastas que tienen potencial, es decir que luego de realizar la rutina diaria, hacen los ejercicios que saben hacer muy bien y quedan con un nivel de  fortaleza mayor a n.

    gimnastasConPotencial :: Int -> [Gimnasta] -> [Gimnasta]
    gimnastasConPotencial n gimnastas = filter (\gimnasta -> (energia gimnasta + fuerzaFisica gimnasta) > n) (map (entrenarDiaria) gimnastas)

    entrenarDiaria :: Gimnasta -> Gimnasta
    entrenarDiaria gimnasta = foldl (flip ($)) gimnasta (map efecto (ejercicios rutinaDiaria))

    -- Para probar:
    -- gimnastasConPotencial 10 [sonia, pedro]
    -- me devuelve a [Nombre: Sonia | Energia: 80 | Equilibrio: 65 | Flexibilidad: 40 | Fuerza Fisica: 97| Habilidades: [medialuna,rolAdelante,saltoMortal],Nombre: Pedro | Energia: 60 | Equilibrio: 55 | Flexibilidad: 50 | Fuerza Fisica: 107| Habilidades: [saltoConSoga,vertical,rolAdelante]]

    -- Punto 4:
    {-
    Dado un conjunto de gimnastas, conocer el nombre del gimnasta que luego de realizar la rutina diaria sean máximo según:
        La fuerza física
        El mínimo entre la flexibilidad y la fortaleza física.
        La cantidad de habilidades que posee luego de ejercitar durante 10 minutos un ejercicio dado.
    -}

    gimnastaMaximoSegun :: (Gimnasta -> Int) -> [Gimnasta] -> String
    gimnastaMaximoSegun f gimnastas = nombre $ foldl1 (maximoSegun f) gimnastas

    maximoSegun :: (Gimnasta -> Int) -> Gimnasta -> Gimnasta -> Gimnasta
    maximoSegun f gimnasta1 gimnasta2
        | f gimnasta1 > f gimnasta2 = gimnasta1
        | otherwise = gimnasta2

    

    -- Para probar:
    -- gimnastaMaximoSegun fuerzaFisica [sonia, pedro]
    -- me devuelve "Pedro"
    -- gimnastaMaximoSegun (minimoSegun flexibilidad fuerzaFisica) [sonia, pedro]
    -- me devuelve "Sonia"

    minimoSegun :: (Gimnasta -> Int) -> (Gimnasta -> Int) -> Gimnasta -> Gimnasta -> Gimnasta
    minimoSegun f1 f2 gimnasta1 gimnasta2
        | f1 gimnasta1 < f1 gimnasta2 = gimnasta1
        | f2 gimnasta1 < f2 gimnasta2 = gimnasta1
        | otherwise = gimnasta2

    -- Para probar:
    -- gimnastaMaximoSegun (minimoSegun flexibilidad fuerzaFisica) [sonia, pedro]
    -- me devuelve "Sonia"
    -- gimnastaMaximoSegun (minimoSegun flexibilidad fuerzaFisica) [pedro, sonia]
    -- me devuelve "Sonia"

    cantidadHabilidades :: Int -> Ejercicio -> [Gimnasta] -> Int
    cantidadHabilidades minutos ejercicio gimnastas = length $ filter (\gimnasta -> nombreEjercicio ejercicio `elem` (map nombreEjercicio $ habilidades $ ejercitar minutos ejercicio gimnasta)) gimnastas

    -- Para probar:
    -- cantidadHabilidades 10 (saltoConSoga 100) [sonia, pedro]
    -- me devuelve 2

    


    -- Punto 5:

    -- Definir el tipo de la expresión h:
    -- h e g = any((\x->x). (== e)) . map g
    -- h :: Eq a => a -> (b -> a) -> [b] -> Bool
    -- h e g = any((\x->x). (== e)) . map g
    -- h 2 (+1) [1,2,3]
    -- me devuelve True


    -- Punto 6:

    -- En la función del punto 3 a)  ¿se podría agregar una validación en  la función ejercitar para verificar si el ejercicio ya lo sabe antes de que lo aprenda?.
    -- Sí, se podría agregar una validación para verificar si el ejercicio ya lo sabe antes de que lo aprenda. Se podría hacer una validación en la función ejercitar para verificar si el ejercicio ya lo sabe antes de que lo aprenda, y en caso de que ya lo sepa, no realizar el ejercicio.

    -- ¿La función h podría aplicarla a una lista infinita de socios y lograr que termine?. Justifique y dar un ejemplo de aplicación y respuesta.
    -- No, la función h no podría aplicarla a una lista infinita de socios y lograr que termine, ya que la función any evalúa la lista hasta encontrar un elemento que cumpla con la condición, y al ser una lista infinita, no terminaría de evaluarla.
    -- h 2 (+1) [1..]
    -- no termina de evaluar la lista, ya que es infinita
    -- h 2 (+1) [1,2,3]
    -- me devuelve True

