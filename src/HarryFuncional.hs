-- https://docs.google.com/document/d/e/2PACX-1vQJX3IJvFA4MxrMj3faBbktGAllV_2_3r647kRwUTPGfwfC8TsIn23knLTzBV2jFpORjXpUmKAAPSL9/pub

{-
Harry Potter y el Examen de Funcional


Nos piden modelar un programa que permita analizar los conflictos en el mundo mágico y, obviamente, nuestro primer impulso es hacerlo en funcional.


De los magos, sabemos que tienen un nombre, una edad y cierta cantidad de salud.
Además, cada mago conoce un conjunto de hechizos el cual puede usar para hacerle cosas raras a otros magos.


Se pide resolver los siguientes puntos, aprovechando al máximo los conceptos del paradigma funcional:


1. Elegir un tipo de dato con el que representar a los Magos y los Hechizos (justificando brevemente la elección) de forma tal que se respete la 
descripción previa del dominio y sea posible modelar los siguientes hechizos:

    a. lagrimaFenix: Este hechizo hace que el mago sobre el que lanzamos el hechizo recupere una cierta cantidad de salud. Este hechizo puede usarse para 
    curar distintas cantidades de vida.
    b. sectumSempra: Este hechizo le hace daño al mago sobre el que se lanza. Si la salud de dicho mago es mayor a 10, le hace 10 puntos de daño, de 
    lo contrario le quita la mitad de su vida actual.
    c. obliviate: El mago objetivo olvida los primeros N hechizos que conozca. Se puede lanzar este hechizo con diferentes valores de N.
    d. confundus: El mago objetivo se ataca a sí mismo con su primer hechizo.


2. Modelar las siguientes funciones respetando los tipos pedidos:

    a. poder :: Mago -> Int
    El poder de un mago es su salud sumada al resultado de multiplicar su edad por la cantidad de hechizos que conoce.

    b. daño :: Mago -> Hechizo -> Int
    Esta función retorna la cantidad de vida que un mago pierde si le lanzan dicho hechizo.
    
    c. diferenciaDePoder :: Mago -> Mago -> Int
    La diferencia de poder entre dos magos es el valor absoluto de la resta del poder de cada uno. Esto siempre retorna un número positivo.


3. Dada una Academia, la cual representamos con el siguiente tipo de dato:

    data Academia = Academia {
    magos :: [Mago],
    examenDeIngreso :: Mago -> Bool
    }

Se pide escribir el código necesario para realizar las siguientes consultas:

    a. Saber si hay algún mago sin hechizos cuyo nombre sea “Rincenwind”.
    b. Saber si todos los magos viejos (cuya edad sea mayor a 50) son ñoños. Esto ocurre si tienen más hechizos que el triple de su edad.
    c. Conocer la cantidad de magos de la academia que no pasarían el examen de ingreso si tuvieran que rendirlo.

4. Dada la siguiente función:

f x [y] = y
f x (y1:y2:ys)
     | x y1 >= x y2 = f x (y1:ys)
     | otherwise = f x (y2 : ys)

Se pide:
    a. Describir brevemente para qué sirve, explicitar su tipo y mejorarla en términos de Expresividad.

    b. Usar esta función para definir las siguientes funciones, sin definir funciones auxiliares:
        
        i.mejorHechizoContra :: Mago -> Mago -> Hechizo
        Dados dos magos, retorna el hechizo del segundo que le haga más daño al primero.
        ii. mejorOponente :: Mago -> Academia -> Mago
        Dado un mago y una academia, retorna el mago de la academia que tenga la mayor diferencia de poder con el mago recibido.


5. Definir la siguiente función sin utilizar recursividad:

noPuedeGanarle :: Mago -> Mago  -> Bool
        Decimos que el segundo mago no puede ganarle al primero si, luego de hechizarlo con todos los hechizos que conoce (uno atrás del otro) 
        la salud del primer mago sigue siendo la misma.




-}


module HarryFuncional where

    data Mago = Mago {
        nombre :: String,
        edad :: Int,
        cantidadDeVida :: Int,
        hechizos :: [Hechizo]
    } deriving (Show)

    data Hechizo = Hechizo {
        nombreHechizo :: String,
        efecto :: Mago -> Mago
    } 

    instance Show Hechizo where
        show (Hechizo nombre _) = "Hechizo: " ++ nombre


    -- PUNTO 1: Elegir un tipo de dato con el que representar a los Magos y los Hechizos (justificando brevemente la elección) de forma tal que se respete la
    -- descripción previa del dominio y sea posible modelar los siguientes hechizos:

    lagrimaFenix :: Int -> Mago -> Mago
    lagrimaFenix cantidadDeVidaRecuperada mago = mago { cantidadDeVida = cantidadDeVida mago + cantidadDeVidaRecuperada }

    sectumSempra :: Mago -> Mago
    sectumSempra mago | cantidadDeVida mago > 10 = mago { cantidadDeVida = cantidadDeVida mago - 10 }
                      | otherwise = mago { cantidadDeVida = cantidadDeVida mago `div` 2 }

    obliviate :: Int -> Mago -> Mago
    obliviate cantidadHechizosOlvidados mago = mago { hechizos = drop cantidadHechizosOlvidados (hechizos mago) }

    confundus :: Mago -> Mago
    confundus mago = efecto (head (hechizos mago)) mago


    -- PUNTO 2: Modelar las siguientes funciones respetando los tipos pedidos:
    
    -- a. poder :: Mago -> Int
    
    poder :: Mago -> Int
    poder mago = (cantidadDeVida mago )+ ((edad mago) * cantidadHechizos mago)

    cantidadHechizos :: Mago -> Int
    cantidadHechizos = length . hechizos

    -- b. daño :: Mago -> Hechizo -> Int

    daño :: Mago -> Hechizo -> Int
    daño mago hechizo = cantidadDeVida mago - cantidadDeVida (efecto hechizo mago)

    -- c. diferenciaDePoder :: Mago -> Mago -> Int

    diferenciaDePoder :: Mago -> Mago -> Int
    diferenciaDePoder mago1 mago2 = abs (poder mago1 - poder mago2)

    
    
    -- PUNTO 3: Dada una Academia, la cual representamos con el siguiente tipo de dato:

    data Academia = Academia {
        magos :: [Mago],
        examenDeIngreso :: Mago -> Bool
    }

    -- Se pide escribir el código necesario para realizar las siguientes consultas:

    -- a. Saber si hay algún mago sin hechizos cuyo nombre sea “Rincenwind”.

    -- pienso en hacer la funcion magosSinHechizos y magosLlamadosRincenwind y luego hacer una funcion que compare ambas listas

    magosSinHechizos :: Academia -> Bool
    magosSinHechizos academia = any ((== 0) . cantidadHechizos) (magos academia)

    magosLlamadosRincenwind :: Academia -> Bool
    magosLlamadosRincenwind academia = any ((== "Rincenwind") . nombre) (magos academia)

    hayMagoSinHechizosLlamadoRincenwind :: Academia -> Bool
    hayMagoSinHechizosLlamadoRincenwind academia = magosSinHechizos academia && magosLlamadosRincenwind academia

    -- b. Saber si todos los magos viejos (cuya edad sea mayor a 50) son ñoños. Esto ocurre si tienen más hechizos que el triple de su edad.



    magosViejos :: Academia -> [Mago]
    magosViejos academia = filter ((> 50) . edad) (magos academia)

    masHechizosQueElTripleDeSuEdad :: Mago -> Bool
    masHechizosQueElTripleDeSuEdad mago = cantidadHechizos mago > 3 * edad mago

    magosNonos :: Academia -> Bool
    magosNonos academia = all masHechizosQueElTripleDeSuEdad (magosViejos academia)


    -- c. Conocer la cantidad de magos de la academia que no pasarían el examen de ingreso si tuvieran que rendirlo.

    noPasanElExamen :: Academia -> Int
    noPasanElExamen academia = length (filter (not . examenDeIngreso academia) (magos academia))


    -- PUNTO 4: Dada la siguiente función:

    f :: (a -> a -> Bool) -> [a] -> a
    f x [y] = y
    f x (y1:y2:ys)
        | x y1 y2 = f x (y1:ys)
        | otherwise = f x (y2 : ys)

    -- a. Describir brevemente para qué sirve, explicitar su tipo y mejorarla en términos de Expresividad.

    -- La funcion f recibe una funcion que recibe dos parametros del mismo tipo y devuelve un booleano, y una lista de elementos del mismo tipo, y 
    -- devuelve un elemento del mismo tipo que los elementos de la lista. La funcion f recorre la lista y compara los elementos de a pares, usando la
    -- funcion que recibe como parametro, y devuelve el primer elemento que cumple con la condicion que recibe como parametro.
    


    -- b. Usar esta función para definir las siguientes funciones, sin definir funciones auxiliares:

    -- i.mejorHechizoContra :: Mago -> Mago -> Hechizo

    mejorHechizoContra :: Mago -> Mago -> Hechizo
    mejorHechizoContra mago1 mago2 = f (\h1 h2 -> daño mago1 h1 >= daño mago1 h2) (hechizos mago2)

    -- ii. mejorOponente :: Mago -> Academia -> Mago

    mejorOponente :: Mago -> Academia -> Mago
    mejorOponente mago academia = f (\m1 m2 -> diferenciaDePoder mago m1 >= diferenciaDePoder mago m2) (magos academia)


    -- PUNTO 5: Definir la siguiente función sin utilizar recursividad:

    -- noPuedeGanarle :: Mago -> Mago  -> Bool





