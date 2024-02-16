{-
Link del ejercicio de Parcial 
LambdaProp.com: https://docs.google.com/document/d/e/2PACX-1vRluMJi1uEVHvGh-c8PAq82yVUqjyHMVwhHWIW3G2CRVmferWQeE59Vpwqmmtvhew-UueX2rdvKALLD/pub


ENUNCIADO:

Buscar departamentos para alquilar por los medios tradicionales es una tarea compleja, ya que requiere mucho tiempo de investigación buscando en los 
clasificados de los diarios y recorriendo inmobiliarias. Es por eso que hoy en día cada vez son más las personas que dejaron eso atrás dejando que internet 
se encargue de buscar las supuestas mejores alternativas para sus necesidades.

Por eso surge una nueva página para buscar departamentos que permita al usuario personalizar sus propias búsquedas y de paso eventualmente mandarle mails 
con las nuevas ofertas inmobiliarias que podrían ser de su interés a ver si agarra viaje.


Tenemos los departamentos modelados de la siguiente forma:


type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto {
 ambientes :: Int,
 superficie :: Int,
 precio :: Int,
 barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
   mail :: Mail,
   busquedas :: [Busqueda]
}

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
 (ordenarSegun criterio . filter (not . criterio x)) xs ++
 [x] ++
 (ordenarSegun criterio . filter (criterio x)) xs

between cotaInferior cotaSuperior valor =
 valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
 Depto 3 80 7500 "Palermo",
 Depto 1 45 3500 "Villa Urquiza",
 Depto 2 50 5000 "Palermo",
 Depto 1 45 5500 "Recoleta"]

Se pide desarrollar las siguientes funciones y consultas de modo que se aprovechen tanto como sea posible los conceptos de orden superior, aplicación parcial y composición.


1. 
    a. Definir las funciones mayor y menor que reciban una función y dos valores, y retorna true si el resultado de evaluar esa función sobre el primer valor es 
    mayor o menor que el resultado de evaluarlo sobre el segundo valor respectivamente.
    
    b. Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar una lista de strings en base a su longitud usando ordenarSegun.


2. Definir las siguientes funciones para que puedan ser usadas como requisitos de búsqueda:

    a. ubicadoEn que dada una lista de barrios que le interesan al usuario, retorne verdadero si el departamento se encuentra en alguno de los barrios de la lista.

    b. cumpleRango que a partir de una función y dos números, indique si el valor retornado por la función al ser aplicada con el departamento se encuentra entre los dos valores 
    indicados.


3. 
    a. Definir la función cumpleBusqueda que se cumple si todos los requisitos de una búsqueda se verifican para un departamento dado.

    b. Definir la funcion departamentosBuscadosEnOrdenDeInteres que a partir de una busqueda, un criterio de ordenamiento y una lista de departamentos, retorne todos aquellos que
    cumplen con la búsqueda ordenados en base al criterio recibido.

    c. Mostrar un ejemplo de uso de buscar para obtener los departamentos de ejemplo, ordenado por mayor superficie, que cumplan con:

        - Encontrarse en Recoleta o Palermo
        - Ser de 1 o 2 ambientes
        - Alquilarse a menos de $6000 por mes


4. Definir la función mailsDePersonasInteresadas que a partir de un departamento y una lista de personas retorne los mails de las personas que tienen alguna búsqueda que se 
cumpla para el departamento dado.


-}


-- ------------------------------------------------------------------------------------------------------------------------------------------------------

module LambdaProp where

    type Barrio = String
    type Mail = String
    type Requisito = Depto -> Bool
    type Busqueda = [Requisito] 

    data Depto = Depto {
    ambientes :: Int,
    superficie :: Int,
    precio :: Int,
    barrio :: Barrio
    } deriving (Show, Eq)
    

    data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
    }
    -- En este caso, tenemos una lista de requisitos dentro de un tipo de dato Busqueda, y una lista de Busqueda dentro de un tipo de dato Persona.
    -- ordenarSegun
    ordenarSegun _ [] = []
    ordenarSegun criterio (x:xs) =(ordenarSegun criterio . filter (not . criterio x)) xs ++ [x] ++ (ordenarSegun criterio . filter (criterio x)) xs

    between cotaInferior cotaSuperior valor = valor <= cotaSuperior && valor >= cotaInferior

    deptosDeEjemplo = [
        Depto 3 80 7500 "Palermo",
        Depto 1 45 3500 "Villa Urquiza",
        Depto 2 50 5000 "Palermo",
        Depto 1 45 5500 "Recoleta"]

-- ------------------------------------------------------------------ SOLUCION:-----------------------------------------------------------------------------------------
{-

    1. 
        a. Definir las funciones mayor y menor que reciban una función y dos valores, y retorna true si el resultado de evaluar esa función sobre el primer valor es 
        mayor o menor que el resultado de evaluarlo sobre el segundo valor respectivamente.

-}

-- Como recibe una funcion, (->) es el constructor de tipos de funciones y a -> a son los otros 2 valores que recibe la funcion.
-- (son iguales porque, al tener que evaluarlas dentro de la misma funcion, deben ser del mismo tipo)

    mayor :: Ord b => (a->b) -> a -> a -> Bool 
    mayor funcion valor1 valor2 = funcion valor1 > funcion valor2

    menor :: Ord b => (a->b) -> a -> a -> Bool 
    menor funcion valor1 valor2 = funcion valor1 < funcion valor2

-- La restricción Ord b en las funciones mayor y menor indica que el tipo b, que es el tipo resultante de aplicar la función dada, debe ser ordenable.

{-

    1. 
        b. Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar una lista de strings en base a su longitud usando ordenarSegun.

-}

-- ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
-- Un ejemplo de ordenarSegun usando la función mayor es: ordenarSegun (mayor length) ["hola", "chau", "buenas tardes", "buenas noches", "buen dia"]
-- Un ejemplo de ordenarSegun usando la función menor es: ordenarSegun (menor length) ["hola", "chau", "buenas tardes", "buenas noches", "buen dia"

{-
2. Definir las siguientes funciones para que puedan ser usadas como requisitos de búsqueda:

    a. ubicadoEn que dada una lista de barrios que le interesan al usuario, retorne verdadero si el departamento se encuentra en alguno de los barrios de la lista.

    b. cumpleRango que a partir de una función y dos números, indique si el valor retornado por la función al ser aplicada con el departamento se encuentra entre los dos valores 
    indicados.

-}
    -- ubicadoEn :: [Barrio] -> (Depto -> Bool) -> otra opcion (es igual a la de abajo)
    ubicadoEn :: [Barrio] -> Requisito
    ubicadoEn barriosDeInteres departamento = elem (barrio departamento) barriosDeInteres

    -- Para probar en consola: ubicadoEn ["Palermo", "Recoleta"] (head deptosDeEjemplo)



    --  cumpleRango :: (Depto -> Int) -> Int -> Int -> (Depto -> Bool) -> otra opcion (es igual a la de abajo)
    cumpleRango :: (Depto -> Int) -> Int -> Int -> Requisito
    cumpleRango funcion numero1 numero2 departamento = between numero1 numero2 (funcion departamento)
    
    -- Para probar en consola: cumpleRango ambientes 1 2 (head deptosDeEjemplo)

--3
{-
    a. Definir la función cumpleBusqueda que se cumple si todos los requisitos de una búsqueda se verifican para un departamento dado.
-}
    -- DATO: Una busqueda es una lista de requisitos, entonces cumpleBusqueda es una funcion que recibe una busqueda y un departamento y devuelve un booleano

    cumpleBusqueda :: Depto -> Busqueda -> Bool
    cumpleBusqueda departamento busqueda = all (cumpleRequisito departamento) busqueda -- con la funcion all, me fijo si todos los elementos de la lista son True

    cumpleRequisito :: Depto -> Requisito -> Bool
    cumpleRequisito departamento requisito = requisito departamento

    {-
    Busqueda que tenga los siguientes requisitos:
    - Encontrarse en Recoleta o Palermo
    - Ser de 1 o 2 ambientes
    - Alquilarse a menos de $6000 por mes
    -}

    busquedaDeEjemplo :: Busqueda
    busquedaDeEjemplo = 
        [ubicadoEn ["Palermo", "Recoleta"], 
        cumpleRango ambientes 1 2, 
        cumpleRango precio 0 6000]


    -- Para probar en consola: cumpleBusqueda (head deptosDeEjemplo) busquedaDeEjemplo0 o cumpleBusqueda (head Depto 2 50 5000 "Palermo") busquedaDeEjemplo

{-
    b. Definir la funcion departamentosBuscadosEnOrdenDeInteres que a partir de una busqueda, un criterio de ordenamiento y una lista de departamentos, retorne todos aquellos que
    cumplen con la búsqueda ordenados en base al criterio recibido.
-}

-- en este punto se usa la idea de a partir de una lista de algo, quiero quedarme con los que cumplen con tal cosa, se necesita hacer un filtrado

    departamentosBuscadosEnOrdenDeInteres :: Busqueda -> (Depto -> Depto -> Bool) -> [Depto] -> [Depto]
    departamentosBuscadosEnOrdenDeInteres busqueda criterioDeOrdenamiento departamentos = ordenarSegun criterioDeOrdenamiento . filter (flip cumpleBusqueda busqueda) $ departamentos


{-
    c. Mostrar un ejemplo de uso de buscar para obtener los departamentos de ejemplo, ordenado por mayor superficie, que cumplan con:

        - Encontrarse en Recoleta o Palermo
        - Ser de 1 o 2 ambientes
        - Alquilarse a menos de $6000 por mes
-}


-- Para probar en consola: departamentosBuscadosEnOrdenDeInteres busquedaDeEjemplo (mayor superficie) deptosDeEjemplo

-- Me devuelve: [Depto {ambientes = 2, superficie = 50, precio = 5000, barrio = "Palermo"},Depto {ambientes = 1, superficie = 45, precio = 5500, barrio = "Recoleta"}]

{-

4. Definir la función mailsDePersonasInteresadas que a partir de un departamento y una lista de personas retorne los mails de las personas que tienen alguna búsqueda que se 
cumpla para el departamento dado.

-}
-- yo quiero a partir de un conjunto de personas e ir a un conjunto de mails, entonces tengo que hacer un mapeo. Por otro lado, no quiero todos los mails, 
-- sino los que cumplen con la busqueda del departamento, entonces tengo que filtrarlos.
    
    mailsDePersonasInteresadas :: Depto -> [Persona] -> [Mail]
    mailsDePersonasInteresadas departamento = map mail . filter (estaInteresada departamento)

    estaInteresada :: Depto -> Persona -> Bool
    estaInteresada departamento persona = any (cumpleBusqueda departamento) (busquedas persona)

-- para probar en consola: mailsDePersonasInteresadas (head deptosDeEjemplo) [Persona "pepito@gmail.com" [[cumpleRango ambientes 1 3], busquedaDeEjemplo], Persona "juanito@gmail.coml2" [busquedaDeEjemplo]]
-- me devuelve: ["pepito@gmail.com"]