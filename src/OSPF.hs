
{-
    - Resolver los problemas planteados aprovechando los conceptos del paradigma funcional, en particular deben haber usos interesantes de orden superior, aplicación parcial y composición.
    - Generar buenas abstracciones y evitar la repetición de lógica.
    
Obras Sociales, Recargado

¡Bienvenidos al hermoso mundo de las obras sociales! Bueno, no es tan hermoso, pero son bienvenidos igual para programar el sistema de OSPF 
(Obra Social de Programadores Funcionales). 

Veamos algunas nociones: 

    - De un Socio se sabe su sexo, peso, edad y sus preexistencias (enfermedades diagnosticadas antes de hacerse socio, como "estornudoGalopante" y "chiterosisMultiple").

    - De un Tratamiento se sabe su costo base (es decir, lo que te saldría si tuvieras que atenderte sin obra social), el número de sesiones y la enfermedad que trata

Parte A:

    1. Modelá a los socios y tratamientos con tuplas, registros (data), listas, o lo que consideres más apropiado, y da ejemplos para las siguientes situaciones: 
    
        a. jose: es hombre, 22 años, 78.9Kg y tiene "zamorreaDistopica"
        b. analia: es mujer, 34 años, 70Kg, y está sana
        c. x1: es tratamiento para la "zamorreaDistopica" sale $5000 y requiere 30 sesiones
        d. xfg23: otro tratamiento para la "zamorreaDistopica", pero sale $10000 y consta de 2 sesiones

    2. Escribí la función diagnosticarPreexistencia, que agrega una preexistencia

    3. Escribí la función estaEnRiesgo, que para un socio es verdadero en los siguientes casos: 
        a. Obesidad (más de 150 Kg)
        b. Edad Avanzada (más de 75 años)
        c. O tiene más de 8 preexistencias


Parte B:

OSPF recibe solicitudes: un pedido de un socio para recibir un tratamiento. Y como es su obligación cubrir un cierto monto de los tratamientos solicitados, 
creó el concepto de prestación: una regla que nos dice cuánto debe cubrir dada una solicitud. 

    4. Modelá las solicitudes y da el siguiente ejemplo: 

        a. solicitud897: jose solicitó el tratamiento x1

    5. Modelá las siguientes prestaciones y declará el tipo Prestacion: 

        a. prestacionTotal: cubre el 100% del costo de cualquier solicitud de tratamiento si es para una enfermedad dada, o nada en caso contrario
        b. prestacionSinPreexistencias:  cubre el 50% del costo de la solicitud, si el tratamiento es para una enfermedad de la que el socio NO tenga preexistencias, o nada en caso contrario
        c. prestacionHastaMaximo: cubre hasta $N pesos del costo para cualquier solicitud
        d. nada: no cubre nada de ninguna solicitud

    6. Finalmente, OSPF maneja el concepto de plan: es una prestación especial que suma a otras prestaciones, en la que OSPF cubre tanto como el máximo entre lo que todas cubran.  
    Sabiendo eso: 

        a. Escribí la función sumarPrestaciones, que tome dos prestaciones y devuelva una nueva prestación que (cuando reciba una solicitud) cubra tanto como la mayor de ellas. 
        b. Escribí la función plan que tome una lista de prestaciones y las sume a todas. 

Parte C:

    7. Bonus: probablemente hayas repetido lógica en el punto 5.a, 5.b y 5.c, así que te vamos a dar una (pequeña) ayudita para eliminarla: 
    una prestación siempre sigue esta lógica: 

        "si se cumple una cierta condición sobre la solicitud, aplicá un descuento al precio. Si no, devolvé cero"

    Sabiendo eso, reescribí dichas funciones utilizando esta nueva idea. 

Parte D:

    Resolver el tipo de esta función 

    f a y z h | (y . filter a) h = length h
            | otherwise        = z a

-}

module OSPF where


{-

Veamos algunas nociones: 

    - De un Socio se sabe su sexo, peso, edad y sus preexistencias (enfermedades diagnosticadas antes de hacerse socio, como "estornudoGalopante" y "chiterosisMultiple").

    - De un Tratamiento se sabe su costo base (es decir, lo que te saldría si tuvieras que atenderte sin obra social), el número de sesiones y la enfermedad que trata


-- Parte A:

1. Modelá a los socios y tratamientos con tuplas, registros (data), listas, o lo que consideres más apropiado, y da ejemplos para las siguientes situaciones: 
    
        a. jose: es hombre, 22 años, 78.9Kg y tiene "zamorreaDistopica"
        b. analia: es mujer, 34 años, 70Kg, y está sana
        c. x1: es tratamiento para la "zamorreaDistopica" sale $5000 y requiere 30 sesiones
        d. xfg23: otro tratamiento para la "zamorreaDistopica", pero sale $10000 y consta de 2 sesiones
-}

-- 1. Modelar los socios y tratamientos.

    -- 1.1 Modelar a los socios.

    data Socio = Socio {
        sexo :: String,
        edad :: Int,
        peso :: Float,
        preexistencias :: [String]
    } deriving (Show)


    -- 1.2 Modelar a los tratamientos.

    data Tratamiento = Tratamiento {
        costoBase :: Int,
        sesiones :: Int,
        enfermedad :: String
    } deriving (Show)


    -- 1.3 Ejemplos de socios y tratamientos.

    pepe :: Socio
    pepe = Socio {
        sexo = "hombre",
        edad = 22,
        peso = 78.9,
        preexistencias = ["zamorreaDistopica"]
    }

    analia :: Socio
    analia = Socio {
        sexo = "mujer",
        edad = 34,
        peso = 70,
        preexistencias = []
    }

    guillermo :: Socio
    guillermo = Socio {
        sexo = "hombre",
        edad = 85,
        peso = 90,
        preexistencias = ["zamorreaDistopica", "estornudoGalopante", "chiterosisMultiple"]
    }


    -- 1.4 Ejemplos de tratamientos.

    x1 :: Tratamiento
    x1 = Tratamiento {
        costoBase = 5000,
        sesiones = 30,
        enfermedad = "zamorreaDistopica"
    }

    xfg23 :: Tratamiento
    xfg23 = Tratamiento {
        costoBase = 10000,
        sesiones = 2,
        enfermedad = "zamorreaDistopica"
    }

--2 Escribí la función diagnosticarPreexistencia, que agrega una preexistencia


    diagnosticarPreexistencia :: String -> Socio -> Socio
    diagnosticarPreexistencia preexistencia socio = socio { preexistencias = preexistencia : preexistencias socio }

--3 Escribí la función estaEnRiesgo, que para un socio es verdadero en los siguientes casos:
    
        -- a. Obesidad (más de 150 Kg)
        -- b. Edad Avanzada (más de 75 años)
        -- c. O tiene más de 8 preexistencias
    
    estaEnRiesgo :: Socio -> Bool
    estaEnRiesgo socio = (peso socio > 150) || (edad socio > 75) || (length (preexistencias socio) > 8)

{-
Parte B:

OSPF recibe solicitudes: un pedido de un socio para recibir un tratamiento. Y como es su obligación cubrir un cierto monto de los tratamientos solicitados, 
creó el concepto de prestación: una regla que nos dice cuánto debe cubrir dada una solicitud. 

    4. Modelá las solicitudes y da el siguiente ejemplo: 

        a. solicitud897: jose solicitó el tratamiento x1
-}

-- 4. Modelar las solicitudes.

    data Solicitud = Solicitud {
        socio :: Socio,
        tratamiento :: Tratamiento
    } deriving (Show)

    solicitud897 :: Solicitud
    solicitud897 = Solicitud {
        socio = pepe,
        tratamiento = x1
    }


{-

--5. Modelá las siguientes prestaciones y declará el tipo Prestacion:

    a. prestacionTotal: cubre el 100% del costo de cualquier solicitud de tratamiento si es para una enfermedad dada, o nada en caso contrario
    b. prestacionSinPreexistencias:  cubre el 50% del costo de la solicitud, si el tratamiento es para una enfermedad de la que el socio NO tenga preexistencias, o nada en caso contrario
    c. prestacionHastaMaximo: cubre hasta $N pesos del costo para cualquier solicitud
    d. nada: no cubre nada de ninguna solicitud
-}    

    -- Definición del tipo Prestacion

    
    prestacionTotal :: Solicitud -> Int 
    prestacionTotal solicitud | (enfermedad (tratamiento solicitud)) == "estornudoGalopante" = (costoBase (tratamiento solicitud))
                             | otherwise = 0

    prestacionSinPreexistencias :: Solicitud -> Int
    prestacionSinPreexistencias solicitud | (length (preexistencias (socio solicitud))) == 0 = (costoBase (tratamiento solicitud)) `div` 2
                                         | otherwise = 0

    prestacionHastaMaximo :: Int -> Solicitud -> Int
    prestacionHastaMaximo n solicitud | (costoBase (tratamiento solicitud)) > n = n
                                      | otherwise = (costoBase (tratamiento solicitud))
    



{-
    6. Finalmente, OSPF maneja el concepto de plan: es una prestación especial que suma a otras prestaciones, en la que OSPF cubre tanto como el máximo entre lo que todas cubran.  
    Sabiendo eso: 

        a. Escribí la función sumarPrestaciones, que tome dos prestaciones y devuelva una nueva prestación que (cuando reciba una solicitud) cubra tanto como la mayor de ellas. 
        b. Escribí la función plan que tome una lista de prestaciones y las sume a todas.

-}