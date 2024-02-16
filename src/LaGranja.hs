{-
La granja:
En una granja viven animales, de los cuales registramos su nombre, el tipo de animal, el peso, la edad y sabemos si esta enfermo, 
lo cual podra requerir una visita medica de alguna persona veterinaria, que diagnostica los dias de recuperacion y le cobra un costo por la atencion.

Punto 1: Animales sueltos.

    La pasó mal:
    Queremos saber si un animal la paso mal, esto implica que alguna de las visitas medicas que le hicieron le implico mas de 30 dias de recuperacion

    Nombre falopa:
    Queremos saber si un animal tienen un nombre falopa, esto pasa si la ultima letra termina en "i". Por ejemplo: "gachi" o "pachi" ademas de ser de sagitario, tiene un nombre falopa. 
    "Dorthy" no tiene un nombre falopa.
    NOTA: En este punto no se puede usar recursividad ni funciones auxiliares, solo composicion y aplicacion parcial.

Punto 2: Actividades.

    Queremos modelar las actividades que se hacen en la granja, entre las cuales estan:

    Engorde: Le dan de comer al animal "x" kilos de alimento balanceado, con lo cual incrementan la mitad de su peso hasta un maximo de 5 kilos. Por ejemplo: Si la vaca Dorothy que pesa 690
    kilos le damos de comer 12 kilos de alimento balanceado, pasara a pesar 695 kilos. Si le damos 4 kilos, pesara 692 kilos.

    Revisacion: Si el animal esta enfermo, se le registra una visita medica anotando los dias de recuperacion y el costo. Ademas, al darle vitaminas, eso equivaldria a que el animal coma 2 kilos de
    alimento balanceado. Consejo: Evite repetir ideas.

    Festejo cumpleanios: Le agrega un año mas y tambien se le hace una fiestita, por la emocion el animal pierde 1 kilo

    Chequeo de peso: Queremos registrar si un animal esta bien de peso, para lo cual tiene que estar por arriba de un peso "x", en caso contrario debe quedar enfermo


Punto 3: El proceso.

    Queremos modelar un proceso, que realiza una serie de actividades sobre un animal. Se pide que ademas muestre un ejemplo de como podria evaluar por consola el proceso para 
    cada una de las actividades resueltas en el punto anterior.
    En este punto no puede utilizar funciones auxiliares ni recursividad , solo composicion y aplicacion parcial.

Punto 4: ¿Mejora o no mejora?
    
    Dado un proceso (lista de actividades) y un animal, queremos saber si el animal mejora sustentablemtente el peso, esto implica que el peso nunca debe bajar de una actividad a otra y 
    tambpoco debe subir mas de 3 kilos de una actividad
    Este punto debe resolverlo con recursividad.

Punto 5: Give me one, give me two...

    a. Queremos obtener los primeros 3 animales que tengan un nombre falopa. Resolverlo solo con funciones de orden superior

    b. Si le pasaramos una cantidad infinita de animales, seria posible obtener un valor computable para la funcion del punto anterior? Justifique su respuesta relacionandola con
    un concepto de la materia.
-}


module LaGranja where

    {-
    En una granja viven animales, de los cuales registramos su nombre, el tipo de animal, el peso, la edad y sabemos si esta enfermo, 
    lo cual podra requerir una visita medica de alguna persona veterinaria, que diagnostica los dias de recuperacion y le cobra un costo por la atencion.
    -}

    data Animal = UnAnimal {
        nombre :: String,
        tipo :: String,
        peso :: Int,
        edad :: Int,
        enfermo :: Bool,
        visitasMedicas :: [VisitaMedica]
    }


    data VisitaMedica = UnaVisita {
        diasRecuperacion :: Int,
        costo :: Int
    }

    instance Show Animal where
        show animal = "Nombre: " ++ nombre animal ++ " | Tipo: " ++ tipo animal ++ " | Peso: " ++ show (peso animal) ++ " | Edad: " ++ show (edad animal) ++ " | Enfermo: " ++ show (enfermo animal) ++ " | Visitas medicas: " ++ show (visitasMedicas animal)

    instance Show VisitaMedica where
        show visita = "Dias de recuperacion: " ++ show (diasRecuperacion visita) ++ " | Costo: " ++ show (costo visita)

    -- Modelo de ejemplo

    dorothy :: Animal
    dorothy = UnAnimal "Dorothy" "Vaca" 690 5 False [visitaMedicaEjemplo3]

    pachi :: Animal
    pachi = UnAnimal "Pachi" "Vaca" 690 5 False [visitaMedicaEjemplo, visitaMedicaEjemplo2]

    gachi :: Animal
    gachi = UnAnimal "Gachi" "Vaca" 690 5 False [visitaMedicaEjemplo, visitaMedicaEjemplo2]

    machi :: Animal
    machi = UnAnimal "Machi" "Vaca" 690 5 True [visitaMedicaEjemplo, visitaMedicaEjemplo2]

    -- Modelo de ejemplo

    visitaMedicaEjemplo :: VisitaMedica
    visitaMedicaEjemplo = UnaVisita 10 100

    visitaMedicaEjemplo2 :: VisitaMedica
    visitaMedicaEjemplo2 = UnaVisita 40 1000

    visitaMedicaEjemplo3 :: VisitaMedica
    visitaMedicaEjemplo3 = UnaVisita 5 100

    -- Punto 1: Animales sueltos.
    {-
    La pasó mal: 
    Queremos saber si un animal la paso mal, esto implica que alguna de las visitas medicas que le hicieron le implico mas de 30 dias de recuperacion
    -}



    laPasoMal :: Animal -> Bool
    laPasoMal = (>30) . maximum . diasDeRecuperacion

    diasDeRecuperacion :: Animal -> [Int]
    diasDeRecuperacion = map diasRecuperacion . visitasMedicas


    -- Para probar: 
    -- laPasoMal pachi -> True
    -- laPasoMal dorothy -> False


    {-
    Nombre falopa:
    Queremos saber si un animal tienen un nombre falopa, esto pasa si la ultima letra termina en "i". Por ejemplo: "gachi" o "pachi" ademas de ser de sagitario, 
    tiene un nombre falopa. 
    "Dorthy" no tiene un nombre falopa.NOTA: En este punto no se puede usar recursividad ni funciones auxiliares, solo composicion y aplicacion parcial.
    -}

    -- Tener en cuenta que para hacer este punto se debe usar composicion y aplicacion parcial, no se puede usar recursividad ni funciones auxiliares.

    nombreFalopa :: Animal -> Bool
    nombreFalopa = (== 'i') . last . nombre -- en este caso, usamos composicion entre la funcion last y la funcion nombre, y aplicacion parcial en la funcion last

    -- Para probar:
    -- nombreFalopa pachi -> True
    -- nombreFalopa dorothy -> False
    -- nombreFalopa gachi -> True


    -- Punto 2: Actividades.

    {-
    Queremos modelar las actividades que se hacen en la granja, entre las cuales estan:

    Engorde: Le dan de comer al animal "x" kilos de alimento balanceado, con lo cual incrementan la mitad de su peso hasta un maximo de 5 kilos. Por ejemplo: Si la vaca Dorothy que pesa 690
    kilos le damos de comer 12 kilos de alimento balanceado, pasara a pesar 695 kilos. Si le damos 4 kilos, pesara 692 kilos.

    Revisacion: Si el animal esta enfermo, se le registra una visita medica anotando los dias de recuperacion y el costo. Ademas, al darle vitaminas, eso equivaldria a que el animal coma 2 kilos de
    alimento balanceado. Consejo: Evite repetir ideas.

    Festejo cumpleanios: Le agrega un año mas y tambien se le hace una fiestita, por la emocion el animal pierde 1 kilo

    Chequeo de peso: Queremos registrar si un animal esta bien de peso, para lo cual tiene que estar por arriba de un peso "x", en caso contrario debe quedar enfermo

    -}

    -- Engorde: Le dan de comer al animal "x" kilos de alimento balanceado, con lo cual incrementan la mitad de su peso hasta un maximo de 5 kilos.Por ejemplo: Si la vaca Dorothy que pesa 690
    -- kilos le damos de comer 12 kilos de alimento balanceado, pasara a pesar 695 kilos. Si le damos 4 kilos, pesara 692 kilos.


    engorde :: Int -> Animal -> Animal
    engorde kilosAlimento animal =
        animal { peso = nuevoPeso (peso animal) (incrementoPeso kilosAlimento) }

    incrementoPeso :: Int -> Int
    incrementoPeso kilosAlimento = div kilosAlimento 2

    nuevoPeso :: Int -> Int -> Int
    nuevoPeso pesoAnimal incremento = min (pesoAnimal + incremento) (pesoAnimal + 5)

    -- Para probar:

    -- engorde 12 dorothy -> UnAnimal {nombre = "Dorothy", tipo = "Vaca", peso = 695, edad = 5, enfermo = False, visitasMedicas = [UnaVisita {diasRecuperacion = 5, costo = 100}]}
    -- engorde 4 dorothy -> UnAnimal {nombre = "Dorothy", tipo = "Vaca", peso = 692, edad = 5, enfermo = False, visitasMedicas = [UnaVisita {diasRecuperacion = 5, costo = 100}]}


    -- Revisacion: Si el animal esta enfermo, se le registra una visita medica anotando los dias de recuperacion y el costo. Ademas, al darle vitaminas, eso equivaldria a que el animal 
    -- coma 2 kilos de alimento balanceado. Consejo: Evite repetir ideas, es decir, no se puede repetir la logica de la funcion engorde.

    revisacion :: Animal -> Animal
    revisacion animal
            | enfermo animal = animal { visitasMedicas = visitaMedica : visitasMedicas animal, peso = peso animal + 2 }
            | otherwise = animal
        where visitaMedica = UnaVisita 10 100

    
    -- Para probar:

    -- revisacion dorothy -> UnAnimal {nombre = "Dorothy", tipo = "Vaca", peso = 692, edad = 5, enfermo = False, visitasMedicas = [UnaVisita {diasRecuperacion = 5, costo = 100}]}
    -- revisacion pachi -> UnAnimal {nombre = "Pachi", tipo = "Vaca", peso = 692, edad = 5, enfermo = False, visitasMedicas = [UnaVisita {diasRecuperacion = 10, costo = 100},UnaVisita {diasRecuperacion = 40, costo = 1000}]}
    -- revisacion gachi -> UnAnimal {nombre = "Gachi", tipo = "Vaca", peso = 692, edad = 5, enfermo = False, visitasMedicas = [UnaVisita {diasRecuperacion = 10, costo = 100},UnaVisita {diasRecuperacion = 40, costo = 1000}]}
    -- revisacion machi -> UnAnimal {nombre = "Machi", tipo = "Vaca", peso = 692, edad = 5, enfermo = True, visitasMedicas = [UnaVisita {diasRecuperacion = 10, costo = 100},UnaVisita {diasRecuperacion = 40, costo = 1000}]}

    -- Festejo cumpleanios: Le agrega un año mas y tambien se le hace una fiestita, por la emocion el animal pierde 1 kilo

    festejoCumpleanios :: Animal -> Animal
    festejoCumpleanios animal = animal { edad = edad animal + 1, peso = peso animal - 1 }

    -- Para probar:
    -- festejoCumpleanios dorothy -> UnAnimal {nombre = "Dorothy", tipo = "Vaca", peso = 689, edad = 6, enfermo = False, visitasMedicas = [UnaVisita {diasRecuperacion = 5, costo = 100}]}


    -- Chequeo de peso: Queremos registrar si un animal esta bien de peso, para lo cual tiene que estar por arriba de un peso "x", en caso contrario debe quedar enfermo

    chequeoDePeso :: Int -> Animal -> Animal
    chequeoDePeso pesoMinimo animal
        | peso animal > pesoMinimo = animal
        | otherwise = animal { enfermo = True }


    -- Para probar:
    -- chequeoDePeso 700 dorothy -> UnAnimal {nombre = "Dorothy", tipo = "Vaca", peso = 690, edad = 5, enfermo = True, visitasMedicas = [UnaVisita {diasRecuperacion = 5, costo = 100}]}
    -- chequeoDePeso 680 dorothy -> UnAnimal {nombre = "Dorothy", tipo = "Vaca", peso = 690, edad = 5, enfermo = False, visitasMedicas = [UnaVisita {diasRecuperacion = 5, costo = 100}]}
    

    {-
    Punto 3: El proceso.

    Queremos modelar un proceso, que realiza una serie de actividades sobre un animal. Se pide que ademas muestre un ejemplo de como podria evaluar por consola el proceso para 
    cada una de las actividades resueltas en el punto anterior.
    En este punto no puede utilizar funciones auxiliares ni recursividad , solo composicion y aplicacion parcial.
    -}


    -- En este caso, usamos composicion y aplicacion parcial para resolver el punto 3.

    type Proceso = Animal -> Animal

    proceso :: [Proceso] -> Animal -> Animal
    proceso actividades animal = foldl (\animal actividad -> actividad animal) animal actividades

    actividad :: [Proceso]
    actividad = [engorde 12, revisacion, festejoCumpleanios, chequeoDePeso 700]

    -- Para probar:
    -- proceso actividad dorothy ->
        -- UnAnimal {nombre = "Dorothy", tipo = "Vaca", peso = 692, edad = 6, enfermo = True, visitasMedicas = [UnaVisita {diasRecuperacion = 5, costo = 100}]}


    {-
    Punto 4: ¿Mejora o no mejora?

    Dado un proceso (lista de actividades) y un animal, queremos saber si el animal mejora sustentablemtente el peso, esto implica que el peso nunca debe bajar de una actividad a otra y
    tampoco debe subir mas de 3 kilos de una actividad
    Este punto debe resolverlo con recursividad y abstraer logica de funciones.
    -}

    
    -- ESTE ME MATASTE, NO SE COMO HACERLO.


    {-
    Punto 5: Give me one, give me two...

        a. Queremos obtener los primeros 3 animales que tengan un nombre falopa. Resolverlo solo con funciones de orden superior

        b. Si le pasaramos una cantidad infinita de animales, seria posible obtener un valor computable para la funcion del punto anterior? Justifique su respuesta relacionandola con
        un concepto de la materia.
    -}

    -- a. Queremos obtener los primeros 3 animales que tengan un nombre falopa. Resolverlo solo con funciones de orden superior

    primerosTresFalopas :: [Animal] -> [Animal]
    primerosTresFalopas = take 3 . filter nombreFalopa

    -- Para probar:
    -- primerosTresFalopas [dorothy, pachi, gachi, machi] -> [pachi, gachi, machi]

    -- b. Si le pasaramos una cantidad infinita de animales, seria posible obtener un valor computable para la funcion del punto anterior? Justifique su respuesta relacionandola con

    -- No, no seria posible obtener un valor computable para la funcion del punto anterior, ya que la funcion take 3 no podria obtener los primeros 3 elementos de una lista infinita, ya que
    -- la lista infinita no tiene un fin, por lo que la funcion take 3 no podria obtener los primeros 3 elementos de una lista infinita.

    