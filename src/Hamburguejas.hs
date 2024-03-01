{-
● Definir el tipo de todas las funciones principales.
● No duplicar lógica.
● No utilizar recursividad a menos que se lo indique.
● Utilizar composición y aplicación parcial.
● Utilizar orden superior.
● No pensar de manera procedural.
● en funcional la idea siempre es tratar de hacer una pasada, y no tener que ir a buscar en base a un criterio.
● Utilizar e identificar (al menos una vez) adecuadamente los siguientes
conceptos:
○ Composición
○ Aplicación Parcial
○ Orden Superior

-}

module Hamburguejas where

    type Ingrediente = String
    type Calorias = Int
    type Alteracion = Combo -> Combo

    data Hamburguesa = Hamburguesa {
        nombre :: String,
        precio :: Int,
        ingredientes :: [Ingrediente]
    } deriving Show

    data Bebida = Bebida {
        nombreBebida :: String,
        tamanioBebeida :: Int,
        light :: Bool
    } deriving Show

    type Acompaniamiento = String

    type Combo = (Hamburguesa, Bebida, Acompaniamiento)

    type InformacionNutricional = [(Ingrediente, Calorias)]

    hamburguesa (h, _, _) = h
    bebida (_, b, _) = b
    acompaniamiento (_, _, a) = a

    informacionNutricional = [("Carne", 250), ("Queso", 50), ("Pan", 20), ("Panceta", 541), ("Lechuga", 5), ("Tomate", 6)]

    condimentos = ["Barbacoa", "Mostaza", "Mayonesa", "Salsa big mac", "Ketchup"]

    comboQyB = (qyb, cocaCola, "Papas")
    comboSoloQuesoNormal = (soloQueso, cocaCola, "Papas")
    comboSoloQuesoVerde = (soloQueso, cocaCola, "Ensalada")
    comboSoloQuesoDietetica = (soloQueso, cocaColaDietetica, "Ensalada")

    cocaCola = Bebida "Coca Cola" 2 False
    cocaColaDietetica = Bebida "Coca Cola" 2 True

    qyb = Hamburguesa "QyB" 200 ["Pan", "Carne", "Queso", "Panceta", "Mayonesa", "Ketchup", "Pan"]
    soloQueso = Hamburguesa "Solo Queso" 150 ["Pan", "Queso","Carne", "Pan"]

    -- Punto 1: Queremos saber cuantas calorias tiene un ingrediente, esto puede obtenerse a partr de la informacion nutricional, a menos que sea
    -- un condimento, en cuyo caso la cantidad de calorias es 10

    -- Para esto, primero defino una funcion caloriasDe, que recibe un ingrediente, y devuelve la cantidad de calorias que tiene ese ingrediente.
    -- Esa caloria la obtengo de la informacion nutricional, que es una lista de tuplas, donde el primer elemento de la tupla es el ingrediente, y el segundo
    -- elemento de la tupla es la cantidad de calorias que tiene ese ingrediente. Si lo encuentra en la lista, devuelve el valor de la segunda componenete
    -- de la tupla, sino, devuelve 10, que es la cantidad de calorias que tiene un condimento..

    caloriasDe :: Ingrediente -> Calorias
    caloriasDe ingrediente
        | esIngrediente ingrediente = caloriasDeIngrediente ingrediente
        | esCondimento ingrediente = 10
    
    -- Esto lo hice verificando que sea condimento o ingrediente, ya que en el dia de mañana puede haber mas cosas que no sean 
    -- ni condimento ni ingrediente de los cuales se quiera saber la cantidad de calorias.

    esCondimento :: Ingrediente -> Bool
    esCondimento ingrediente = ingrediente `elem` condimentos

    esIngrediente :: Ingrediente -> Bool
    esIngrediente ingrediente = ingrediente `elem` (map fst informacionNutricional)

    -- caloriasDeIngrediente, recibe un ingrediente, y devuelve la cantidad de calorias que tiene ese ingrediente, usando la funcion caloriasDeIngrediente.
    -- la caloria del ingrediente la obtengo de la informacion nutricional, que es una lista de tuplas, donde el primer elemento de la tupla es el 
    -- ingrediente, y el segundo elemento de la tupla es la cantidad de calorias que tiene ese ingrediente.

    caloriasDeIngrediente :: Ingrediente -> Calorias
    caloriasDeIngrediente ingrediente = snd (head (filter ((== ingrediente) . fst) informacionNutricional))

    -- lo que hace es que primero se fija si el ingrediente es igual al primero de la tupla, y si lo es, devuelve el segundo elemento de la tupla. 
    -- En caso de que no lo sea, sigue buscand en la lista.

    -- Para probar: caloriasDe "Carne" me devuelve 250
    -- caloriasDe "Barbacoa" me devuelve 10
    -- caloriasDe "Queso" me devuelve 50

    -- Punto 2: Se quiere saber si un combo esMortal. Esto cumple cuando la bebida no es dietetica y el
    -- acompañamiento no es ensalada, o si la hamburguesa es una bomba (tiene entre sus ingredientes al menos uno que tenga mas de 300 calorias o si
    -- en total la hamburguesa supera las 1000 calorias.)


    esMortal :: Combo -> Bool
    esMortal (h,b,a) = (((light b) && a /= "Ensalada") || esBomba h)

    esBomba :: Hamburguesa -> Bool
    esBomba h = (caloriasHamburguesa h > 1000) || any ((> 300) . caloriasDe) (ingredientes h)

    caloriasHamburguesa :: Hamburguesa -> Calorias
    caloriasHamburguesa h = sum (map caloriasDe (ingredientes h))

    -- Para probar:
    -- esMortal comboQyB me devuelve True
    -- esMortal comboSoloQuesoNormal me devuelve True, ya que
    -- esMortal comboSoloQuesoVerde me devuelve False

    -- Punto 3: Definir las siguientes funciones:
    -- 3.1. agrandarBebida, el combo alterado deberia tener el mismo tipo de bebida pero incrementado en 1 su tamaño.

    agrandarBebida :: Alteracion
    agrandarBebida (h,b,a) = (h, b {tamanioBebeida = tamanioBebeida b + 1}, a)

    --Para probar:
    -- agrandarBebida comboQyB me devuelve (Hamburguesa {nombre = "QyB", precio = 200, ingredientes = ["Pan","Carne","Queso","Panceta","Mayonesa","Ketchup","Pan"]},Bebida {nombreBebida = "Coca Cola", tamanioBebeida = 3, light = False},"Papas")

    -- 3.2 cambiarAcompaniamientoPor, el combo alterado deberia tener el acompañamiento elegido por el cliente.

    cambiarAcompaniamientoPor :: Acompaniamiento -> Alteracion
    cambiarAcompaniamientoPor nuevoAcompaniamiento (h,b,_) = (h, b, nuevoAcompaniamiento)
    -- Para probar:
    -- cambiarAcompaniamientoPor "Papas" comboSoloQuesoVerde me devuelve (Hamburguesa {nombre = "Solo Queso", precio = 150, ingredientes = ["Pan","Queso","Carne","Pan"]},Bebida {nombreBebida = "Coca Cola", tamanioBebeida = 2, light = False},"Papas")

    -- 3.3 peroSin: la hamburguesa del combo deberia excluir ingredientes que cumplan con una determinada condicion. En principio, nos interesa 
    -- contemplar las siguientes condiciones sobre los ingredientes, pero deberia admitir otras condiciones del mismo tipo: 
            -- esCondimento: un ingrediente cumple esta condicion si es igual a alguno de los condimentos concidos.
            -- masCaloricoQue: se cumple esta condicion si las calorias del ingrediente superan un valor dado.

    -- esCondimento :: (Ingrediente -> Bool), ya la tengo definida, asi que la puedo usar directamente.
    -- masCaloricoQue :: (Ingrediente -> Bool), me fijo si las calorias del ingrediente superan un valor dado, usando la funcion caloriasDe, que me devuelve la cantidad de calorias de un ingrediente.

    masCaloricoQue :: Calorias -> (Ingrediente -> Bool)
    masCaloricoQue calorias = (> calorias) . caloriasDe

    peroSin :: (Ingrediente -> Bool) -> Alteracion  -- elijo devolver Alteracion ( pasar Combo y devolver Combo), ya que no me interesa devolver un tipo de dato distinto al que recibo, sino que solamente modificar la hamburguesa del combo.
    peroSin condicion combo = (filtrarIngredientes condicion (hamburguesa combo), bebida combo, acompaniamiento combo)

    filtrarIngredientes :: (Ingrediente -> Bool) -> Hamburguesa -> Hamburguesa
    filtrarIngredientes condicion hamburguesa = hamburguesa {ingredientes = filter (not . condicion) (ingredientes hamburguesa)}


    -- filtrarIngredientes, recibe una condicion y una hamburguesa, y devuelve una hamburguesa con los ingredientes que cumplen con la condicion.
    -- Para esto, uso la funcion filter, que recibe una condicion y una lista, y devuelve una lista con los elementos que cumplen con la condicion.
    -- En este caso, la condicion es que el ingrediente no cumpla con la condicion que recibo por parametro.


    -- Para probar:
    -- peroSin esCondimento comboQyB me devuelve (Hamburguesa {nombre = "QyB", precio = 200, ingredientes = ["Pan","Carne","Queso","Panceta","Pan"]},Bebida {nombreBebida = "Coca Cola", tamanioBebeida = 2, light = False},"Papas")
    -- peroSin (masCaloricoQue 5) comboQyB me devuelve (Hamburguesa {nombre = "QyB", precio = 200, ingredientes = []},Bebida {nombreBebida = "Coca Cola", tamanioBebeida = 2, light = False},"Papas")
    
    
    -- Punto 4: Realizar una consulta usando lo desarrollado hasta ahora que permita obtener a partir del comboQyB y una lista de alteraciones, aquellas
    -- alteracion tras las cuales el combo en cuestion no es mortal. Las alteraciones a incluir en la lista deben ser las siguientes:
    -- agrandar el tamaño de la bebida, cambiar el acompañamiento por ensalada, que venga sin condimento, que venga sin ingredientes de mas de 400 
    -- calorias  y que venga sin queso.

    -- aplicarAlteraciones recibe una lista de alteraciones y un combo, y devuelve un combo con la lista de alteraciones aplicada.

    aplicarAlteraciones :: Combo -> [Alteracion] -> Combo
    aplicarAlteraciones combo listaDeAlteraciones = foldl (flip ($)) combo listaDeAlteraciones

    listaDeAlteraciones = [agrandarBebida, cambiarAcompaniamientoPor "Ensalada", peroSin esCondimento, peroSin (masCaloricoQue 400), peroSin (== "Queso")]
    -- Para probar:
    -- aplicarAlteraciones comboQyB [agrandarBebida, cambiarAcompaniamientoPor "Ensalada", peroSin esCondimento, peroSin (masCaloricoQue 400), peroSin (== "Queso")] me devuelve
