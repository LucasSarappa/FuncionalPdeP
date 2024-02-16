{-
Super Mario Bros: El Parcial:

https://github.com/pdep-utn-miercoles-manana/2023-funcional-parcial-fedescarpa/blob/master/src/Lib.hs

Mario, una de las franquicias más conocidas, posiblemente el videojuego más conocido de todo el mundo. Recientemente estrenó una pésima excelente película de esta franquicia, 
rompiendo varios récords de taquilla. Obviamente, con semejante torta de plata, se dieron cuenta de que la franquicia puede recaudar en otros formatos multimedia, por lo que nos 
pidieron que hiciéramos un parcial temático con el fin de aumentar las ventas.

Como todos sabemos, Mario es un plomero, una noble profesión que nos salva las papas del fuego cuando se nos rompen los caños de la casa y no tenemos ni la más pálida idea de por 
dónde arrancar. 

De los plomeros conocemos su nombre, la caja de herramientas que llevan, el historial de reparaciones que han hecho y el dinero que llevan encima (¡no trabajan gratis!). 

Hay unas cuantas herramientas que un plomero puede tener encima, y de cada una conocemos su denominación (nombre de la herramienta), su precio y el material de su empuñadura, 
que puede ser hierro, madera, goma o plástico.

Se pide, usando los conceptos del paradigma funcional:


    1) Modelar a los plomeros y sus herramientas.

        Mario, un plomero que tiene $1200, no hizo ninguna reparación hasta ahora y en su caja de herramientas lleva una llave inglesa con mango de hierro que tiene un precio de $200 
        y un martillo con empuñadura de madera que le salió $20.

        Wario, tiene 50 centavos encima, no hizo reparaciones, lleva infinitas llaves francesas, obviamente de hierro, la primera le salió un peso, pero cada una que compraba le salía un 
        peso más cara. La inflación lo está matando. 
    

    2) Saber si un plomero:

            a) Tiene una herramienta con cierta denominación.

            b) Es malvado: se cumple si su nombre empieza con Wa.

            c) Puede comprar una herramienta: esto sucede si tiene el dinero suficiente para pagar el precio de la misma.


    3) Saber si una herramienta es buena, cumpliendose solamente si tiene empuñadura de hierro que sale más de $10000 o es un martillo con mango de madera o goma.
    

    4) Todo plomero necesita comprar una herramienta, cuando lo hace paga su precio y agrega la herramienta a las suyas. Solo sucede si puede pagarlo.


    5) Hay un sinfín de reparaciones que los plomeros deben resolver. Cada una de ellas goza de una descripción del problema a reparar y un requerimiento que varía dependiendo de la reparación. 
    Por ejemplo, una filtración de agua requiere que un plomero tenga una llave inglesa en su caja de herramientas.
        
        a) Modelar las reparaciones y la filtración de agua.
        
        b) Saber si una reparación es difícil: esto ocurre cuando su descripción es complicada, es decir que tiene más de 100 caracteres y además es un grito, es decir está escrito totalmente 
        en mayúsculas.
        
        c) Saber el presupuesto de una reparación, el cual se calcula como el 300% de la longitud de su descripción (por eso es importante describir los problemas de manera sencilla).
    

    6) Hacer que un plomero haga una reparación. Si no puede resolverla te cobra $100 la visita. Si puede hacerla, cobra el dinero por el presupuesto de la misma y agrega esa reparación a su 
    historial de reparaciones, además de:

        Si el plomero es malvado, le roba al cliente un destornillador con mango de plástico, claramente su precio es nulo.

        Si no es malvado y la reparación es difícil, pierde todas sus herramientas buenas.

        Si no es malvado ni es difícil la reparación, sólo se olvida la primera de sus herramientas.

    Un plomero puede hacer una reparación si cumple su requerimiento o es un plomero malvado con un martillo.
    
    
    7) Nintendo, pese a ser una empresa de consolas y juegos, gana millones de dólares con su red de plomeros. Cada plomero realiza varias reparaciones en un día. Necesitamos saber 
    cómo afecta a un plomero una jornada de trabajo. Bajan línea desde Nintendo que no usemos recursividad.

    
    8) Nintendo beneficia a sus plomeros según ciertos criterios, es por eso que necesita saber, dado un conjunto de reparaciones a realizar en una jornada laboral, cuál de todos sus empleados es:
        
        a) El empleado más reparador: El plomero que más reparaciones tiene en su historial una vez realizada su jornada laboral.

        b) El empleado más adinerado: El plomero que más dinero tiene encima una vez realizada su jornada laboral.

        c) El empleado que más invirtió: El plomero que más plata invertida tiene entre las herramientas que le quedaron una vez realizada su jornada laboral.

-}

module SuperMarioBros where
    import Text.Show.Functions
    import Data.List(genericLength)
    import Data.Char(isUpper)

    --------------
    -- Punto 01 --
    --------------

    data Plomero = Plomero {
        nombre :: String,
        dinero :: Float,
        reparaciones :: [Reparacion],
        herramientas :: [Herramienta]
    } deriving (Show)

    data Herramienta = Herramienta {
        denominacion :: String,
        material :: Material,
        precio :: Float
    } deriving (Show, Eq)

    data Material = Hierro | Madera | Goma | Plastico deriving (Show, Eq)

    mario:: Plomero
    mario = Plomero "Mario" 1200 [] [Herramienta "Llave Inglesa" Hierro 200, Herramienta "Martillo" Madera 20]

    wario:: Plomero
    wario = Plomero "Wario" 0.50 [] (map (Herramienta "Llave Francesa" Hierro) [1..])

    --------------
    -- Punto 02 --
    --------------

    tiene :: String -> Plomero -> Bool
    tiene unaDenominacion = any ((== unaDenominacion) . denominacion) . herramientas

    esMalvado :: Plomero -> Bool
    esMalvado = (== "Wa") . take 2 . nombre

    puedeComprar :: Herramienta -> Plomero -> Bool
    puedeComprar unaHerramienta = (>= precio unaHerramienta) . dinero

    --------------
    -- Punto 03 --
    --------------

    esBuena :: Herramienta -> Bool
    esBuena (Herramienta _          Hierro   precio) = precio > 10000
    esBuena (Herramienta "Martillo" material      _) = elem material [Madera, Goma]
    esBuena _                                        = False

    --------------
    -- Punto 04 --
    --------------

    comprar :: Herramienta -> Plomero -> Plomero
    comprar unaHerramienta unPlomero
        | puedeComprar unaHerramienta unPlomero = perderDinero (precio unaHerramienta) . agregarHerramienta unaHerramienta $ unPlomero
        | otherwise = unPlomero

    perderDinero :: Float -> Plomero -> Plomero
    perderDinero unDinero = mapDinero (subtract unDinero)

    agregarHerramienta :: Herramienta -> Plomero -> Plomero
    agregarHerramienta unaHerramienta = mapHerramientas ((:) unaHerramienta)

    mapDinero :: (Float -> Float) -> Plomero -> Plomero
    mapDinero f unPlomero = unPlomero { dinero = f $ dinero unPlomero }

    mapHerramientas :: ([Herramienta] -> [Herramienta]) -> Plomero -> Plomero
    mapHerramientas f unPlomero = unPlomero { herramientas = f $ herramientas unPlomero }

    --------------
    -- Punto 05 --
    --------------

    data Reparacion = Reparacion {
    descripcion :: String,
    requerimiento :: Plomero -> Bool
    } deriving (Show)

    filtracionDeAgua :: Reparacion
    filtracionDeAgua = Reparacion "Filtración de agua" (tiene "Llave Inglesa")

    esDificil :: Reparacion -> Bool
    esDificil (Reparacion descripcion _) = length descripcion >= 50 && esUrgente descripcion

    esUrgente :: String -> Bool
    esUrgente caracteres = all isUpper caracteres

    presupuesto :: Reparacion -> Float
    presupuesto = (*3) . genericLength . descripcion

    --------------
    -- Punto 06 --
    --------------

    reparar :: Reparacion -> Plomero -> Plomero
    reparar unaReparacion unPlomero
        | puedeResolver unaReparacion unPlomero = agregarReparacion unaReparacion . cambiarHerramientasSegun unaReparacion . aumentarDinero (presupuesto unaReparacion) $ unPlomero
        | otherwise = aumentarDinero 100 unPlomero

    agregarReparacion :: Reparacion -> Plomero -> Plomero
    agregarReparacion unaReparacion unPlomero = unPlomero { reparaciones = unaReparacion : reparaciones unPlomero }

    puedeResolver :: Reparacion -> Plomero -> Bool
    puedeResolver unaReparacion unPlomero = requerimiento unaReparacion unPlomero || tiene "Martillo" unPlomero && esMalvado unPlomero

    cambiarHerramientasSegun :: Reparacion -> Plomero -> Plomero
    cambiarHerramientasSegun unaReparacion unPlomero
        | esMalvado unPlomero     = agregarHerramienta (Herramienta "Destornillador" Plastico 0) unPlomero
        | esDificil unaReparacion = mapHerramientas (filter esBuena) unPlomero
        | otherwise               = mapHerramientas (drop 1) unPlomero

    aumentarDinero :: Float -> Plomero -> Plomero
    aumentarDinero unDinero = mapDinero (+ unDinero)

    --------------
    -- Punto 07 --
    --------------

    diaDeLaburo :: [Reparacion] -> Plomero -> Plomero
    diaDeLaburo unosReparacions unPlomero = foldl (flip reparar) unPlomero unosReparacions

    --------------
    -- Punto 08 --
    --------------

    empleadoMasReparador :: [Reparacion] -> [Plomero] -> Plomero
    empleadoMasReparador = empleadoMas (length . reparaciones)

    empleadoMasAdinerado :: [Reparacion] -> [Plomero] -> Plomero
    empleadoMasAdinerado = empleadoMas dinero

    empleadoMasInvertidor :: [Reparacion] -> [Plomero] -> Plomero
    empleadoMasInvertidor = empleadoMas (sum . map precio . herramientas)

    empleadoMas :: Ord b => (Plomero -> b) -> [Reparacion] -> [Plomero] -> Plomero
    empleadoMas f unosReparacions = maximumBy (f . diaDeLaburo unosReparacions)

    maximumBy :: Ord b => (a -> b) -> [a] -> a
    maximumBy = foldl1 . maxBy

    maxBy :: Ord b => (a -> b) -> a -> a -> a
    maxBy f x y
        | f x > f y = x
        | otherwise = y