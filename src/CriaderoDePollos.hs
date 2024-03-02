{-
Paradigma funcional
Criaderos de pollos
Tenemos un sistema que se ocupa de monitorear el alimento y salud de los pollos de criadero.
Un criadero tiene muchos pollos. De cada uno de ellos se conoce su peso (un número entero), su estado de
salud (booleano que indica si está sano) y una descripción (una cadena de caracteres).
1) Se necesita alimentar a los pollos que requieran alimento y obtener cómo quedan todos los pollos (tanto
los que se alimentaron como los que no). Un pollo require alimento si pesa menos de 1000 y alimentarlo
consiste en aumentar su peso en un 10%
● Hacer la función alimentar, no olvidar definir su tipo, y mostrar ejemplo de consulta y respuesta
2) Existen algunos medicamentos habilitados para pollos, lo que les provoca diferentes consecuencias:
según qué medicamento sea.
● una vacuna los deja con buena salud, y hace que su peso quede en 1000 unidades
● el antibiótico también los deja con buena salud, y aumenta su peso en tantas unidades como la el
doble de la dosis que se utilice de dicho antibiótico
● hay unas pastillitas de colores que no alteran el peso ni la salud, pero se agrega el color de la pastilla
al principio de la descripción del pollo.
Los responsables de suministrar estos medicamento son los veterinarios. Hay varios, y cada uno se
especializa en un medicamento (o eventualmente en varios que se suministran juntos) Cuando un
veterinario hace su trabajo, le suministra a todos los pollos del criadero el medicamento en el que se
especializa.
Por ejemplo:
● juan y pedro se especializan en vacunas
● ana suministran un antibiotico con dosis 5, paula tambien pero con 10 de dosis.
● tati se encarga de una pastilla verde
● susana aplica una vacuna y a continuación una pastilla azul.
● hay un veterinario bot que da placebos (que no le hacen nada a los pollos)
● está tambien tatiana, que hace el trabajo de tati y de ana.
Hacer la función trabajar, que hace que un veterinario realice su trabajo en un criadero y permite
obtener cómo quedan todos los pollos.
Definir los veterinarios enumerados como ejemplo (y las funciones que sean necesarias para ello) y
mostrar ejemplos de consulta con cada uno.
data Veterinario = UnVeterinario {
nombre:: String,
especialidad:: Pollo -> Pollo
}
3) Justificar la utilización de aplicación parcial y composición en la solución, señalando donde fueron utilizados
y explicando qué ventajas tienen.

-}

-- Crear Data
data Pollo = Pollo {
    peso:: Float,
    estado:: Bool,
    descripcion:: String
} deriving (Show, Eq)

--Ejemplos Data
pollo1 :: Pollo
pollo1 = Pollo 5000 True "Alto"

pollo2 :: Pollo
pollo2 = Pollo 500 False "Flaco"

pollo3 :: Pollo
pollo3 = Pollo 4000 False "Cantor"

listaPollos :: [Pollo]
listaPollos = [pollo1,pollo2,pollo3]

-- Modificar elementos

type PolloNuevo = Pollo -> Pollo

modificarSalud :: PolloNuevo
modificarSalud pollo = pollo { estado= True }

modificiarPeso :: (Float -> Float)-> PolloNuevo
modificiarPeso modificador pollo = pollo {peso = modificador (peso pollo)}

agregarDescripcion :: String -> PolloNuevo
agregarDescripcion desc pollo = pollo {descripcion =  desc ++ descripcion pollo }

establecerPeso :: PolloNuevo
establecerPeso pollo = pollo {peso = 1000}

--1     Use  Aplicacion Parcial
alimentar :: PolloNuevo
alimentar pollo 
    | peso pollo < 1000 = modificiarPeso (*1.10) pollo
    | otherwise = pollo

--Ejemplo consulta
--alimentar pollo1
--Pollo {peso = 5000.0, estado = True, descripcion = "Alto"} SE MANTIENE EL PESO PORQUE PESA MAS DE 1000

--alimentar pollo2
--Pollo {peso = 550.0, estado = False, descripcion = "Flaco"} COMO PESA MENOS DE 1000 SE MULTIPLICA POR 1.10

-- 2
dobleDosis :: Float -> Float
dobleDosis n = n*2

--Medicamentos

--Use Composicion
vacuna :: PolloNuevo 
vacuna  = establecerPeso . modificarSalud

--Use Composicion + Aplicacion parcial
antibiotico :: Float -> PolloNuevo
antibiotico dosis  = modificarSalud . modificiarPeso (*dobleDosis dosis)

pastilla :: String -> PolloNuevo
pastilla = agregarDescripcion  

placebo :: PolloNuevo
placebo pollo = pollo

--Creo Veterinario

data Veterinario = Veterinario {
nombre:: String,
especialidad:: Pollo -> Pollo
} 

--Ejemplos

juan :: Veterinario
juan = Veterinario "juan" vacuna

pedro :: Veterinario
pedro = Veterinario "pedro" vacuna

ana :: Veterinario
ana = Veterinario "ana" (antibiotico 5) 

paula :: Veterinario
paula = Veterinario "paula" (antibiotico 10) 

tati :: Veterinario
tati = Veterinario "tati" (pastilla "Verde")
--Use composicion
susana :: Veterinario
susana = Veterinario "susana" (vacuna . pastilla "Verde")

bot :: Veterinario
bot = Veterinario "bot" placebo
--Use composicion
tatiana :: Veterinario
tatiana = Veterinario "tatiana" (pastilla "Verde" . antibiotico 5 )

--Hace su trabajo 
trabajar :: Veterinario -> [Pollo] -> [Pollo]
trabajar _ [] = []
trabajar veterinario (x:xs) = especialidad veterinario x : trabajar veterinario xs

-- EJEMPLOS
--trabajar bot listaPollos
--[Pollo {peso = 5000.0, estado = True, descripcion = "Alto"},Pollo {peso = 500.0, estado = False, descripcion = "Flaco"},Pollo {peso = 4000.0, estado = False, descripcion = "Cantor"}]

--trabajar juan listaPollos
--[Pollo {peso = 1000.0, estado = True, descripcion = "Alto"},Pollo {peso = 1000.0, estado = True, descripcion = "Flaco"},Pollo {peso = 1000.0, estado = True, descripcion = "Cantor"}]

--trabajar paula listaPollos
--[Pollo {peso = 100000.0, estado = True, descripcion = "Alto"},Pollo {peso = 10000.0, estado = True, descripcion = "Flaco"},Pollo {peso = 80000.0, estado = True, descripcion = "Cantor"}]

--trabajar tati listaPollos
--[Pollo {peso = 5000.0, estado = True, descripcion = "VerdeAlto"},Pollo {peso = 500.0, estado = False, descripcion = "VerdeFlaco"},Pollo {peso = 4000.0, estado = False, descripcion = "VerdeCantor"}]

--3
--El uso de composicion nos permite ir haciendo "unificando" funciones
-- De forma que se vayan ejecutando en un orden
-- Ejemplo : en la funcion antibiotico
-- Primero se resuelve: *dobleDosis dosis
-- Para luego seguir con : modificiarPeso (*dobleDosis dosis)
-- y por ultimo se ejecuta la funcion : modificarSalud

--mientras que en la aplicacion parcial
-- se genera una nueva funcion sin pasar todos los parametros que la deficion propone :
--modificiarPeso (*dobleDosis dosis) --> ¡*dobleDosis dosis!
