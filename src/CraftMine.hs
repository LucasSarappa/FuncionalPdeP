{- PARCIAL CRAFTMINE:

En este videojuego, existen personajes cuyo objetivo es minar materiales y construir nuevos objetos.

Los personajes tienen un nombre, un puntaje y un inventario con todos los materiales que poseen. De un mismo material se pueden tener varias unidades.

Para ello, se define el siguiente tipo de dato

data Personaje = UnPersonaje {
	nombre:: String,
	puntaje:: Int,
	inventario:: [Material]
} deriving Show

Craft

Craftear consiste en construir objetos a partir de otros objetos. Para ello se cuenta con recetas que consisten en una lista de materiales que se requieren para craftear un nuevo objeto. 
En ninguna receta hace falta más de una unidad de mismo material. La receta también especifica el tiempo que tarda en construirse. Todo material puede ser componente de una receta y todo 
objeto resultante de una receta también es un material y puede ser parte en la receta de otro.

Por ejemplo:

    para hacer una fogata, se necesita madera y fósforo y se tarda 10 segundos
    para hacer pollo asado, se necesita fogata y un pollo, pero se tarda 300 segundos
    para hacer un sueter, se necesita lana, agujas y tintura, y demora 600 segundos

    1. Hacer las funciones necesarias para que un jugador craftee un nuevo objeto:

        El jugador debe quedar con el nuevo objeto en su inventario

        El jugador debe quedar sin los materiales usados para craftear

        La cantidad de puntos del jugador se incrementa a razón de 10 puntos por segundo utilizado en el crafteo.

        El objeto se craftea sólo si se cuenta con todos los materiales requeridos antes de comenzar la tarea. En caso contrario, no se altera el inventario, pero se pierden 100 puntos.
        
    Por ejemplo, si un jugador con 1000 puntos tenía un sueter, una fogata y dos pollos y craftea un pollo asado, mantiene su sueter intacto, se queda con un sólo pollo, sin fogatas 
    y pasa a tener un pollo asado en su inventario. Su puntaje pasa a ser 4000.

    2. Dado un personaje y una lista de recetas: 

        Encontrar los objetos que podría craftear un jugador y que le permitirían como mínimo duplicar su puntaje. 

        Hacer que un personaje craftee sucesivamente todos los objetos indicados en la lista de recetas. 

        Averiguar si logra quedar con más puntos en caso de craftearlos a todos sucesivamente en el orden indicado o al revés.


Mine

El mundo del videojuego se compone de biomas, donde cada bioma tiene muchos materiales. Para poder minar en un bioma particular el personaje debe tener un elemento necesario según 
el bioma. Por ejemplo, en un bioma ártico, donde hay hielo, iglues y lobos, se debe tener un suéter. 

Cuando un personaje va a minar a un bioma, si cuenta con el elemento necesario, agrega a su inventario uno de los materiales del bioma y gana 50 puntos. 
La forma de elegir cuál es el material del bioma a conseguir, depende de la herramienta que use al minar. Por ejemplo, el hacha hace que se mine el último de los materiales del bioma, 
mientras que la espada actúa sobre el primero de ellos. Existe tambien el pico, que por ser más preciso permite apuntar a una determinada posición de los materiales. 
Por ejemplo, si un personaje con un sueter en su inventario mina el artico con un pico de precisión 1, agrega un iglú a su inventario. En caso de no poder minar por no tener lo 
necesario el personaje se va con las manos vacías y sigue como antes.

    1. Hacer una función minar, que dada una herramienta, un personaje y un bioma, permita obtener cómo queda el personaje.

    2. Definir las herramientas mencionadas y agregar dos nuevas. Mostrar ejemplos de uso. Hacerlo de manera que agregar en el futuro otras herramientas no implique modificar la 
    función minar.

        a. Utilizando la función composición, usar una que permita obtener un material del medio del conjunto de materiales.
        b. Utilizando una expresión lambda, inventar una nueva herramienta, diferente a las anteriores

    3. ¿Qué pasa al intentar minar en un bioma con infinitos materiales? Mostrar ejemplos donde con diferentes herramientas o personajes sucedan diferentes cosas. Justificar. 

-}
data Jugador = UnJugador {
    nombre :: String,
    puntaje :: Int,
    inventario :: [Material]
} deriving Show

-- PARTE 1

type Material = String

data Receta = Receta {
    materiales :: [Material],
    tiempo :: Int,
    resultado :: Material
}

fogata,fosforo, madera,polloAsado,pollo,sueter,hielo,lobos,iglues :: Material
fogata = "fogata"
fosforo = "fosforo"
madera = "madera"
pollo = "pollo"
polloAsado = "pollo asado"
sueter = "sueter"
hielo = "hielo"
iglues = "iglues"
lobos = "lobos"

intentarCraftear :: Receta -> Jugador ->  Jugador
intentarCraftear receta jugador
    | tieneMateriales receta jugador  =  craftear receta jugador 
    | otherwise = alterarPuntaje (-100) jugador 

craftear :: Receta -> Jugador -> Jugador
craftear receta = alterarPuntaje (10*tiempo receta).agregarMaterial (resultado receta).quitarMateriales  (materiales receta)


-- Auxiliares
tieneMateriales :: Receta -> Jugador -> Bool
tieneMateriales  receta jugador = all (tieneMaterial jugador) (materiales receta)

tieneMaterial :: Jugador -> Material -> Bool
tieneMaterial jugador material = elem material (inventario jugador)

agregarMaterial :: Material -> Jugador -> Jugador
agregarMaterial material jugador = jugador {inventario = material:inventario jugador }

quitarMateriales :: [Material] -> Jugador -> Jugador
quitarMateriales materiales jugador = jugador{inventario = foldr quitarUnaVez (inventario jugador) materiales}

quitarUnaVez:: Eq a => a -> [a] -> [a]
quitarUnaVez _ [] = []
quitarUnaVez material (m:ms)  
 | material == m = ms
 | otherwise = m:quitarUnaVez material ms 

alterarPuntaje :: Int -> Jugador ->  Jugador
alterarPuntaje n jugador  = jugador {puntaje = puntaje jugador + n}

{-
Ejemplos:
ghci> intentarCraftear recetaPollo maria
UnJugador {nombre = "maria", puntaje = 4000, inventario = ["pollo asado","pollo","sueter"]}
-}

recetaFogata :: Receta
recetaFogata = Receta [madera, fosforo] 10 fogata

recetaPollo :: Receta
recetaPollo = Receta [fogata, pollo] 300 polloAsado

juan, maria :: Jugador
juan = UnJugador "juan" 20 [madera, fosforo, pollo, sueter]
maria = UnJugador "maria" 1000 [fogata, pollo, pollo, sueter]

unasRecetas :: [Receta]
unasRecetas = [recetaFogata, recetaPollo]

crafteablesDuplicadores :: [Receta] -> Jugador -> [Material]
crafteablesDuplicadores recetas jugador = map resultado (filter (duplicaLuegoDeCraftear jugador) recetas)

duplicaLuegoDeCraftear ::  Jugador -> Receta -> Bool
duplicaLuegoDeCraftear jugador receta = puntaje (intentarCraftear receta jugador ) > 2 * puntaje jugador

craftearSucesivamente :: Jugador -> [Receta] ->  Jugador
craftearSucesivamente = foldr intentarCraftear

masPuntosAlReves ::  Jugador -> [Receta] -> Bool
masPuntosAlReves jugador listaDeRecetas = puntaje (craftearSucesivamente jugador (reverse listaDeRecetas)) > puntaje (craftearSucesivamente jugador listaDeRecetas)

{-
Ejemplos:
ghci> intentarCraftear recetaPollo maria
UnJugador {nombre = "maria", puntaje = 4000, inventario = ["pollo asado","pollo","sueter"]}

ghci> crafteablesDuplicadores unasRecetas maria
["pollo asado"]
ghci> crafteablesDuplicadores unasRecetas juan
["fogata"]

ghci> craftearSucesivamente juan (reverse unasRecetas)
UnJugador {nombre = "juan", puntaje = 3120, inventario = ["pollo asado","sueter"]}
ghci> craftearSucesivamente juan unasRecetas
UnJugador {nombre = "juan", puntaje = 20, inventario = ["fogata","pollo","sueter"]}

ghci> masPuntosAlReves juan unasRecetas
True
-}

-- PARTE 2


data Bioma = UnBioma{
    materialesPresentes :: [Material],
    materialNecesario :: Material
}

biomaArtico :: Bioma
biomaArtico = UnBioma [hielo, iglues, lobos] sueter

type Herramienta = [Material] -> Material

hacha :: Herramienta
hacha = last

espada :: Herramienta
espada = head 

pico :: Int -> Herramienta
pico = flip (!!) 
    
posicionMitad :: Herramienta
posicionMitad lista = pico (length lista `div` 2) lista

minar :: Herramienta -> Bioma -> Jugador  -> Jugador
minar herramienta bioma jugador 
    | tieneMaterial jugador (materialNecesario bioma)  = agregarMaterial (herramienta (materialesPresentes bioma)) (alterarPuntaje 50 jugador)
    | otherwise = jugador
{-
EJEMPLOS DE USO DE HERRAMIENTAS:

ghci> minar hacha biomaArtico juan
UnJugador {nombre = "juan", puntaje = 70, inventario = ["lobos","madera","fosforo","pollo","sueter"]}

ghci> minar (pico 1) biomaArtico juan
UnJugador {nombre = "juan", puntaje = 70, inventario = ["iglues","madera","fosforo","pollo","sueter"]}

-}

-- PARTE 3

listaPollosInfinitos :: [String]
listaPollosInfinitos = pollo : listaPollosInfinitos

{-
> minar espada (UnBioma listaPollosInfinitos sueter) juan
UnJugador {nombre = "juan", puntaje = 1050, inventario = ["pollo","madera","fosforo","pollo crudo","sueter"]}

-}