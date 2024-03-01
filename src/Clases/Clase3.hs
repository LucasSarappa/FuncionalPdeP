------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------- CLASE 3: DATAS Y TUPLAS: https://www.youtube.com/watch?v=6tEGnH_FYGY&list=PL2xYJ49ov_dc1hCGcRMvu8VU3jexRUjf3&index=5------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------

-- -------------------- DATA Y TUPLAS: --------------------

module Clase3 where

-- Queremos modelar estudiantes de quienes conocemos su nombre, legajo y nota. Necesitamos saber si un estudiante aprobo. ¿De que tipo querriamos que sea esa funcion?

{- 

PARA DEFINIR UN DATA:

data tipo_de_dato = nombre_constructor {
     atributo1 :: tipo,
     atributo2 :: tipo,
      atributo3 :: tipo
}

pepito :: tipo_de_dato
pepito = nombre_constructor {
    nombre = "Pepito",
    legajo = 1234,
    nota = 7
}

-}


data Estudiante = UnEstudiante {
    nombre :: String,
    legajo :: String,
    nota :: Int
} deriving (Show, Eq)


juanita :: Estudiante
juanita = UnEstudiante {
    nombre = "Juanita",
    legajo = "123456-7",
    nota = 8
}

pepito :: Estudiante
pepito = UnEstudiante {
    nombre = "Pepe",
    legajo = "654321-7",
    nota = 6
}

aprobo :: Estudiante -> Bool
aprobo estudiante = nota estudiante >= 6


-- PATERN MATCHING CON DATAS:--
-- El constructor es una funcion, por lo tanto se puede hacer pattern matching con el. :t UnEstudiante --> UnEstudiante :: String -> String -> Int -> Estudiante
-- y ademas sirve para trabajar con Patter Matching

legajoyNombre :: Estudiante -> String
legajoyNombre (UnEstudiante legajo nombre _) = nombre ++ ", " ++ legajo


-- Para saber si a un estudiante le fue mejor que a otro en base a su nota.
-- Con pattern Matching: es mas declarativo, porque no nos importa el nombre de los atributos, solo nos importa la nota.
leFueMejor :: Estudiante -> Estudiante -> Bool
leFueMejor (UnEstudiante _ _ mejorNota) (UnEstudiante _ _ otraNota) = mejorNota > otraNota

-- Sin pattern Matching: es mas imperativo, porque nos importa el nombre de los atributos.

{-

leFueMejor' estudiante1 estudiante2 = nota estudiante1 > nota estudiante2 

-} 

-- INMUTABILIDAD:--

-- Si queremos cambiarle la nota a un estudiante, no podemos hacerlo, porque los datos son inmutables.
-- Cambiarle la nota a alguien, parece ser un efecto, por lo tanto en funcional no se puede hacer ya que no tenemos efecto.
-- En cambio, funcional propone definir una nueva funcion cambiarNota, que reciba un estudiante y una nota, y devuelva un estudiante con esa nota cambiada.

cambiarNota :: Int -> Estudiante -> Estudiante
cambiarNota nuevaNota (UnEstudiante nombre legajo _) = UnEstudiante nombre legajo nuevaNota

{- 
Que paso? Si quiero cambiar la nota de juanita hago > nota (cambarNota 10 juanita) me devuelve 10. Pero si hago > nota juanita me devuelve 8.
Porque es esto? porque no estamos produciendo efecto, estamos creando un nuevo valor del tipo Estudiante, que tiene la nota que yo querria que tenga lñuego de cambiarle 
la nota a juanita. Por lo tanto, en funcional los datos no pueden cambiarse, pero si pueden crearse funciones que devuelvan nuevos datos con los cambios que yo quiera.

Como se trabaja si no se puede cambiar nada?
-} 

subirNota :: Estudiante -> Estudiante
subirNota estudiante = cambiarNota (nota estudiante + 1) estudiante

-- DUPLAS: FUNCIONES DE ACCESO--
-- Las duplas son un tipo de dato que tiene dos elementos, y se pueden usar para agrupar dos elementos de distinto tipo.
{- 
fst :: (a, b) -> a
snd :: (a, b) -> b
-}

-- DATA VS TUPLAS--

-- |--------------------------DATAS:----------------------------|--------------------------TUPLAS:------------------------------|
-- |* Tipos propios                                             |* Tipos predefinidos                                           |                                                    
-- |* Mayor expresividad                                        |* Se pierde la semantica de lo que representa                  | 
-- |* Incluiye azucares sintacticos para facilitar su uso       |* Solo hay funciones predefinidas para tuplas de 2 componentes |    
-- |* Herramienta idea para modelar datos de dominio            |* Puede servir para salir del paso                             |
-- |------------------------------------------------------------|---------------------------------------------------------------|

