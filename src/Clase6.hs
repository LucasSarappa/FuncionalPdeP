------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------- CLASE 6: ORDEN SUPERIOR: https://www.youtube.com/watch?v=CSMljUwpE-s&list=PL2xYJ49ov_dc1hCGcRMvu8VU3jexRUjf3&index=5&pp=iAQB------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------

module Clase6 where

-- Orden superior: funciones que reciben funciones como parametros y/o devuelven funciones como resultado.

    esMultiploDeDos :: Int -> Bool
    esMultiploDeTres :: Int -> Bool
    esMultiploDeDiez :: Int -> Bool

    esMultiploDeDos numero = mod numero 2 == 0
    esMultiploDeTres numero = mod numero 3 == 0
    esMultiploDeDiez numero = mod numero 10 == 0

    -- Para simplificar, podriamos fabricar la funcion esMultiploDe que tenga el mismo codigo pero pasando por parametro el numero por el cual queremos saber si es multiplo
    esMultiplo :: Int -> Int -> Bool
    esMultiplo divisor numero = mod numero divisor == 0