------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------- CLASE 4y5: COMPOSICION Y APLICACION PARCIAL: https://www.youtube.com/watch?v=CSMljUwpE-s&list=PL2xYJ49ov_dc1hCGcRMvu8VU3jexRUjf3&index=5&pp=iAQB------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------

module Clase4y5 where
-- Alias de tipos--

-- type es un alias de tipos, es decir, un nombre alternativo para un tipo ya existente. 
-- Puede ayudar a desambiguar, aportan semantica y mas expresividad PERO no hace falta usarlo siempre --

type Edad = Int
esMayor :: Edad -> Bool
esMayor edad = edad >= 18

-- APLICACION PARCIAL:--
{- Las funciones pueden ser aplicadas con menos parametros de los que esperan. Osea, si a una funcion
que espera una cantidad n de parametros, la aplicamos con m valores (siendo m<n), la estamos aplicando parcialmente, 
y nos queda una funcion que espera n-m parametros.
Los parametros se aplican en el orden en que estan definidos en la funcion.
-}
--  terminar de ver el video me perdi bastante :)