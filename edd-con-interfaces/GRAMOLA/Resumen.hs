module Resumen where 

--Interfaz del tipo abstracto Resumen 
data Resumen = R
			   Int -- cant observaciones
			   Int -- obs maxima
			   Int -- suma de observaciones



--PROP: Crea un nuevo resumen.
--O(1)
newR :: Resumen
newR = R 0 0 0 


--PROP: Agrega una observación
--O(1)
addR :: Int -> Resumen -> Resumen
addR obs (R cant m sum) = R (cant+1) (max obs m) (sum + obs)


--PROP: Devuelve la cant total de observaciones
--O(1)
cantR :: Resumen -> Int 
cantR (R cant m sum) = cant 


--Prec.: Debe haber al menos una observación.
maxR :: Resumen -> Int 
maxR (R cant m sum) = m



--PROP: Denota el promedio de las observaciones.
--PREC: Hay al menos una observación
--O(1)
promedioR :: Resumen -> Int 
promedioR (R cant m sum) = div sum cant 


























































