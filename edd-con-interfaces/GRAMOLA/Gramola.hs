module Gramola where 

import Resumen
--newR
--addR
--cantR
--maxR
--promedioR

data Gramola = G [(String, Resumen)] -- temas anteriores(en orden inverso para mantener constante) 
				  String  -- tema actual
				  Resumen -- votos del tema actual
				 [(String, Resumen)] -- temas siguientes
				  String -- tema más votado
				  Int -- cantidad de votos del tema más votado

-- INV: El nombre del tema más votado es alguno de los nombres de los temas de toda
-- la gramola, y es el que tiene mayor cantidad de votos.
-- Y el número que representa la cantidad de votos del tema más contado, debe ser
-- verdaderamente la cantidad de votos que recibió el mismo tema en toda la gramola.


--Interfaz 

--PREC: La lista no debe ser vacía.
nuevaG :: [String] -> Gramola 
nuevaG ls = G [] (head ls) newR (incorporarTemas (tail ls)) (head ls) 0 

incorporarTemas :: [String] -> [(String, Resumen)]
incorporarTemas []     = []
incorporarTemas (x:xs) = (x, newR) : incorporarTemas xs



siguienteG :: Gramola -> Gramola 
siguienteG (G ants x rx [] tmv ptmv) = G ants x rx [] tmv ptmv 
siguienteG (G ants x rx ((s,rs) : sigs) tmv ptmv) = 
		    G ((x,rx) : ants) s rs sigs tmv ptmv



anteriorG :: Gramola -> Gramola
anteriorG (G [] x rx sigs tmv ptmv) = G [] x rx sigs tmv ptmv
anteriorG (G ((a, ra) : ants) x rx sigs tmv ptmv) = 
		   G ants a ra ((x, rx) : sigs) tmv ptmv 




cancionG :: Gramola -> String
cancionG (G ants x rx sigs tmv ptmv) = x





--PREC: El puntaje debe ser un número entre 1 y 10
votarG :: Int -> Gramola -> Gramola 
votarG puntaje (G ants x rx sigs tmv ptmv) = 
				if cantR rx == ptmv 
				then G ants x (addR puntaje rx) sigs x (ptmv + 1)
				else G ants x (addR puntaje rx) sigs tmv ptmv




--Puntaje en promedio de la canción actual.
--Si nunca fue votado, devuelve 0.
puntajeG :: Gramola -> Int 
puntajeG (G ants x rx sigs tmv ptmv) = if cantR rx > 0 
									   then promedioR rx
									   else 0




temaMasVotadoG :: Gramola -> String	
temaMasVotadoG (G ants x rx sigs tmv ptmv) = tmv 