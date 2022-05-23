module Biblioteca where 

import Map -- implementar ambos con funciones de eficiencia logarítmica
import Set

data Biblioteca = B [Libro]              --libros en la biblioteca
					(Set Libro)			 --disponibles
					(Map Libro Fecha)	 --prestados
					(Set (Libro, Fecha)) --manipulaciones


data ResultadoConsulta = Inexistente
					   | Disponible
					   | Prestado Fecha


type Libro = Int 

type Fecha = Int 

--Interfaz del tipo abstracto de datos Biblioteca.

--O(1)
libroB :: Libro -> Biblioteca
libroB libro = B [libro]
				 (addS libro emptyS)
				  emptyM 
				  emptyS


--(Indiferente eficiencia)
--PREC: Las bibliotecas no deben tener libros en común.
juntarB :: Biblioteca -> Biblioteca -> Biblioteca
juntarB (B libros1 disponibles1 prestados1 manip1)
		(B libros2 disponibles2 prestados2 manip2) =
					B (libros1 ++ libros2)
					  (unionS disponibles1 disponibles2)
					  agregarPrestados (keysM prestados2)
					  					prestados1 prestados2 
					  (unionS manip1 manip2)



agregarPrestados :: [Libro] -> Map Libro Fecha -> Map Libro Fecha -> Map Libro Fecha
agregarPrestados []     m1 m2 = m2
agregarPrestados (l:ls) m1 m2 = insertM l 
				(fromJust (lookupM' l m1))
				(agregarPrestados ls m1 m2)				  



--O(log n + log F)
--n es la cantidad de libros, F la cantidad de fechas.
--PREC: El libro debe existir en la biblioteca.
pedirPrestadoB :: Biblioteca -> Fecha -> Libro -> Biblioteca
pedirPrestadoB (B libros disponibles prestados manip) fecha libro =
				B libros (removeS libro disponibles) --O(log n)
						 (insertM libro fecha prestados) --O(log n)
						 (addS (libro, fecha) manip) --O(log(n*F)) == O(log n + log F)


--O(log n + log F)
--PREC: El libro debe estar prestado y existir en la biblioteca.
devolverB :: Biblioteca -> Fecha -> Libro -> Biblioteca 
devolverB (B libros disponibles prestados manip) fecha libro =
		   B libros (addS libro disponibles)
		   			(removeM libro fecha prestados)
		   			(addS (libro, fecha) manip)


--O(1)
librosB :: Biblioteca -> [Libro]
librosB (B ls _ _ _) = ls


--O(log n)
consultaB :: Biblioteca -> Libro -> ResultadoConsulta 
consultaB (B [] ds ps ms) libro = Inexistente 
consultaB (B ls ds ps ms) libro = if elemS libro ds --O(log n)
								  then Disponible  
								  else case lookupM libro prestados of --O(log n)
								  	   Nothing    -> Inexistente     
								  	   Just fecha -> Prestado fecha


--O(log n + log F)
fueManipuladoB :: Biblioteca -> Fecha -> Libro -> Bool 
fueManipuladoB (B _ _ _ ms) fecha libro = elemS (libro, fecha) ms --O(log n + log F)












--PREC: El valor no puede ser Nothing.
fromJust :: Maybe a -> a
fromJust (Just f) = f 
fromJust (Nothing) = error "el valor no existe"



























