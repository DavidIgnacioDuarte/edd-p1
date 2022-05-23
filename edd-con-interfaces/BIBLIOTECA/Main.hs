import Biblioteca 


--Complejidad: O(n (log n + log F))
devolverTodos :: Fecha -> Biblioteca -> Biblioteca
devolverTodos fechaActual biblioteca =
			  devolverTodos' (librosB biblioteca)
			  				  fechaActual
			  				  biblioteca 


--Complejidad: O(n * (log n + log F))
devolverTodos' :: [Libro] -> Fecha -> Biblioteca -> Biblioteca
devolverTodos' [] fechaActual biblioteca = biblioteca 
devolverTodos' (libro:libros) fechaActual biblioteca = 
			   if estaPrestado libro biblioteca --O(log n)
			   then devolverB (devolverTodos' libros fechaActual biblioteca)
			   				   --O(log n + log F)
			   				   fechaActual
			   				   libro 
			   else devolverTodos' libros fechaActual biblioteca


estaPrestado :: Libro -> Biblioteca
estaPrestado libro biblioteca = 
			 case consultarB bib libro of --O(log n)
			 	  Inexistente -> False
			 	  Disponible  -> False
			 	  Prestado f  -> True 






















