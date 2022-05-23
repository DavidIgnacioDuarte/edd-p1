module Emulador where 
--Un emulador es un sistema donde se cargan los programas y se ejecutan en simultáneo. 
import Programa 


data Emulador = E Memoria (Heap (Int, Programa))
				  Int --Instante actual
				  Int --Cantidad de programas pendientes

{-
INV REP:
	La cantidad de programas pendientes coincide con el número de programas en la heap.
	La heap no debe contener programas vacíos.
-}

--Interfaz HEAP 
--emptyH   :: Heap a 
--queue    :: Int -> a -> Heap a --Determina también su prioridad
--isEmptyh :: Heap a -> Bool   
--proximoH :: Heap a -> a 
--dequeue  :: Heap a -> Heap a 


--Crea un nuevo emulador de tamaño Int, que es el tamaño de su memoria.
--O(n log n)
nuevoE  :: Int -> Emulador 
nuevoE n = E (memoriaConCeros n) emptyH 0 0

memoriaConCeros :: Int -> Memoria 
memoriaConCeros n = if n == 0
					then emptyM 
					else insertM (n-1) 0 (memoriaConCeros (n-1))



--Se carga un programa al emulador y devuelve un emulador con el programa cargado. Ademas
--recibe un número entero, que determina en qué momento se va a ejecutar ese programa.
--O(log P) donde p es la cantidad de programas cargados en el sistema.
cargarE :: Int -> Programa -> Emulador -> Emulador 
cargarE tInicio prog (E memoria heap tActual pendientes) = 
		if not (null prog)
		 then E memoria (queue tInicio (tInicio, prog) heap) 
		        tActual (pendientes + 1)
		 else E memoria heap tActual pendientes 



--Avanza 1 solo paso de 1 solo programa. Y a ese programa le avanza el tiempo.
--O(log p + log M)
emularE :: Emulador -> Emulador 
emularE (E memoria heap tActual pendientes) = 
		if isEmptyh heap 
		then E memoria heap (tActual + 1) pendientes 
		else let (tInicio, programa) = proximoH heap in 
			 if tInicio > tActual 
			 then E memoria heap (tActual + 1) pendientes 
			 else emularSiguiente (E memoria heap tActual pendientes)

emularSiguiente :: Programa -> Emulador -> Emulador 
emularSiguiente (E memoria heap tActual pendientes) = 
				let (tInicio, instr : programa) = proximoH heap in 
			    E (ejecutarI memoria i) --O(log M)
			      ( if null programa
			      	then dequeue heap 
			      	else queue (tInicio+1) (tInicio+1, programa) (dequeue heap)) --O(log p)
			      (tActual + 1)
			      (if null programa
			       then pendientes - 1
			       else pendientes )




--Devuelve cual es el valor almacenado en la direccion de memoria dada en el emulador.
--O(log M)
leerE   :: Direccion -> Emulador -> Int 
leerE d (E memoria heap tActual pendientes) = 
		fromJust(lookupM d memoria)



--Devuelve cuantos programas que están cargados en el emulador se encuentran
--pendientes de ejecución.
--O(1)
nroPendientesE :: Emulador -> Int
nroPendientesE (E memoria heap tActual pendientes) = pendientes 








































