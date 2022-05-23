module Cola2 where


--Variante 2: El próximo elemento se encuentra al final de la lista [a]
data Cola a = CC [a]

--Interfaz del módulo Cola2

--CONSTANTE-- O(1)
vaciaC :: Cola a 
vaciaC = CC []



--CONSTANTE-- O(1)
estaVaciaC :: Cola a -> Bool 
estaVaciaC (CC c) = null c



--CONSTANTE-- O(1)
encolarC :: a -> Cola a -> Cola a
encolarC a (CC c) = CC (a : c)



--LINEAL-- O(n)
--PRECONDICIÓN: La cola no debe estar vacía.
proximoC :: Cola a -> a 
proximoC (CC c) = last c 



--LINEAL-- O(n)
--PRECONDICIÓN: La cola no debe estar vacía.
desencolarC :: Cola a -> Cola a
desencolarC (CC c) = init c --QUITA EL ÚLTIMO, RECORRIDO SOBRE LA LISTA.


last :: [a] -> a 
last (x:xs) = if null xs 
		  	  then x 
		  	  else last xs 


-- 3 constantes, 2 lineales.











