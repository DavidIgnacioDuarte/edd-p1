module Cola1 where


--Variante 1: El próximo elemento se encuentra al principio de la lista [a]
data Cola a = CC [a]

--Interfaz del módulo Cola1

--CONSTANTE-- O(1)
vaciaC :: Cola a 
vaciaC = CC []



--CONSTANTE-- O(1)
estaVaciaC :: Cola a -> Bool 
estaVaciaC (CC c) = null c



--LINEAL-- O(n) donde n es el tamaño de la cola
encolarC :: a -> Cola a -> Cola a
encolarC a (CC c) = CC (c ++ [a]) 



--CONSTANTE-- O(1)
--PRECONDICIÓN: La cola no debe estar vacía.
proximoC :: Cola a -> a 
proximoC (CC c) = head c 



--CONSTANTE-- O(1)
--PRECONDICIÓN: La cola no debe estar vacía.
desencolarC :: Cola a -> Cola a
desencolarC (CC c) = tail c



--4 constantes, 1 lineal














































