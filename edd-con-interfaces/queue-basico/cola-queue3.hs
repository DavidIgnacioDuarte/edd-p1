module Cola3 where 


--Variante 3: con 2 listas (frente y dorso) (front & back)
data Cola a = CC [a] [a]

--Interfaz del módulo Cola3

--CONSTANTE-- O(1)
vaciaC :: Cola a
vaciaC = CC [] []



--CONSTANTE-- O(1)
estaVaciaC :: Cola a -> Bool 
estaVaciaC (CC f b) = null f && null b



--CONSTANTE-- O(1)
encolarC :: a -> Cola a -> Cola a
encolarC a (CC f b) = CC f (a:b)



--LINEAL-- O(n) solo si estoy en el peor caso.
proximoC :: Cola a -> a
proximoC (CC f b) = if null f 
					then last dorso 
					else head f



--LINEAL-- O(n) solo si estoy en el peor caso.
desencolarC :: Cola a -> Cola a
desencolarC (CC f b) = if null frente 
					   then CC (tail (reverse b)) [] --O(n) nueva implementación reverse
					   else CC (tail f) b --O(1)



last :: [a] -> a 
last (x:xs) = if null xs 
		  	  then x 
		  	  else last xs 

reverse :: [a] -> [a]
reverse xs =  reverse' xs []

reverse' :: [a] -> [a] -> [a]
reverse' [] l2     = l2 
reverse' (x:xs) l2 = reverse' xs (x : l2)


--3 constantes, 2 lineales











































 