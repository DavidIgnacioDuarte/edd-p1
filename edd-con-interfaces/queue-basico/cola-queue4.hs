module Cola4 where 


--Variante 3: con 2 listas (frente y dorso) (front & back)
data Cola a = CC [a] [a]
--INV REP= Sea CC fs bs, si fs está vacía, la cola se encuentra vacía.



--CONSTANTE-- O(1)
vaciaC :: Cola a
vaciaC = CC [] []



--CONSTANTE-- O(1)
estaVaciaC :: Cola a -> Bool 
estaVaciaC (CC f b) = null f 



--CONSTANTE-- O(1)
encolarC :: a -> Cola a -> Cola a
encolarC a (CC f b) = CC f (a:b)



primeroQ :: Queue a -> a 
primeroQ (CC f b) = head b 



desencolarC :: Queue a -> Queue a 
desencolarC (CC f b) = if null frente 
					   then CC (tail (reverse b)) [] --O(n) nueva implementación reverse
					   else CC (tail f) b --O(1)




reverse :: [a] -> [a]
reverse xs =  reverse' xs []

reverse' :: [a] -> [a] -> [a]
reverse' [] l2     = l2 
reverse' (x:xs) l2 = reverse' xs (x : l2)


--3 constantes, 2 lineales

--LINEAL-- O(n) solo si estoy en el peor caso.
proximoC :: Cola a -> a
proximoC (CC f b) = if null f 
					then last dorso 
					else head f


last :: [a] -> a 
last (x:xs) = if null xs 
		  	  then x 
		  	  else last xs 