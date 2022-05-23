module Queue3 where



--VARIABLE 2: Se encola por el principio de la lista y se desencola por el final.

data Queue a = Q [a] 
				 Int --longitud de la lista


-- O(1)
emptyQ :: Queue a 
emptyQ = Q [] 0


-- O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q ls) = null ls


--O(1)
queue :: a -> Queue a -> Queue a
queue a (Q ls n) = Q (a : ls) (n+1)


-- O(n)
--PRECONDICION: La lista de la Queue no debe estar vacía.
firstQ :: Queue a -> a
firstQ (Q ls n) = last' ls 


-- O(n)
--PRECONDICION: La lista de la Queue no debe estar vacía.
dequeue :: Queue a -> Queue a
dequeue (Q ls n) = Q (withoutLast ls) n


last' :: [a] -> a 
last' [a] = a
last' (a:as) = last' as


withoutLast :: [a] -> [a]
withoutLast [a] = []
withoutLast (a:as) = a : withoutLast as  



--QUEUE CON LONGITUD CONSTANTE
lenQ :: Queue a -> Int 
lenQ (Q ls n) = n 








