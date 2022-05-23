module Queue1 where


--VARIABLE 1: Se encola por el final de la lista y se desencola por adelante.

data Queue a = Q [a] Int deriving Show


-- O(1)
emptyQ :: Queue a 
emptyQ = Q [] 0


-- O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q ls n) = null ls


--O(n)
queue :: a -> Queue a -> Queue a
queue a (Q ls n) = Q (ls ++ [a]) (n+1)


-- O(1)
--PRECONDICION: La lista de la Queue no debe estar vacía.
firstQ :: Queue a -> a
firstQ (Q ls n) = head ls 


-- O(1)
--PRECONDICION: La lista de la Queue no debe estar vacía.
dequeue :: Queue a -> Queue a
dequeue (Q ls n) = Q (tail ls) (n-1)


lenQ :: Queue a -> Int 
lenQ (Q ls n) = n  









































