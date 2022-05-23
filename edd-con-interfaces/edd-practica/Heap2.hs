module Heap2(
	Heap,
	emptyH,
	isEmptyH,
	insertH,
	findMin,
	deleteMin,
	splitMin
 )
where

data Heap a = H [a]

-- Inv. Rep.:
-- La lista está ordenada
-- de menor a mayor

-- Devuelve una heap vacia
-- Eficiencia: O(1)
emptyH :: Heap a
emptyH = H []

-- Dice si una heap esta vacia
-- Eficiencia: O(1)
isEmptyH :: Heap a -> Bool
isEmptyH (H xs) = null xs

-- Agrega un elemento a la heap
-- Eficiencia: O(n)
insertH :: Ord a => a -> Heap a -> Heap a
insertH x (H xs) = H (insertOrd x xs)

-- Prec.: la lista ordenada
-- Propósito: devuelve
-- una lista ordenada
-- con el elemento agregado
-- a la misma
-- Eficiencia: O(n)
insertOrd :: Ord a => a -> [a] -> [a]
insertOrd e [] = [e]
insertOrd e (x:xs) =
	if e > x
	   then x : insertOrd e xs
	   else e : x : xs

-- 8
-- [1,5,7,10,11,12]

-- ->

-- [1,5,7,8,10,11,12]

-- Encuentra el minimo elemento en la heap
-- Eficiencia: O(1)
findMin :: Ord a => Heap a -> a
findMin (H xs) = head xs

-- Borra el minimo elemento de la heap
-- Eficiencia: O(1)
deleteMin :: Ord a => Heap a -> Heap a
deleteMin (H xs) = 
	H (tail xs)

-- Hace findMin y deleteMin
-- Eficiencia: O(1)
splitMin :: Ord a => Heap a -> (a, Heap a)
splitMin h =
	(findMin h, deleteMin h)
