module Heap(
	Heap,
	emptyH,
	isEmptyH,
	insertH,
	findMin,
	deleteMin,
	splitMin,
	maxH,
	deleteMaxH
 )
where

data Heap a = H [a] deriving (Eq,Show)

-- Inv. Rep.:
-- ?

-- Devuelve una heap vacia
-- Eficiencia: O(1)
emptyH :: Heap a
emptyH = H []

-- Dice si una heap esta vacia
-- Eficiencia: O(1)
isEmptyH :: Heap a -> Bool
isEmptyH (H xs) = null xs

-- Agrega un elemento a la heap
-- Eficiencia: O(1)
insertH :: Ord a => a -> Heap a -> Heap a
insertH x (H xs) = H (x:xs)

-- Encuentra el minimo elemento en la heap
-- Eficiencia: O(n)
findMin :: Ord a => Heap a -> a
findMin (H xs) = minimum xs

-- Borra el minimo elemento de la heap
-- Eficiencia: O(n)
deleteMin :: Ord a => Heap a -> Heap a
deleteMin (H xs) = 
	H (deleteOnce (minimum xs) xs)

-- Eficiencia: O(n)
deleteOnce :: Eq a => a -> [a] -> [a]
deleteOnce e [] = []
deleteOnce e (x:xs) =
	if e == x
	   then xs
	   else x : deleteOnce e xs

-- Hace findMin y deleteMin
-- Eficiencia: O(n)
splitMin :: Ord a => Heap a -> (a, Heap a)
splitMin h =
	(findMin h, deleteMin h)

-- O(1)
maxH :: Ord a => Heap a -> a
maxH (H xs) = maximum xs

-- O(log M)
deleteMaxH :: Ord a => Heap a -> Heap a 
deleteMaxH (H xs) = 
	H (deleteOnce (maximum xs) xs)