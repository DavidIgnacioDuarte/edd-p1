import Heap2

-- Interfaz a usar:
-- emptyH
-- isEmptyH
-- insertH
-- findMin
-- deleteMin
-- splitMin

heapSort :: Ord a => [a] -> [a]
heapSort xs = heapToList (listToHeap xs)

heapToList :: Ord a => Heap a -> [a]
heapToList h =
	if isEmptyH h
	   then []
	   else findMin h : 
	        heapToList (deleteMin h)

listToHeap :: Ord a => [a] -> Heap a
listToHeap [] = emptyH
listToHeap (x:xs) =
	insertH x (listToHeap xs)

