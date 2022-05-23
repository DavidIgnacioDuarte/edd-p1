import BinaryHeap


---- Eficiencia: O(1)
--emptyH :: Heap a

---- Eficiencia: O(1)
--isEmptyH :: Heap a -> Bool

----Eficiencia: O(1)
--findMin :: Ord a => Heap a -> a

----Eficiencia: O(log n)
--insertH :: Ord a => a -> Heap a -> Heap a

---- Eficiencia: O(log n)
--deleteMin :: Ord a => Heap a -> Heap a



--O(n * log n)
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
















