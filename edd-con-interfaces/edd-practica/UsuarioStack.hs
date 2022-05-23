import Stack3

-- No puedo, ver a S
-- ¡soy usuario!
-- lenS_u (S xs n) = n

-- Tampoco puedo hacer
-- pattern matching sobre
-- emptyS porque no es un
-- constructor
-- lenS_u emptyS = 0
-- lenS_u ... = ...

-- Eficiencia: O(n)
lenS_u :: Stack a -> Int
lenS_u s =
	if isEmptyS s
	   then 0
	   else 1 + lenS_u (pop s)

-- Eficiencia: O(n)
sumS_u :: Stack Int -> Int
sumS_u s =
	if isEmptyS s
	   then 0
	   else top s + sumS_u (pop s)

-- Eficiencia: O(n^2)
reverseS_u :: Ord a => Stack a -> Stack a
reverseS_u s =
	if isEmptyS s
		then emptyS
		else pushLast (top s)
		              (reverseS_u (pop s))

-- Eficiencia: O(n)
pushLast :: Ord a => a -> Stack a -> Stack a
pushLast x s =
	if isEmptyS s
	   then push x emptyS
	   else push (top s)
	             (pushLast x (pop s))

-- una posible
-- represetación de una pila
-- 33 <- top
-- 22
-- 10
-- 6
-- 5

-- [3,44,1,33,2]
ej_s = 
   push 3
  (push 44
  (push 1 
  (push 33
  (push 2
  emptyS))))

stackToList :: Ord a => Stack a -> [a]
stackToList s =
	if isEmptyS s
	   then []
	   else top s : stackToList (pop s)

test1 :: Bool
test1 =
	maxS ej_s == maxS (reverseS_u ej_s)

