module Stack1(
	Stack,
	emptyS,
	isEmptyS,
	push,
	pop,
	top,
	lenS
 )
where

data Stack a = S [a] Int

-- Inv. Rep.
-- Que el Int representa
-- la cantidad de elementos de la lista

-- Este ejemplo lo cumple
-- S [1,2,3] 3

-- Estos ejemplos no lo cumplen
-- S [] 3
-- S [1,2,3] (-3)

-- Eficiencia: O(1)
emptyS :: Stack a
emptyS = S [] 0

-- Eficiencia: O(1)
isEmptyS :: Stack a -> Bool
isEmptyS (S xs n) = n == 0

-- Eficiencia: O(1)
push :: a -> Stack a -> Stack a
push x (S xs n) = S (x:xs) (n+1)

-- Eficiencia: O(1)
top :: Stack a -> a
top (S xs n) = head xs

-- Eficiencia: O(1)
pop :: Stack a -> Stack a
pop (S xs n) = S (tail xs) (n-1)

-- Eficiencia: O(1)
lenS :: Stack a -> Int
lenS (S xs n) = n