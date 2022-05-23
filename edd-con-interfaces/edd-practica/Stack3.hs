module Stack3(
	Stack,
	emptyS,
	isEmptyS,
	push,
	pop,
	top,
	maxS
 )
where

data Stack a = S [a] [a]

-- Inv. Rep.
-- Sea S xs ms,
-- 1) ms tiene la misma
--    cantidad de elementos
--    que xs
-- 2) Si ms tiene elementos,
--    el head de ms
--    es el maximo de xs

-- Eficiencia: O(1)
emptyS :: Stack a
emptyS = S [] []

-- Eficiencia: O(1)
isEmptyS :: Stack a -> Bool
isEmptyS (S xs ms) = null xs

-- Eficiencia: O(1)
push :: Ord a => a -> Stack a -> Stack a
push x (S xs ms) = S (x:xs) 
                     (updateMax x ms)

-- Eficiencia: O(1)
updateMax :: Ord a => a -> [a] -> [a]
updateMax x [] = [x]
updateMax x (m:ms) =
	max x m : m : ms

-- Eficiencia: O(1)
top :: Ord a => Stack a -> a
top (S xs ms) = head xs

-- No cumple con el enunciado
-- Eficiencia: O(1)
pop :: Stack a -> Stack a
pop (S xs ms) = S (tail xs)
                  (tail ms)

-- Eficiencia: O(1)
maxS :: Ord a => Stack a -> a
maxS (S xs ms) = head ms