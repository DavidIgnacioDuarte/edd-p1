module Stack1(
	Stack,
	emptyS,
	isEmptyS,
	push,
	pop,
	top,
	maxS
 )
where

data Stack a = S [a] (Maybe a)

-- Inv. Rep.
-- Sea S xs m,
-- 1) si m es Nothing,
--    la lista está vacía
-- 2) si m es Just x,
--    x es el maximo de xs

-- Eficiencia: O(1)
emptyS :: Stack a
emptyS = S [] Nothing

-- Eficiencia: O(1)
isEmptyS :: Stack a -> Bool
isEmptyS (S xs m) = null xs

-- Eficiencia: O(1)
push :: Ord a => a -> Stack a -> Stack a
push x (S xs m) = S (x:xs) (updateMax x m)

updateMax x Nothing  = Just x
updateMax x (Just y) = Just (max x y)

-- Eficiencia: O(1)
top :: Stack a -> a
top (S xs m) = head xs

-- No cumple con el enunciado
-- Eficiencia: O(n)
pop :: Stack a -> Stack a
pop (S xs m) = S (tail xs)
                 (buscarMax (tail xs))

-- Eficiencia: O(n)
buscarMax [] = Nothing
buscarMax xs = Just (maximum xs)

-- Eficiencia: O(1)
maxS :: Ord a => Stack a -> a
maxS (S xs m) = fromJust m

fromJust (Just x) = x