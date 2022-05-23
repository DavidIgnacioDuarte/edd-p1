--CALCULO DE COSTOS--------------------------------------------------------------------------

--CONSTANTE-- O(k)
head' :: [a] -> a
head' (x:xs) = x


--CONSTANTE-- O(k)
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1


--LINEAL-- O(n) donde n es el tamaño del parámetro
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)


--LINEAL-- O(n) donde n es el tamaño de la lista
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


--LINEAL-- O(n) donde n es el tamaño de la lista
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs


--LINEAL-- O(n) donde n es el tamaño de la lista
pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs


--CUADRATICA-- O(n*n)
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =
			 if elem x xs
			 then sinRepetidos xs
			 else x : sinRepetidos xs


--LINEAL-- O(n) donde n es el tamaño de la primera lista
--Recorre la primera lista 
--equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys


--LINEAL-- O(n)
concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs


--LINEAL-- O(n)
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs


--LINEAL-- O(n)
dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs


--LINEAL-- O(n)
partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)


--LINEAL-- O(n)
minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)


--LINEAL-- O(n)
sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) =
		if n == x
		then xs
		else x : sacar n xs


--LINEAL-- O(n)
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar xs =
		let m = minimo xs
		in m : ordenar (sacar m xs)


--CUADRATICA-- O(n*n)
hayRepetidos :: Eq a => [a] -> Bool
hayRepetidos [] = False 
hayRepetidos (x:xs) = if elem x xs 
					  then True
					  else False || hayRepetidos xs  


--CONSTANTE-- O(1) --es número fijo
milesimoElemento :: [a] -> a
milesimoElemento [] = error "la lista debe tener al menos mil elementos"
milesimoElemento ls = conseguirNumero 1000 ls 

conseguirNumero :: Int -> [a] -> a
conseguirNumero 0 ls = head ls 
conseguirNumero n ls = conseguirNumero (n-1) (tail ls)


--LINEAL-- O(n) donde n es la cantidad de nodos del arbol 
--cantNodos :: Tree a -> Int

--LINEAL-- O(n) donde n es la cantidad de nodos del arbol
--altura :: Tree a -> Int


--CONSTANTE-- O(1)
min3 :: Int -> Int -> Int -> Int 
min3 n1 n2 n3 = min n1 (min n2 n3)


--LINEAL-- O(n*m) donde n es la cantidad de elementos de la 1ra lista y
--                      m la cantidad de elementos de la 2da lista
--interseccion :: Eq a => [a] -> [a] -> [a]


--CUADRATICA-- O(n*n)
--reverse :: [a] -> [a]


promedio :: [Int] -> Int 
promedio [] = 0
promedio ls = div (sumatoria ls) (longitud ls)

sumatoria :: [Int] -> Int 
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs






















