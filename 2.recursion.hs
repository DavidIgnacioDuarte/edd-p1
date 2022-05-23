-- Ejercicio 1 ---------------------------------------------------------------
-- PATTERN MATCHING ----------------------------------------------------------
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty x = False

head' :: [a] -> a
head' [] = error "imposible calcular el primer elemento de una lista vacía"
head' (x:xs) = x

tail' :: [a] -> [a]
tail' [] = error "imposible calcular la cola de una lista vacía"
tail' (x:xs) = xs


-- Ejercicio 2 --------------------------------------------------------------
-- RECURSION ----------------------------------------------------------------
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

{- Para una lista vacía, se devuelve lo mismo al no haber sucesor.
   Caso contrario, se le suma 1 a cada valor de la lista.-}
sucesores :: [Int] -> [Int]
sucesores [] = []
--sucesores (x:xs) = [x+1] ++ sucesores xs 
sucesores (x:xs) = (x+1) : sucesores xs

{- Se analiza el primer elemento de la lista como un par dentro de la lista(())
   y a "z" luego de igual forma, con la recursión. -}
sumaDePares :: [(Int, Int)] -> [Int]
sumaDePares [] = []
sumaDePares ((x,y):z) = [x+y] ++ sumaDePares z


conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x:xs) = x && conjuncion xs


disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs


-- pertenece 3 [1,2,3,4,5] = True
pertenece :: Eq a => a -> [a] -> Bool
pertenece a [] = False
pertenece a (x:xs) = a == x || pertenece a xs


-- apariciones True [True, True, False, True, False] = 3
apariciones :: Eq a => a -> [a] -> Int
apariciones a [] = 0
apariciones a (x:xs) = sumaSi (a==x) + apariciones a xs

sumaSi :: Bool -> Int 
sumaSi True = 1
sumaSi False = 0


-- losMenoresA 3 [1,2,3,4,5,6] = [1,2]
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA y [] = []
losMenoresA y (x:xs) = if y > x 
					   then x : losMenoresA y xs
					   else losMenoresA y xs


-- losDistintosA 1 [1,2,3,1] = [2,3]
losDistintosA :: Eq a => a -> [a] -> [a]
losDistintosA a [] = []
losDistintosA a (x:xs) = if a /= x 
						 then x:losDistintosA a xs
						 else losDistintosA a xs


-- longitudes [[1,2,3], [4,5], [6,7,8,9]] = [3,2,4]
longitudes :: [[a]] -> [Int]
longitudes [] = [] 
longitudes (x:xs) = longitud x : longitudes xs 
--longitudes [[1,2], [3], []] = longitud [1,2] : [1,0]
--								2 : [1,0] = [2,1,0]


-- lasDeLongitudMayorA 2 [[1], [2,3], [4,5,6]] = [[4,5,6]]
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA y [] = []
lasDeLongitudMayorA y (x:xs) = if longitud x > y 
							   then x:lasDeLongitudMayorA y xs
							   else lasDeLongitudMayorA y xs


-- intercalar False [True, True, True] = [True, False, True, False, True]
intercalar :: a -> [a] -> [a]
intercalar y [] = []
intercalar y (x:[]) = [x]
intercalar y (x:xs) = x : y : intercalar y xs


-- snoc [1,2,3] 4 = [1,2,3,4]
snoc :: [a] -> a -> [a]
snoc [] y = [y]
snoc (x:xs) y = x : snoc xs y


append :: [a] -> [a] -> [a]
append [] lista2 = lista2
append (x:xs) lista2 = x : append xs lista2
-- RECURSIÓN EN LISTA2 DEBIDO A =
--ej: append [1,2,3] [4,5,6]
-- 1 : ( append [2,3] [4,5,6] )
-- 1 : ( 2 : ( append [3] [4,5,6] ) )
-- 1 : ( 2 : ( 3: ( append [] [4,5,6] ) ) ) <- CASO BORDE(append [] lista2)
-- 1 : ( 2 : ( 3: ( [4,5,6] ) ) )
-- 1 : 2 : 3 : [4,5,6]
-- [1,2,3,4,5,6]


-- "concat" predefinida en Haskell
-- aplanar [[1], [2,3], [4,5,6]] = [1,2,3,4,5,6]
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = append x (aplanar xs)


-- "reverse" predefinida en Haskell
-- reversa [5,4,3,2,1] = [1,2,3,4,5]
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = append (reversa xs) [x]
-- equivalente a
-- reversa (x:xs) = reversa xs ++ [x] || snoc (reversa xs) x
-- reversa [5,4,3,2,1] = reversa [4,3,2,1] ++ [5]
-- reversa [3,2,1] ++ [4] ++ [5]
-- reversa [2,1] ++ [3] ++ [4] ++ [5]
-- reversa [1] ++ [2] ++ [3] ++ [4] ++ [5]
-- reversa [] ++ [1] ++ [2] ++ [3] ++ [4] ++ [5]
-- [1,2,3,4,5]


-- funcion "max", "head" y "tail" predefinidas en Haskell
-- zipMaximos [1,4,7,12] [0,5,10,11] = [1,5,10,12]
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos lista1 [] = lista1
zipMaximos [] (y:ys) = (y:ys)
zipMaximos lista1 (y:ys) = max (head lista1) y : zipMaximos (tail lista1) ys 
--equivalente a
-- zipMaximos [] [] = []
-- zipMaximos (x:xs) [] = (x:xs)
-- zipMaximos [] (y:ys) = (y:ys)
-- zipMaximos (x:xs) (y:ys) = max x y : zipMaximos xs ys


-- PRECONDICIÓN: Ambas listas deben tener la misma longitud.
zipSort :: [Int] -> [Int] -> [(Int, Int)] 
zipSort lista1 [] = [] -- se asume que también lista1 está vacía
zipSort lista1 (y:ys) = if (head lista1) < y
						then ((head lista1), y) : zipSort (tail lista1) ys
						else (y, (head lista1)) : zipSort (tail lista1) ys

-- "zip" predefinida en Haskell
zipSort2 :: [Int] -> [Int] -> [(Int, Int)]
zipSort2 [] [] = []
zipSort2 [] (y:ys) = []
zipSort2 (x:xs) [] = []
zipSort2 (x:xs) (y:ys) = (min x y, max x y) : zipSort2 xs ys


-- Recursión estructural en el cálculo de su sumatoria y longitud.
promedio :: [Int] -> Int
promedio [] = 0
promedio xs = div (sumatoria xs) (longitud xs)


-- "minimum" predefinida en Haskell
-- "Ord" como restricción a los elementos que tienen un orden
-- letras en alfabeto, números de menor a mayor o viceversa
minimum' :: Ord a => [a] -> a
minimum' [] = error "la lista debe tener al menos 1 elemento"
minimum' (x:[]) = x -- caso borde necesario si la lista no es vacía
minimum' (x:xs) = min x (minimum' xs)


-- Recursión sobre números --------------------------------------------------
-- factorial 4 = 4*3*2*1 = 24
-- factorial 3 = 3*2*1 = 6
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
-- 4 ... factorial (n-1)
-- 4 ... factorial (3)
-- 4 ... 6 = 24 (?)
-- 4 * 6 = 24 [por ende, debo multiplicar]


-- cuentaRegresiva 3 = [3,2,1]
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n > 0
					then n : cuentaRegresiva (n-1)
					else []


-- contarHasta 4 = [1,2,3,4]
-- contarHasta 3 = [1,2,3]
contarHasta :: Int -> [Int]
contarHasta 0 = []
contarHasta n = contarHasta(n-1) ++ [n]
-- [1,2,3,4]
-- 4 ... contarHasta 3
-- 4 ... [1,2,3] (append, ++, snoc)


-- replicarN 3 True = [True, True, True]
replicarN :: Int -> a -> [a]
replicarN 0 x = []
replicarN n x = x : replicarN (n-1) x


-- desdeHasta 2 6 = [2,3,4,5,6]
-- desdeHasta 3 6 = [3,4,5,6]
-- desdeHasta 3 5 = [3,4,5]
desdeHasta :: Int -> Int -> [Int]
desdeHasta n m =  if (max n m) > (min n m) 
				  then (min n m) : desdeHasta ((min n m) + 1) (max n m)
				  else [(max n m)]
-- if 6 > 3 then 3 : desdeHasta 4 6
-- 3: ( if 6 > 4 then 4 : desdeHasta 5 6 )
-- 3: (4: ( if 6 > 5 then 5 : desdeHasta 6 6 ) )
-- 3: (4: (5: (if 6 > 6 then [6] ) ) )
-- 3:4:5:[6] = [3,4,5,6]


-- "take" predefinida en Haskell
-- takeN 3 [1,2,3,4,5,6] = [1,2,3]
takeN :: Int -> [a] -> [a]
takeN n [] = [] -- valido también cuando "n" es 0
takeN 0 (x:xs) = []
takeN n (x:xs) = x : takeN (n-1) xs
-- 1 : takeN (2) [2,3,4,5,6]
-- 1: (2: ( takeN (1) [3,4,5,6] ) )
-- 1: (2: (3: ( takeN (0) [4,5,6] ) ) )
-- 1: (2: (3: ( [] ) ) ) 
-- [1,2,3]


-- "drop" predefinida en Haskell
-- dropN 3 [1,2,3,4,5,6] = [4,5,6]
dropN :: Int -> [a] -> [a]
dropN n [] = []
dropN 0 ys = ys
dropN n (y:ys) = dropN (n-1) ys


-- splitN 3 [1,2,3,4,5,6] = ([1,2,3],[4,5,6])
splitN :: Int -> [a] -> ([a], [a])
splitN n [] = ([],[])
splitN 0 (x:xs) = ([],(x:xs))
splitN n (x:xs) = ( takeN n (x:xs) , dropN n (x:xs) )

splitN2 :: Int -> [a] -> ([a], [a])
splitN2 n [] = ([], [])
splitN2 0 (x:xs) = ([], (x:xs))
splitN2 n (x:xs) = agregarALaPrimera x ( splitN2 (n-1) xs)

agregarALaPrimera :: a -> ([a],[a]) -> ([a],[a])
agregarALaPrimera a (xs, ys) = (a:xs, ys)
-- splitN 2 [1,2,3] = ([1,2],[3])
-- agregarALaPrimera 1 ( splitN2 1 [2,3] ) == ([1], [])
-- agregarALaPrimera 2 ( splitN2 0 [3] ) == ([1,2], [])
-- = ([1,2], [3])


maximum' :: Ord a => [a] -> a 
maximum' [] = error "una lista vacía no tiene un valor máximo"
maximum' [x] = x 
maximum' (x:xs) = max x (maximum' xs)


splitMin :: Ord a => [a] -> (a, [a])
splitMin [] = error "una lista vacía no tiene un valor mínimo"
splitMin [x] = (x, [])
splitMin xs = (minimum' xs, sinElMinimo xs)


sinElMinimo :: Ord a => [a] -> [a]
sinElMinimo [] = []
sinElMinimo (x:xs) = if minimum' (x:xs) == x 
					 then xs 
					 else x : sinElMinimo xs 


ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (x:xs) = insertar x (ordenar xs)

insertar :: Ord a => a -> [a] -> [a]
insertar a [] = [a]
insertar a (x:xs) = if a < x 
					then a : (x:xs)
					else x:insertar a xs


ordenar' :: Ord a => [a] -> [a]
ordenar' [] = []
ordenar' [x] = [x]
ordenar' (x:xs) = minimum' (x:xs) : ordenar' xs 


interseccion :: Eq a => [a] -> [a] -> [a]
interseccion [] lista2 = [] 
interseccion (x:xs) lista2 = if elem x lista2 
							 then x : interseccion xs lista2 
							 else interseccion xs lista2


diferencia :: Eq a => [a] -> [a] -> [a]
diferencia [] lista2 = [] 
diferencia (x:xs) lista2 = if elem x lista2 
						   then diferencia xs lista2
						   else x : diferencia xs lista2



-- particionPorSigno [1,-1,4,-5,7,8] = ([1,4,7,8],[-1,-5])
particionPorSigno :: [Int] -> ([Int], [Int])
particionPorSigno [] = ([],[])
particionPorSigno (x:xs) = agregarSegunSigno x (particionPorSigno xs)

agregarSegunSigno :: Int -> ([Int], [Int]) -> ([Int], [Int])
agregarSegunSigno n (xs, ys) = if n >= 0 
							   then (n:xs, ys)
							   else (xs, n:ys)


particionPorParidad :: [Int] -> ([Int], [Int])
particionPorParidad [] = ([], [])
particionPorParidad (x:xs) = agregarSegunParidad x (particionPorParidad xs)

agregarSegunParidad :: Int -> ([Int], [Int]) -> ([Int], [Int])
agregarSegunParidad x (pares,impares) = if esPar x 
										then (x:pares, impares)
										else (pares, x:impares)

esPar :: Int -> Bool 
esPar n = (mod n 2) == 0


subtails :: [a] -> [[a]]
subtails [] = [[]]
subtails (x:xs) = [(x:xs)] ++ subtails xs 


agrupar :: Eq a => [a] -> [[a]]
agrupar [] = []
agrupar (x:xs) = agregarSiVale x (agrupar xs)

agregarSiVale :: Eq a => a -> [[a]] -> [[a]]
agregarSiVale e [] = [[e]]
agregarSiVale e (x:xs) = if pertenece e x 
						 then (e : x) : xs
						 else [e] : (x : xs) 


--[1,2,3] es prefijo de [2,3] y de [3]
esPrefijo :: Eq a => [a] -> [a] -> Bool
esPrefijo [] [] = False 
esPrefijo [x] [y] = False 
esPrefijo l1 [] = True 
esPrefijo [] l2 = False 
esPrefijo l1 l2 = head (reversa l1) == head (reversa l2) && 
				  esPrefijo (sinElUltimo l1) (sinElUltimo l2)



-- [2,3] es sufijo de [1,2,3] y de [0,1,2,3]
esSufijo :: Eq a => [a] -> [a] -> Bool
esSufijo [] [] = False 
esSufijo [x] [y] = False 
esSufijo l1 [] = False
esSufijo [] l2 = True
esSufijo l1 l2 = head (reversa l1) == head (reversa l2) && 
				 esSufijo (sinElUltimo l1) (sinElUltimo l2)

sinElUltimo :: [a] -> [a]
sinElUltimo [] = [] 
sinElUltimo [x] = []
sinElUltimo (x:xs) = x : sinElUltimo xs













