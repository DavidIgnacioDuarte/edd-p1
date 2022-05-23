module Set2(
		Set,
		emptyS,
		addS,
		belongsS,
		sizeS,
		removeS,
		unionS,
		intersectionS,
		setToListS
		)
where 

--VARIANTE 2: SE PERMITEN REPETIDOS EN LA IMPLEMENTACION 


--INV REP: No tiene
data Set a = S [a]
		    deriving Show


-- O(1)
emptyS :: Set a 
emptyS = S []


--O(1)
addS :: Eq a => a -> Set a -> Set a
addS x (S set) = S (x:set)


--O(n) donde n es el tamaño del set 
belongsS :: Eq a => a -> Set a -> Bool
belongsS x (S set) = elem x set 


-- O(n^2)
sizeS :: Eq a => Set a -> Int 
sizeS (S set) = longitud (sinRepetidos set)


--O(n)
--PRECONDICION: El elemento "a" debe existir en el Set.
removeS :: Eq a => a -> Set a -> Set a 
removeS y (S set) = S (remove y set) 

remove :: Eq a => a -> [a] -> [a]
remove y (x:xs) = if y == x
				  then remove y xs
				  else x : remove y xs 



--O(n^2)
unionS ::  Eq a => Set a -> Set a -> Set a
unionS (S set1) (S set2) = S (sinRepetidos (union set1 set2))

union :: Eq a => [a] -> [a] -> [a]
union ls1 ls2 = ls1 ++ ls2

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs



--O(n^2)
intersectionS :: Eq a => Set a -> Set a -> Set a
intersectionS (S set1) (S set2) = S (sinRepetidos(intersection set1 set2))

intersection :: Eq a => [a] -> [a] -> [a]
intersection [] lista2 = [] 
intersection (x:xs) lista2 = if elem x lista2 
							 then x : intersection xs lista2 
							 else intersection xs lista2



--O(n)
setToListS :: Eq a => Set a -> [a]
setToListS (S set) = sinRepetidos set 



sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if elem x xs
					  then sinRepetidos xs
					  else x : sinRepetidos xs 


--2 constantes
--3 lineales
--3 cuadraticas