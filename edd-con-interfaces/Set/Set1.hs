module Set1(
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

--VARIANTE 1: NO SE PERMITEN REPETIDOS EN LA IMPLEMENTACION


--INV REP: Sea S [a] Int, 
--1) se cumple que [a] no tiene elementos repetidos
data Set a = S 
			[a]
		    Int -- cantidad de elementos del set
		    deriving Show


-- O(1)
emptyS :: Set a 
emptyS = S [] 0


--O(n) donde n es el tamaño del set 
addS :: Eq a => a -> Set a -> Set a
addS x (S set n) = if elem x set 
				   then S set n 
				   else S (x:set) (n+1)


--O(n) donde n es el tamaño del set 
belongsS :: Eq a => a -> Set a -> Bool
belongsS x (S set n) = elem x set 


-- O(1)
sizeS :: Eq a => Set a -> Int 
sizeS (S set n) = n 


--O(n)
--PRECONDICION: El elemento "a" debe existir en el Set.
removeS :: Eq a => a -> Set a -> Set a 
removeS y (S (x:xs) n) = if y == x
					     then S xs (n-1)
					     else S (x : remove y xs) (n-1)

remove :: Eq a => a -> [a] -> [a]
remove y (x:xs) = if y == x
				  then xs 
				  else x : remove y xs 



--O(n)
unionS ::  Eq a => Set a -> Set a -> Set a
unionS (S set1 n1) (S set2 n2) = S (union set1 set2) 
								   (longitud (union set1 set2))

union :: Eq a => [a] -> [a] -> [a]
union [] lista2 = [] 
union (x:xs) lista2 = if elem x lista2 
							 then union xs lista2 
							 else x : union xs lista2

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs



--O(n)
intersectionS :: Eq a => Set a -> Set a -> Set a
intersectionS (S set1 n1) (S set2 n2) = S (intersection set1 set2) 
								   		  (longitud (intersection set1 set2))

intersection :: Eq a => [a] -> [a] -> [a]
intersection [] lista2 = [] 
intersection (x:xs) lista2 = if elem x lista2 
							 then x : intersection xs lista2 
							 else intersection xs lista2



--O(1)
setToListS :: Eq a => Set a -> [a]
setToListS (S set n) = set 


--3 constantes
--3 lineales
--2 cuadraticas


















