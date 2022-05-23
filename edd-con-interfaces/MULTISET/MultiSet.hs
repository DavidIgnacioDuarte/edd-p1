module MultiSet where 


data MultiSet a = MS [a] [(a, Int)] deriving Show


ms1 = MS [True, True, False, True, False] [(True, 3), (False, 2)]



--O(1)
--Denota un multiconjunto vacío
emptyMS :: MultiSet a
emptyMS = MS [] []


--O(n)
--Dados un elemento y un multiconjunto,
--agrega una ocurrencia de ese elemento al multiconjunto.
addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS x (MS [] res) = MS (x:[]) [(x,1)] 
addMS x (MS ls res) = MS (x:ls) (addAndCount x res)


addAndCount :: Eq a => a -> [(a, Int)] -> [(a, Int)]
addAndCount x [] = [(x, 1)]
addAndCount x (a:as) = if x == fst a 
					   then (x,snd a + 1) : as 
					   else a : addAndCount x as 


--O(n)
--PREC: El elemento debe existir en el MultiSet.
--Dados un elemento y un multiconjunto indica la cantidad de apariciones
--de ese elemento en el multiconjunto.
ocurrencesMS :: Eq a => a -> MultiSet a -> Int
ocurrencesMS x (MS ls (a:as)) = if x == fst a
							  then snd a 
							  else ocurrencesMS x (MS ls as)



--O(1)
--Dado un multiconjunto devuelve una lista con todos los elementos del conjunto y su
--cantidad de ocurrencias.
multiSetToList :: MultiSet a -> [(a, Int)]
multiSetToList (MS ls res) = res 
























































