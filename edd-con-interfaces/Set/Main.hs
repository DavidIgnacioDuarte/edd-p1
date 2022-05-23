import Set1
import TreeGenerico




losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] set = []
losQuePertenecen (x:xs) set = if belongsS x set 
							  then x : losQuePertenecen xs set 
							  else losQuePertenecen xs set 



sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos ls = setToListS (sinRepetidos' ls emptyS)


sinRepetidos' :: Eq a => [a] -> Set a -> Set a
sinRepetidos' [] set = set
sinRepetidos' (x:xs) set = addS x (sinRepetidos' xs set)



unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS 
unirTodos (NT set t1 t2) = unionS set (unionS (unirTodos t1) (unirTodos t2))




















