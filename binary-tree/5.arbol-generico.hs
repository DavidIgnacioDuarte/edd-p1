data Tree a = EmptyT
			| NT a (Tree a) (Tree a)
			  deriving Show

-- ARBOLES REPRESENTADOS -----------------------------------------------------------------
arbol1 :: Tree Int
arbol1 =  NT 1 -- nodo raiz
			 (NT 2 -- primer subárbol (izquierdo)
				(NT 3 EmptyT EmptyT) --hoja
				(NT 33 EmptyT EmptyT) ) --hoja
			 (NT 22 -- segundo subárbol (derecho) y hoja
				(EmptyT)
				(EmptyT)) 

arbol2 :: Tree Bool
arbol2 = NT True
			(NT False
				(NT True (NT False EmptyT EmptyT) (EmptyT))
				(NT True EmptyT EmptyT))
			(NT False
				(NT True (NT False (NT True EmptyT EmptyT) (EmptyT)) (EmptyT) ) 
				(NT False EmptyT EmptyT))

arbol3 :: Tree String 
arbol3 = NT "nodo1nivel0"
			(NT "nodoDnivel1"
				(NT "nodoDnivel2" (NT "nodoInivel3" EmptyT EmptyT)
								  (NT "nodoDnivel3" (NT "nodoDNivel4" EmptyT EmptyT) EmptyT))
				(NT "nodoInivel2" EmptyT EmptyT))
			(NT "nodoInivel1"
				(NT "nodoDnivel2" (NT "nodoInivel3" EmptyT EmptyT) EmptyT)
				(EmptyT))
-----------------------------------------------------------------------------------------

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NT a t1 t2) = a + sumarT t1 + sumarT t2



sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NT a t1 t2) = 1 + sizeT t1 + sizeT t2



mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NT n t1 t2) = NT (n*2) (mapDobleT t1) (mapDobleT t2) 



mapLongitudT :: Tree String -> Tree Int
mapLongitudT EmptyT = EmptyT
mapLongitudT (NT n t1 t2) = NT (length n) (mapLongitudT t1) (mapLongitudT t2)



perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT a EmptyT = False
perteneceT a (NT n t1 t2) = a == n || 
							(perteneceT a t1) ||
							(perteneceT a t2)



aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT a EmptyT = 0
aparicionesT a (NT n t1 t2) = if a == n 
							  then 1 + (aparicionesT a t1) + (aparicionesT a t2)
							  else (aparicionesT a t1) + (aparicionesT a t2)


countLeaves :: Tree a -> Int
countLeaves EmptyT = 0
countLeaves (NT n t1 t2) = if (isLeaf (NT n t1 t2))
						   then 1 + (countLeaves t1) + (countLeaves t2)
						   else (countLeaves t1) + (countLeaves t2)


isLeaf :: Tree a -> Bool
isLeaf EmptyT = False
isLeaf (NT n (EmptyT) (EmptyT)) = True
isLeaf (NT n _ _) = False



leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NT n t1 t2) = if (isLeaf (NT n t1 t2))
					  then [n] ++ (leaves t1) ++ (leaves t2)
					  else (leaves t1) ++ (leaves t2)


heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NT n t1 t2) = 1 + (max (heightT t1) (heightT t2))


-- CON RECURSIÓN EXPLICITA
countNotLeaves :: Tree a -> Int
countNotLeaves EmptyT = 0
countNotLeaves (NT n t1 t2) = if (isLeaf (NT n t1 t2))
							  then 0 + (countNotLeaves t1) + (countNotLeaves t2)
							  else 1 + (countNotLeaves t1) + (countNotLeaves t2)

-- SIN RECURSIÓN EXPLICITA
countNotLeaves' :: Tree a -> Int
countNotLeaves' tree = (sizeT tree) - (countLeaves tree)



mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT 
mirrorT (NT n t1 t2) = NT n (mirrorT t2) (mirrorT t1)



listInOrder :: Tree a -> [a]
listInOrder EmptyT = []
listInOrder (NT n t1 t2) = (listInOrder t1) ++ [n] ++ (listInOrder t2)


elementos :: Tree a -> [a]
elementos EmptyT = []
elementos (NT n t1 t2) = [n] ++ (elementos t1) ++ (elementos t2)



listPreOrder :: Tree a -> [a]
listPreOrder EmptyT = []
listPreOrder (NT n t1 t2) = [n] ++ (listPreOrder t1) ++ (listPreOrder t2) 



listPosOrder :: Tree a -> [a]
listPosOrder EmptyT = []
listPosOrder (NT n t1 t2) = (listInOrder t1) ++ (listInOrder t2) ++ [n]



concatenarListasT :: Tree [a] -> [a]
concatenarListasT EmptyT = []
concatenarListasT (NT n t1 t2) = (concatenarListasT t1) ++ n ++ (concatenarListasT t2)



levelN :: Int -> Tree a -> [a]
levelN 0 (NT x t1 t2) = [x]
levelN n EmptyT = []
levelN n (NT x t1 t2) = (levelN (n-1) t1) ++ (levelN (n-1) t2)

{- Explicación de un salame en 200 líneas de código
levelN :: Int -> Tree a -> [a]
levelN 0 tree = valorComoLista tree
levelN n tree = if (isEmpty tree)
					  	then []
					  	else (levelN (n-1) (ramaIzq tree)) ++
					  		 (levelN (n-1) (ramaDer tree))


valorComoLista :: Tree a -> [a]
valorComoLista EmptyT = []
valorComoLista (NT n t1 t2) = [n]

isEmpty :: Tree a -> Bool
isEmpty EmptyT = True
isEmpty _ = False

ramaIzq :: Tree a -> Tree a 
ramaIzq (NT n t1 t2) = t1

ramaDer:: Tree a -> Tree a 
ramaDer (NT n t1 t2) = t2
-}


--PRIMERA SOLUCIÓN--------------------------------------------------------------------
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NT n t1 t2) = [n] : (zipListas (listPerLevel t1) (listPerLevel t2))

zipListas :: [[a]] -> [[a]] -> [[a]]
zipListas [] ys = ys
zipListas xs [] = xs
zipListas (x:xs) (y:ys) = (x ++ y) : (zipListas xs ys)
--------------------------------------------------------------------------------------
--SEGUNDA SOLUCIÓN--------------------------------------------------------------------
listPerLevel' :: Tree a -> [[a]]
listPerLevel' tree = nivelesEntre 0 (heightT tree) tree

nivelesEntre :: Int -> Int -> Tree a -> [[a]]
nivelesEntre desde hasta tree = if desde == hasta
										then []
										else (levelN desde tree) : 
										     (nivelesEntre (desde+1) hasta tree)
-------------------------------------------------------------------------------------



ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NT n t1 t2) = if (heightT t1) > (heightT t2)
						 	then n : elementos t1
						 	else n : elementos t2


{-
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NT n t1 t2) = [n] : caminosHastaHojas t1 
-}


























