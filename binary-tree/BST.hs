module BST where

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
   deriving Show

-- Binary Search Tree (arboles de busqueda binaria)
-- Invariantes:
-- Dado un arbol NodeT x ti td
-- 1) todos los elementos de ti
--    son menores a x
-- 2) todos los elementos de td
--    son mayores a x
-- 3) ti y td son BST
-- 4) Opcional: no hay elementos repetidos

-- Prec.: el arbol es BST
-- Eficiencia: O(log n)
perteneceBST :: Ord a => a -> Tree a -> Bool
perteneceBST e EmptyT = False
perteneceBST e (NodeT x ti td) =
	if e == x
	   then True
	   else if e < x
	   	       then perteneceBST e ti
	   	       else perteneceBST e td

-- Prec.: el arbol es BST
-- Eficiencia: O(log n)
lookupBST :: Ord k => k -> Tree (k, v) -> Maybe v
lookupBST k EmptyT = Nothing
lookupBST k (NodeT p ti td) =
	if k == fst p
	   then Just (snd p)
	   else if k < fst p
	   	       then lookupBST k ti
	   	       else lookupBST k td

-- Prec.: el arbol es BST
-- Proposito: dado un arbol BST
-- devuelve un arbol BST
-- Eficiencia: O(log n)
insertT :: Ord a => a -> Tree a -> Tree a
insertT e EmptyT = NodeT e EmptyT EmptyT
insertT e (NodeT x ti td) = 
	if e == x
	   then NodeT x ti td
	   else if e < x
	   	       then NodeT x (insertT e ti) td
	   	       else NodeT x ti (insertT e td)

-- insertT 20 (insertT 10 (insertT 5 (insertT 15 EmptyT)))
-- NodeT 15 
--     (NodeT 5
--     	EmptyT 
--     	(NodeT 10 
--     		EmptyT
--     		EmptyT))
--     (NodeT 20 
--     	EmptyT 
--     	EmptyT)

-- NodeT 4 
--    (NodeT 3 
--    	  (NodeT 2 
--    	  	(NodeT 1 
--    	  		EmptyT
--    	  		EmptyT) 
--    	  	EmptyT) 
--    	  EmptyT) 
--    EmptyT




deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST a EmptyT = EmptyT 
deleteBST a (NodeT x ti td) = 
			 if a == x
			 then deleteRootBST (NodeT x ti td)
			 else if a < x 
			 	  then NodeT x (deleteBST a ti) td 
			 	  else NodeT x ti (deleteBST a td)


deleteRootBST :: Ord a => Tree a -> Tree a
deleteRootBST (NodeT x EmptyT td) = td 
deleteRootBST (NodeT x ti td) = let (a,tree) = deleteMax ti in
								NodeT a tree td 


deleteMax :: Ord a => Tree a -> (a, Tree a)
deleteMax (NodeT x ti EmptyT) = (x, ti)
deleteMax (NodeT x ti td)     = let (a, tree) = deleteMax td in 
								(a, NodeT x ti tree)


--Dado un BST devuelve un par con la raíz y el árbol sin la misma.
splitRootBST :: Ord a => Tree a -> (a, Tree a)
splitRootBST (NodeT x EmptyT EmptyT) = (x, EmptyT)
splitRootBST (NodeT x ti EmptyT)     = (x, ti)
splitRootBST (NodeT x EmptyT td)     = (x, td)
splitRootBST (NodeT x ti td)         = (x, (deleteRootBST (NodeT x ti td)))



-- Dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
splitMinBST :: Ord a => Tree a -> (a, Tree a)
splitMinBST (NodeT x EmptyT td) = (x, td)
splitMinBST tree         	    = (findMin tree, deleteMin tree)



findMin :: Ord a => Tree a -> a
findMin (NodeT x EmptyT _)      = x 
findMin (NodeT x ti td)			= findMin ti 

deleteMin :: Ord a => Tree a -> Tree a 
deleteMin (NodeT x EmptyT td) 	  = td
deleteMin (NodeT x ti td)         = NodeT x (deleteMin ti) td  
----------------------------------------------------------------------------


--Dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
splitMaxBST (NodeT x ti EmptyT) = (x, ti)
splitMaxBST tree 				= (findMax tree, deleteMax' tree)

findMax :: Ord a => Tree a -> a 
findMax (NodeT x _ EmptyT) = x 
findMax (NodeT x ti td)    = findMax td 

deleteMax' :: Ord a => Tree a -> Tree a 
deleteMax' (NodeT x ti EmptyT) = ti 
deleteMax' (NodeT x ti td)	   = NodeT x ti (deleteMax' td)
----------------------------------------------------------------------------



{-
--Dado un BST y un elemento, devuelve el máximo elemento que sea menor al elemento dado.
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorA n (EmptyT) = Nothing 
elMaximoMenorA n (NodeT x ti td) = 
				  if n > x 
				  then compareWithRight n x td 
				  else compareWithLeft n x ti 

isEmptyT :: Tree a -> Bool 
isEmptyT EmptyT = True
isEmptyT _ = False

compareWithRight :: Ord a => a -> a -> Tree a -> Maybe a 
compareWithRight n x (NodeT k ti td) = if n > k && not (isEmptyT td)
							           then compareWithRight n k td 
							     	   else if n > x
									   		then Just x
									   		else Nothing


compareWithLeft :: Ord a => a -> a -> Tree a -> Maybe a
compareWithLeft n x (NodeT k ti td) = if n < k && not (isEmptyT ti)
							           then compareWithLeft n k ti 
							     	   else if n < x 
							     	   		then Just x 
							     	   		else Nothing
-}



