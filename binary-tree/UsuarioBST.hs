import BST 

---- Eficiencia: O(log n)
--perteneceBST :: Ord a => a -> Tree a -> Bool
---- Eficiencia: O(log n)
--lookupBST :: Ord k => k -> Tree (k, v) -> Maybe v
---- Eficiencia: O(log n)
--insertT :: Ord a => a -> Tree a -> Tree a




insertBST :: Ord a => a -> Tree a -> Tree a
insertBST a EmptyT = NodeT a EmptyT EmptyT 
insertBST a tree = if not perteneceBST a tree
							  then insertT a tree 


--PREC: El elemento a debe existir en el árbol.
deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST a tree = delete' fromJust(lookupBST' a tree) tree 


fromJust (Just a) = a 


lookupBST' :: Ord a => a -> Tree a -> Tree a
lookupBST k (NodeT p ti td) =
	if k == p
	   then mergeTreeWithout ti td p
	   else if k < p 
	   		then NodeT p (lookupBST k ti) td 
	   		else NodeT p ti (lookupBST k td)



mergeTreeWithout :: Tree a -> Tree a -> Tree a 
mergeTreeWithout tree1 EmptyT x = tree1 
mergeTreeWithout EmptyT tree2 x = tree2 
mergeTreeWithout (NodeT a1 ti1 td1) (NodeT a2 ti2 td2) x =
		   let elemToRoot (NodeT a1 ti1 td1) x in 
		   deleteBST elemToRoot(mergeWith elemToRoot (NodeT a1 ti1 td1) (NodeT a2 ti2 td2))



elemToRoot :: Tree a -> a -> a
elemToRoot (NodeT x ti td) deleted = 
			if hasNoLeaveToRight td 
			then x 
			else elemToRoot td deleted




root :: Tree a -> a 
root EmptyT = error "no tiene raiz"
root (NodeT x ti td) = x


hasNoLeaveToRight :: Tree a -> Bool 
hasNoLeaveToRight (NodeT x _ EmptyT) = True 
hasNoLeaveToRight _ = False 


mergeAfterDelete :: Tree a -> a -> Tree a 
mergeAfterDelete (NodeT x ti td) =  



































