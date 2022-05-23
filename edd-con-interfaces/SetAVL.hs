module SetAVL (
	Set,
	emptyS,
	belongsS,
    addS,
	removeS,
	setToList
  ) 
where

import AVL

data Set a = S (AVL a)

-- Eficiencia: O(1)
emptyS :: Set a
emptyS = S emptyAVL

-- Eficiencia: O(log n)
belongsS :: Ord a => a -> Set a -> Bool
belongsS x (S t) = hasValue (findAVL x t)

hasValue (Just _) = True
hasValue Nothing  = False

-- Eficiencia: O(log n)
addS :: Ord a => a -> Set a -> Set a
addS x (S t) = S (insertAVL x t)

-- Eficiencia: O(log n)
removeS :: Ord a => a -> Set a -> Set a
removeS x (S t) = S (deleteAVL x t)

-- Eficiencia: O(n)
setToList :: Set a -> [a]
setToList (S t) = inorderAVL t