module MultiSet where 

import AVL


data MultiSet a = MS [] (AVL (a, Int)) deriving Show


--O(1)
--Denota un multiconjunto vacío
emptyMS :: MultiSet a
emptyMS = MS [] emptyAVL


--O(log n)
--Dados un elemento y un multiconjunto,
--agrega una ocurrencia de ese elemento al multiconjunto.
addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS x (MS ls ms) = S (x:ls) (insertAVL' x ms)


insertAVL' :: Ord a => a -> AVL (a, Int) -> AVL (a, Int)
insertAVL' x EmptyAVL = leafAVL (x,1)
insertAVL' x (NodeAVL (a, n) h ti td) =
    if x == a
       then NodeAVL (a, n+1) h ti td
       else if x < r
               then joinAVL (a, n) (insertAVL' x ti) td
               else joinAVL (a, n) ti (insertAVL' x td)


--O(log n)
--PREC: El elemento debe existir en el MultiSet.
--Dados un elemento y un multiconjunto indica la cantidad de apariciones
--de ese elemento en el multiconjunto.
ocurrencesMS :: Eq a => a -> MultiSet a -> Int
ocurrencesMS x (MS ls (a:as)) = ocurrencesAVL'

ocurrencesAVL' :: Ord a => a -> AVL (a, Int) -> Int
ocurrencesAVL' x (NodeAVL (a,n) h ti td) =
    if x == a
    then n
    else if x < r
           then ocurrencesAVL x ti
           else ocurrencesAVL x td



--O(n)
--Dado un multiconjunto devuelve una lista con todos los elementos del conjunto y su
--cantidad de ocurrencias.
multiSetToList :: MultiSet a -> [(a, Int)]
multiSetToList (MS ls res) = inorderAVL 