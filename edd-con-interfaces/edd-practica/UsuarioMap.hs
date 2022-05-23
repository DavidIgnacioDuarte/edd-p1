import Map1
import Set1

-- Interfaz de Map
-- emptyM
-- assocM
-- lookupM
-- deleteM
-- domM

-- Interfaz de Set
--emptyS,
--addS,
--belongsS,
--sizeS,
--removeS,
--unionS,
--intersectionS,
--setToListS


-- Prec.: el argumento no es Nothing
fromJust :: Maybe a -> a
fromJust (Just x) = x



--O(n^2)
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM map = valuesM' map (domM map)

valuesM' :: Eq k => Map k v -> [k] -> [Maybe v]
valuesM' map []     = [] 
valuesM' map (k:ks) = lookupM k map : valuesM' map ks  





--O(n)
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] map = True 
todasAsociadas (k:ks) map = existe k map && todasAsociadas ks map 

existe :: Eq k => k -> Map k v -> Bool 
existe k map = case lookupM k map of 
			   Nothing -> False 
			   (Just k) -> True 





--O(n)
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap []     = emptyM
listToMap (p:ps) = assocM (fst p) (snd p) (listToMap ps)






--O(n^2)
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList map = pairs (domM map) map 

pairs :: Eq k => [k] -> Map k v -> [(k, v)]
pairs [] map = []
pairs (k:ks) map = (k, fromJust (lookupM k map)) : pairs ks map 







--O(n^2)
unirDoms :: Eq k => [Map k v] -> Set k
unirDoms maps = mapsToSet ( doms maps )

doms :: [Map k v] -> [k] 
doms []     = [] 
doms (m:ms) = domM m ++ doms ms 

mapsToSet :: Eq k => [k] -> Set k 
mapsToSet [] = emptyS
mapsToSet (k:ks) = addS k (mapsToSet ks)







incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar [] map = map 
incrementar (k:ks) map = case lookupM k map of
						 (Just a) -> updateIM k a (incrementar ks map) 
						 Nothing  -> incrementar ks map

updateIM :: Eq k => k -> Int -> Map k Int -> Map k Int 
updateIM k n map = assocM k (n+1) map 







mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
mergeMaps map1 map2 = assocM' (mapToList map1) map2

assocM' [] map = map
assocM' (kv:kvs) map = assocM (fst kv) (snd kv) (assocM' kvs map)

					  






























