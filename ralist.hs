

data RAList a = MkR Int (Map Int a) (Heap a)


{-
INV REP:
- El Int de la estructura representa la próxima posición a ocupar en la lista.
- La primera clave Int del Map será igual a 0.
- La clave Int de mayor valor del Map siempre será igual a la longitud de la Heap.
- Cada clave Int del Map estará asociada al índice de su valor en la Heap.
- El tamaño del Map y de la Heap será siempre igual.
-}


emptyRAL :: RAList a
emptyRAL = MkR 0 emptyM emptyH 


isEmptyRAL :: RAList a -> Bool
isEmptyRAL (MkR n map heap) = n == 0


lengthRAL :: RAList a -> Int
lengthRAL (MkR n map heap) = n + 1


get :: Int -> RAList a -> a
get indice (MkR n map heap) = fromJust (lookupM map indice)

fromJust :: Maybe a -> a 
fromJust (Just x) = x
fromJust Nothing = error "XD"


minRAL :: Ord a => RAList a -> a
minRAL (MkR n map heap) = findMin heap


add :: Ord a => a -> RAList a -> RAList a
add x (MkR n map heap) = MkR (n+1) (assocM n x map) (insertH x)


elems :: Ord a => RAList a -> [a]
elems (MkR 0 map heap) = []
elems (MkR n map heap) = findMin heap : elems (MkR n-1 map (deleteMin heap))


remove :: Ord a => RAList a -> RAList a
remove (MkR n map heap) = MkR (n-1) (deleteM n-1 map) ()



set :: Ord a => Int -> a -> RAList a -> RAList a
set indice elemento (MkR n map heap) = MkR n (replaceM )



addAt :: Ord a => Int -> a -> RAList a -> RAList a
addAt indice elemento (MkR n map heap) = 
	MkR (n+1) (addAtM n indice elemento map) (insertH heap)


addAtM :: Ord a => Int -> Int -> a -> Map Int a -> Map Int a
addAtM posMax indice elemento map = 
	assocM indice elemento (corridosDePosicion indice posMax map)

corridosDePosicion :: Ord a => Int -> Int -> Map Int a
corridosDePosicion indice posMax map =
	if indice < posMax
	then corridosDePosicion (n+1) posMax (assocM (n+1) (fromJust(lookupM map n)) (deleteM n map)) 
	else emptyM






