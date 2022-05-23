module Map (
	Map,
	emptyM,
	assocM,
	lookupM,
	deleteM,
	domM
  )
where

data Map k v = M [(k, v)]
    deriving (Eq,Show)
-- Variante 1: se repiten los k
-- Variante 2: M [k] [v]

-- Inv. Rep.:
-- Los k no están repetidos

-- -- Lo repesta:
-- M [("Carlos", 32), ("Maria", 44), ("El Tuki", 27)]

-- -- No lo respeta los invariantes:
-- M [("Carlos", 32), ("Carlos", 34), ("El Tuki", 27)]

-- devuelve un map vacio
-- Eficiencia: O(1)
emptyM :: Map k v
emptyM = M []

-- asocia una clave k a un valor v
-- (si ya existe un valor para esa
--  clave, es reemplazado por el nuevo valor)
-- Eficiencia: O(n)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M ps) = M (asocc k v ps)

-- Eficiencia: O(n)
asocc :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
asocc k v [] = [(k, v)]
asocc k v (p:ps) =
	if k == fst p
	   then (k, v) : ps
	   else p : asocc k v ps

-- dada una clave k, devuelve el valor v
-- asociado a la misma
-- Efiencia: O(n)
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M ps) =
	lookupL k ps

-- Efiencia: O(n)
lookupL :: Eq k => k -> [(k, v)] -> Maybe v
lookupL k [] = Nothing
lookupL k (p:ps) =
	if k == fst p
	   then Just (snd p)
	   else lookupL k ps

-- dada una clave k, elimina el valor v
-- asociado a la misma

deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M ps) = M (delete k ps)

-- Efiencia: O(n)
delete :: Eq k => k -> [(k, v)] -> [(k, v)]
delete k [] = []
delete k (p:ps) =
	if k == fst p
	   then ps
	   else p : delete k ps

-- devuelve la lista de claves (sin repetidos)
domM :: Map k v -> [k]
domM (M ps) = firsts ps

firsts :: [(k, v)] -> [k]
firsts [] = []
firsts (p:ps) = fst p : firsts ps

-- Asocia diferentes tipos
-- Map DNI Persona
-- Map String String
-- Map Direccion Casa
-- Map Legajo Estudiante
-- Map Edad [Persona]
-- ...