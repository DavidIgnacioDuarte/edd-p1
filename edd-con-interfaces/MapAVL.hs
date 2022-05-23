module MapAVL (
	Map,
	emptyM,
	assocM,
	lookupM,
	removeM,
	domM
  ) 
where

import AVL

data Assoc k v = OnlyKey k | KeyValue k v deriving Show

key :: Assoc k v -> k
key (OnlyKey  k)   = k
key (KeyValue k _) = k

-- Prec.: debe ser un
-- Assoc construido con KeyValue
value :: Assoc k v -> v
value (KeyValue k v) = v

instance Eq k => Eq (Assoc k v) where
  kv1 == kv2 = key kv1 == key kv2

instance Ord k => Ord (Assoc k v) where
  kv1 <= kv2 = key kv1 <= key kv2

data Map k v = M (AVL (Assoc k v)) deriving Show
-- Inv. Rep.:
-- * En el AVL sólo se guardan valores
--   construidos con KeyValue

-- Eficiencia: O(1)
emptyM :: Map k v
emptyM = M emptyAVL

-- Eficiencia: O(log n)
assocM :: Ord k => k -> v -> Map k v -> Map k v
assocM k v (M t) = M (insertAVL (KeyValue k v) t)

-- Eficiencia: O(log n)
lookupM :: Ord k => k -> Map k v -> Maybe v
lookupM k (M t) = case findAVL (OnlyKey k) t of
                    Nothing -> Nothing
                    Just kv -> Just (value kv)

-- Eficiencia: O(log n)
removeM :: Ord k => k -> Map k v -> Map k v
removeM k (M t) = M (deleteAVL (OnlyKey k) t)

-- Eficiencia: O(n)
domM :: Map k v -> [k]
domM (M t) = keys (inorderAVL t)

-- Eficiencia: O(n)
keys :: [Assoc k v] -> [k]
keys []       = []
keys (kv:kvs) = key kv : keys kvs

-- Ejemplo de implementacion de Eq
-- data Persona = P Int String

-- dni (P d _) = d

-- instance Eq Persona where
-- 	pers1 == pers2 = dni pers1 == dni pers2