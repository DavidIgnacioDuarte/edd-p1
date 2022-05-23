module Mago (
    Mago,
    crearM,
    nombre,
    aprender,
    hechizos
)
where

import Set

data Mago = ElMago Nombre (Set Hechizo) deriving Show

type Nombre = String
type Hechizo = String

-- O(1)
crearM :: Nombre -> Mago
crearM n = (ElMago n emptyS)

-- O(1)
nombre :: Mago -> Nombre
nombre (ElMago n _) = n

-- O(log H)
aprender :: Hechizo -> Mago -> Mago 
aprender h (ElMago n hs) = (ElMago (n) (addS h hs))

-- O(1)
hechizos :: Mago -> Set Hechizo
hechizos (ElMago _ hechizos) = hechizos

--aprender "asd" (ElMago "Harry" emptyS)
