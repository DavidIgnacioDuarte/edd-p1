-- ¿qué tienen en común?
-- Los constructores no toman parámetros
-- Enumerando posibles casos de valores de ese tipo

data Dir   = Norte | Sur | Este | Oeste deriving Show

-- data Bool = True | False

data TipoDePokemon = Fuego | Planta | Agua deriving Show

-- Registros
-- ¿qué tienen en común?
-- Tienen un sólo constructor
-- Dicho constructor toma parámetros
-- Esos parámetros van a ser los campos
-- del registro
data Persona = ConsP Int String deriving Show
data Pokemon = ConsPokemon String TipoDePokemon deriving Show
data Entrenador = ConsE String [Pokemon] deriving Show

nombre :: Persona -> String
nombre (ConsP e n) = n

incrementarEdad :: Persona -> Persona
incrementarEdad (ConsP e n) = ConsP (e+1) n 

-- ¿Registros con posibles casos?

data Figura = Circulo Float -- radio en cm
            | Rectangulo Float Float -- ancho y alto en cm
            deriving Show

superficie :: Figura -> Float
superficie (Circulo r) = pi * r ^ 2
superficie (Rectangulo ancho alto) = ancho * alto

-- No es una representacion deseable
-- data Figura = ConsFigura TipoDeFigura Int Int
-- data TipoDeFigura = Circulo | Rectangulo

-- No es tan interesante
data Contenedor a = C a deriving Show

-- Pero este es más interesante
-- data Maybe a = Nothing | Just a

cantidadM :: Maybe a -> Int
cantidadM Nothing  = 0
cantidadM _        = 1

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

-- Prec.: el parámetro debe ser Just x
fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = error "nothing no tiene elementos"

-- Función parcial
-- head :: [a] -> a
-- head (x:xs) = x
-- head []     = error "lista sin elementos"

-- Función total
headM :: [a] -> Maybe a
headM (x:xs) = Just x
headM []     = Nothing

sumM :: Maybe Int -> Maybe Int -> Maybe Int
sumM Nothing  _        = Nothing
sumM _        Nothing  = Nothing
sumM (Just x) (Just y) = Just (x+y)

divM :: Int -> Int -> Maybe Int
divM x 0 = Nothing
divM x y = Just (div x y)

tailM :: [a] -> Maybe [a]
tailM [] = Nothing
tailM (x:xs) = Just xs

lastM :: [a] -> Maybe a
lastM []     = Nothing
lastM [x]    = Just x
lastM (x:xs) = lastM xs

maximo :: [Int] -> Int
maximo [x]    = x
maximo (x:xs) = max x (maximo xs)

maxM :: Maybe Int -> Maybe Int -> Maybe Int
maxM Nothing  _        = Nothing
maxM _        Nothing  = Nothing
maxM (Just x) (Just y) = Just (max x y)

maximoM :: [Int] -> Maybe Int
maximoM []     = Nothing
maximoM [x]    = Just x
maximoM (x:xs) = maxM (Just x) (maximoM xs)
 -- y esta otra es más molesta pero funciona
 -- Just (max x (fromJust (maximoM xs)))

-- data Maybe a = Nothing | Just a

-- esquema sobre Maybe
-- f Nothing   = ...
-- f (Just x)  = ... x ...

-- ¿Qué tiene de diferente esta definición
-- a las anteriores?

-- data [a] = [] | a : [a]

-- esquema sobre listas
-- f [] = ...
-- f (x:xs) = ... f xs ...

data Color = Azul | Rojo deriving Show
data Celda = ConsCelda [Color] deriving Show
data SecuenciaDeCeldas = 
	  UnitSec Celda
	| ConsSec Celda SecuenciaDeCeldas
	deriving Show

proximaCelda :: SecuenciaDeCeldas -> SecuenciaDeCeldas
proximaCelda (ConsSec c cs) = cs
proximaCelda (UnitSec c)    = 
	error "no hay proxima celda"

cantidadDeCeldas :: SecuenciaDeCeldas -> Int
cantidadDeCeldas (UnitSec c)    = 1
cantidadDeCeldas (ConsSec c cs) =
	1 + cantidadDeCeldas cs

-- el esquema de secuencias
-- f (UnitSec c) = ...
-- f (ConsSec c cs) = ... f cs ...

cantBolitas :: SecuenciaDeCeldas -> Int
cantBolitas (UnitSec c)    = cantBolitasCelda c
cantBolitas (ConsSec c cs) =
   cantBolitasCelda c + cantBolitas cs

cantBolitasCelda :: Celda -> Int
cantBolitasCelda (ConsCelda xs) = length xs

celdaVacia = ConsCelda []
ejemplo =
 ConsSec celdaVacia (UnitSec celdaVacia)

------------------------------------------
-- enumerativo
data Objeto =
	  Espada
	| Chatarra
	| Oro Int
	deriving Show

-- registro
data Cofre = ConsCofre Objeto 
   deriving Show

-- tipo recursivo
data Camino =
	  Fin -- caso base
	| Punto Cofre Camino -- caso recursivo
	deriving Show

-- esquema sobre Camino
-- f Fin = ...
-- f (Punto cofre camino) =
--	... f camino ...

longitudCamino :: Camino -> Int
longitudCamino Fin = 0
longitudCamino (Punto cofre camino) =
	1 + longitudCamino camino

objetos :: Camino -> [Objeto]
objetos Fin = []
objetos (Punto cofre camino) =
	objetoDelCofre cofre : objetos camino

objetoDelCofre :: Cofre -> Objeto
objetoDelCofre (ConsCofre obj) = obj

caminoConUnaEspada :: Camino
caminoConUnaEspada = Punto (ConsCofre Espada) Fin

-- longitudCamino' camino = 
-- 	length (objetos camino)

-- también lo puedo representar así
-- type Camino = [Cofre]
-- caminoConUnaEspada :: Camino
-- caminoConUnaEspada = ConsCofre Espada : []
-- longitudCamino camino = length camino

-- y se asemeja a...
-- type String = [Char]

data Pizza = Prepizza
           | Capa Ingrediente Pizza
           deriving Show

data Ingrediente = Salsa
                 | Queso
                 | Jamon
                 | Aceitunas Int
                 deriving Show

soloQueso :: Pizza
soloQueso = Capa Queso Prepizza

jamonYQueso :: Pizza
jamonYQueso = Capa Jamon soloQueso

-- conCuatroAceitunas :: Pizza -> Pizza
-- conCuatroAceitunas pizza = 
-- 	Capa (Aceitunas 4) pizza

conNAceitunas :: Int -> Pizza -> Pizza
conNAceitunas n pizza = 
	Capa (Aceitunas n) pizza

conCuatroAceitunas :: Pizza -> Pizza
conCuatroAceitunas pizza = 
	conNAceitunas 4 pizza

-- data Pizza = Prepizza
--            | Capa Ingrediente Pizza

capas :: Pizza -> [Ingrediente]
capas Prepizza = []
capas (Capa ing p) =
	ing : capas p