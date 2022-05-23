-- Ejercicio 1 ---------------------------------------------------------------
{- sucesor :: Int -> Int
   Dado un número devuelve su sucesor -}

sucesor :: Int -> Int
sucesor x = x + 1


{- sumar :: Int -> Int -> Int
   Dados dos números devuelve su suma utilizando la operación + -}

sumar :: Int -> Int -> Int
sumar x y = x + y


{- maximo :: Int -> Int -> Int
   Dados dos números devuelve el mayor de estos. -}

maximo a b = if a > b
			 then a
			 else b



-- Ejercicio 2 -------------------------------------------------------------
-- en consola, ej: sumar (maximo 1 3) (maximo 3 5) ---> = 5 + 3 = 8



-- Ejercicio 3 PATTERN MATCHING --------------------------------------------
{- primera :: (Int,Int) -> Int
   Dado un par de números devuelve la primera componente.
   Definida en Haskell como fst. -}

primera (a, b) = a


{- segunda :: (Int,Int) -> Int
   Dado un par de números devuelve la segunda componente.
   Definida en Haskell como snd. -}

segunda (a, b) = b


{- sumaPar :: (Int,Int) -> Int
   Dado un par de números devuelve su suma. -}

sumaPar(a, b) = a + b


{- maxDelPar :: (Int,Int) -> Int
   Dado un par de números devuelve el mayor de estos. -}

maxDelPar(a, b) = maximo a b



-- Ejercicio 4 ------------------------------------------------------------
{- Definir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste.
   Luego implementar las siguientes funciones:
   a) opuesto :: Dir -> Dir
   Dada una dirección devuelve su opuesta.
   b) siguiente :: Dir -> Dir
   Dada una dirección devuelve su siguiente, en sentido horario. -}

data Dir = Norte
		 | Sur
		 | Este
		 | Oeste
		 deriving Show

opuesto :: Dir -> Dir
opuesto Norte = Sur 
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = Norte



-- Ejercicio 5 -------------------------------------------------------------
{- Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes,
   Miércoles, Jueves, Viernes, Sabado y Domingo. Supongamos que el primer día
   de la semana es lunes, y el último es domingo. Luego implementar las 
   siguientes funciones:
  a) primerDia :: DiaDeSemana
  Devuelve el primer dia de la semana.
  b) ultimoDia :: DiaDeSemana
  Devuelve el ultimo dia de la semana.
  c) nroDeDia :: DiaDeSemana -> Int
  d) empiezaConM :: DiaDeSemana -> Bool
  Dado un dia de la semana devuelve V o F dado el caso.
  e) estaEnElMedio :: DiaDeSemana -> Bool
  Dado un dia de la semana indica si no es ni el primer ni el ultimo dia.
  f ) vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
  Dado dos dias de semana, indica si el primero viene desp que el segundo.-}

data DiaDeSemana = Lunes
				 | Martes
				 | Miercoles
				 | Jueves
				 | Viernes 
				 | Sabado
				 | Domingo
				 deriving Show

primerDia :: DiaDeSemana
primerDia = Lunes

ultimoDia :: DiaDeSemana
ultimoDia = Domingo

nroDeDia :: DiaDeSemana -> Integer
nroDeDia Lunes = 1
nroDeDia Martes = 2
nroDeDia Miercoles = 3
nroDeDia Jueves = 4
nroDeDia Viernes = 5
nroDeDia Sabado = 6
nroDeDia Domingo = 7

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Lunes = False
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM Jueves = False
empiezaConM Viernes = False
empiezaConM Sabado = False
empiezaConM Domingo = False

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Martes = True
estaEnElMedio Miercoles = True
estaEnElMedio Jueves = True
estaEnElMedio Viernes = True
estaEnElMedio Sabado = True
estaEnElMedio Domingo = False

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues dia1 dia2 = (nroDeDia dia1) > (nroDeDia dia2)



-- Ejercicio 6 ---------------------------------------------------------------
{- Los booleanos también son un tipo de enumerativo. Un booleano es True o False.
   Defina las siguientes funciones utilizando pattern matching (no usar las
   funciones sobre booleanos ya definidas en Haskell):
   a) negar :: Bool -> Bool
   Dado un booleano, si es True devuelve False, y si es False devuelve True.
   En Haskell ya está definida como not.
   b) and :: Bool -> Bool -> Bool
   Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
   En Haskell ya está definida como &&.
   c) or :: Bool -> Bool -> Bool
   Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve
   False.
   En Haskell ya está definida como ||. -}

negar :: Bool -> Bool
negar True = False
negar False = True

and :: Bool -> Bool -> Bool
and True True = True
and False True = False
and True False = False
and False False = False

or :: Bool -> Bool -> Bool
or True True = True
or True False = True
or False True = True
or False False = False



-- Ejercicio 7 --------------------------------------------------------------
{- Defina las siguientes funciones polimórficas:
   a) loMismo :: a -> a
   Dado un elemento de algún tipo devuelve ese mismo elemento.
   b) siempreSiete :: a -> Int
   Dado un elemento de algún tipo devuelve el número 7.
   c) duplicar :: a -> (a,a)
   Dado un elemento de algún tipo devuelve un par con ese elemento en ambas
   componentes. -}

loMismo :: a -> a
loMismo a = a 

siempreSiete :: a -> Int
siempreSiete a = 7

duplicar :: a -> (a,a)
duplicar a = (a,a)



-- Ejercicio 8 --------------------------------------------------------------
{- Responda la siguiente pregunta: ¿Por qué estas funciones son polimórficas?-}

-- Son polimórficas ya que se comportan igual para todos los tipos. Cualquier
-- asignación es válida, al ser funciones totales.