-- Ejercicio 1 -----------------------------------------------------------------------------
data Persona = ConsP String Int 
               deriving Show

david :: Persona 
david = ConsP "David" 19
juan :: Persona
juan = ConsP "Juan" 54
laura :: Persona
laura = ConsP "Laura" 40
pablo :: Persona
pablo = ConsP "Pablo" 12


nombre :: Persona -> String
nombre (ConsP n e) = n
edad :: Persona -> Int
edad (ConsP n e) = e
crecer :: Persona -> Persona
crecer (ConsP n e) = (ConsP n (e+1))


cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nuevoN (ConsP n e) = (ConsP nuevoN e)

esMenorQueLaOtra :: Persona -> Persona -> Bool
esMenorQueLaOtra (ConsP n1 e1) (ConsP n2 e2) = e1 < e2


-- mayoresA 39 [david, juan, laura, pablo] = [ConsP "Juan" 54, ConsP "Laura" 40]
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (p:ps) = if (edad p) > n
							then p : mayoresA n ps
							else mayoresA n ps


-- prec= La lista no puede ser vacía
promedioEdad :: [Persona] -> Int
promedioEdad ps = div (sumatoriaEdades ps) (length ps)
-- a pesar de la precondición, la sumatoria de edades debe contemplar como caso base a 
-- la lista vacía, debido a que cuando termine la lista, sumará 0.
sumatoriaEdades :: [Persona] -> Int
sumatoriaEdades [] = 0
sumatoriaEdades (p:ps) = edad p + sumatoriaEdades ps


elMasViejo :: [Persona] -> Persona
elMasViejo (p:[]) = p
elMasViejo (p:ps) = if edad p > (edad (elMasViejo ps))
					then p
					else elMasViejo ps














