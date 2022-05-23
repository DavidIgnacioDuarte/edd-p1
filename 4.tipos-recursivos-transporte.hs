type Cod = Int
type Ciudad = String

data Medio = Tren
		   | Micro deriving Show

data Mapa = Vacio
		  | AddT Cod Medio Ciudad Ciudad Int Mapa deriving Show
		  	  -- codigo -> origen -> destino -> tarifa


-- REPRESENTACIÓN ------------------------------------------------------------------------
mapa1 :: Mapa
mapa1 = AddT 1 Tren "Berazategui" "Quilmes" 10 (AddT 2 Micro "Avellaneda" "CABA" 20 Vacio)

mapa2 :: Mapa 
mapa2 = AddT 11 Micro "A" "B" 110 
		 (AddT 7 Tren "A" "B" 80
		  (AddT 13 Micro "B" "C" 40
		   (AddT 5 Tren "C" "D" 150 Vacio))) --orden del origen y destino irrelevantes
------------------------------------------------------------------------------------------

-- FUNCIONES -----------------------------------------------------------------------------

--Devuelve True si el codigo identifica un transporte existente
transporteExistente :: Mapa -> Cod -> Bool
transporteExistente Vacio cod = False
transporteExistente (AddT c m c1 c2 n mapa) cod = (c == cod) || 
												  (transporteExistente mapa cod)



--Devuelve una lista de las 2 ciudades terminales del transporte indicado.
--PRECONDICION: El codigo identifica un transporte existente.
terminales :: Mapa -> Cod -> [Ciudad]
terminales Vacio cod = error "el transporte con el codigo cod no existe"
terminales (AddT c m c1 c2 n mapa) cod = if c == cod
										 then [c1, c2]
										 else terminales mapa cod



-- Devuelve la tarifa del transporte indicado.
-- PRECONDICION: El codigo identifica un transporte existente
tarifa :: Mapa -> Cod -> Int
tarifa (AddT c m c1 c2 n mapa) cod = if c == cod
								   then n
								   else tarifa mapa cod



--Devuelve la lista de los trenes que llegan a la ciudad indicada.
trenesQueLlegan :: Mapa -> Ciudad -> [Cod]
trenesQueLlegan Vacio city = []
trenesQueLlegan (AddT c m c1 c2 n mapa) city = if ((esCiudad c1 city) || (esCiudad c2 city))
												  && (esTren m)
											   then c : trenesQueLlegan mapa city
											   else trenesQueLlegan mapa city

esCiudad :: Ciudad -> Ciudad -> Bool
esCiudad (x:xs) [] = False
esCiudad [] (y:ys) = False
esCiudad [] [] = True
esCiudad (x:xs) (y:ys) = esIgual x y && esCiudad xs ys

esIgual :: Char -> Char -> Bool
esIgual x y = x == y
-- AL PEDO PORQUE STRINGS SI PUEDEN COMPARARSE POR IGUALDAD ._.XD

esTren :: Medio -> Bool
esTren Tren = True
esTren _ = False



--Combina la informacion de todos los mapas dados en un único mapa que contiene
--la informacion de todos los transportes. Suponer que en la lista de mapas no hay
--dos transportes con el mismo codigo.
combinarMapas :: [Mapa] -> Mapa
combinarMapas [] = Vacio
combinarMapas (m:ms) = juntarMapas m (combinarMapas ms)

juntarMapas :: Mapa -> Mapa -> Mapa
juntarMapas Vacio mapa2 = mapa2
juntarMapas (AddT c m c1 c2 n mapa) mapa2 = ( AddT c m c1 c2 n 
										     (juntarMapas mapa mapa2) )





















