data SecuenciaDeCeldas = S [Celda] Celda [Celda] deriving Show

data Celda = C [Color] deriving Show

data Dir = Izq | Der deriving Show

data Color = Azul | Rojo deriving Show


{-
PROPÓSITO: Dada una secuencia de celdas, denota la lista de celdas que dicha secuencia
representa, respetando el orden de la secuencia.
-}
celdas :: SecuenciaDeCeldas -> [Celda]
celdas ( S c1 c2 c3 ) = (snoc c1 c2) ++ c3


{-
PROPÓSITO: Dada una secuencia de celdas, indica si el cabezal está ubicado en el
extremo izquierdo.
-}
enElOrigen :: SecuenciaDeCeldas -> Bool
enElOrigen ( S c1 c2 c3 ) = isEmpty c1


{-
PROPÓSITO: Dado un número n, y una lista de celdas cs, produce una secuencia que sigue 
el orden de la lista cs, y en la que la celda número n de la lista tendrá ubicado el cabezal.
PRECONDICIÓN: el número n es un índice válido de la lista y la lista cs no está vacía.
-}
nuevaSecuencia :: Int -> [Celda] -> SecuenciaDeCeldas
nuevaSecuencia n cs = ( S (celdasIzq n cs) (celdaConCabezal n cs) (celdasDer n cs) )
 -- nuevaSecuencia 2 [ (C [Rojo, Azul]), (C [Azul]), (C []) ] =
-- ( S (celdasIzq 2 [ (C [Rojo, Azul]), (C [Azul]), (C []) ] ) )
-- celdasIzq 2 [Celda] = if 2 > 1
-- then (C [Rojo, Azul]) : celdasIzq 1 [ (C [Azul]), (C []) ]
-- ( C [Rojo, Azul] ) :
-- celdasIzq 1 [ (C [Azul]), (C []) ] = if 1 > 1 
-- else [] 
-- = [ ( C [Rojo, Azul] ) ]

-- celdaConCabezal 2 [ (C [Rojo, Azul]), (C [Azul]), (C []) ] =
-- if 2 > 1
-- then celdaConCabezal 1 [ (C [Azul]), (C []) ] 
-- if 1 > 1
-- else [ (C [Azul]) ]

-- celdasDer 2 [ (C [Rojo, Azul]), (C [Azul]), (C []) ] =
-- drop' 2 [ (C [Rojo, Azul]), (C [Azul]), (C []) ]
-- [ (C []) ]
celdasIzq :: Int -> [Celda] -> [Celda]
celdasIzq n [] = [] -- CASO BORDE
celdasIzq n (c:cs) = if n > 1 
				  	 then c : celdasIzq (n-1) cs
				 	 else []

celdaConCabezal :: Int -> [Celda] -> Celda
celdaConCabezal n [] = (C []) -- CASO BORDE
celdaConCabezal n (c:cs) = if n > 1
						   then celdaConCabezal (n-1) cs
						   else c 

celdasDer :: Int -> [Celda] -> [Celda]
celdasDer n [] = [] -- CASO BORDE
celdasDer n (c:cs) = drop' n (c:cs) 


{-
PROPÓSITO: Dada una dirección y una secuencia de celdas, avanza el cabezal hacia la celda
lindante en la dirección dada. Operación total.
-}
mover :: Dir -> SecuenciaDeCeldas -> SecuenciaDeCeldas
mover d sc = if esDireccionDer d 
			 then nuevaSecuencia ((posicionDelCabezal (sc)) + 1) ( celdas (sc) )
			 else nuevaSecuencia ((posicionDelCabezal (sc)) - 1) ( celdas (sc) )

esDireccionDer :: Dir -> Bool
esDireccionDer Der = True
esDireccionDer Izq = False

posicionDelCabezal :: SecuenciaDeCeldas -> Int
posicionDelCabezal ( S c1 c2 c3 ) = longitud c1 + 1
-- posicionDelCabezal (S [C [Rojo,Azul],C [Azul]] (C []) [C [],C [Azul,Rojo],C [Rojo,Rojo]])
-- longitud [C [Rojo,Azul],C [Azul]] = 2 + 1 (próximamente está el cabezal)


{-
PROPÓSITO: Dado un número n, una dirección y una secuencia de celdas, mueve el cabezal n 
veces hacia dicha dirección hasta donde pueda. Operación total.
-}
moverN :: Int -> Dir -> SecuenciaDeCeldas -> SecuenciaDeCeldas
moverN n d sc = if esDireccionDer d 
				then nuevaSecuencia ((posicionDelCabezal (sc)) + n) ( celdas (sc) )
				else nuevaSecuencia ((posicionDelCabezal (sc)) - n) ( celdas (sc) )


{-
PROPÓSITO: Dada una secuencia de celdas, ubica el cabezal en el extremo izquierdo.
-}
irAlOrigen :: SecuenciaDeCeldas -> SecuenciaDeCeldas
irAlOrigen (S c1 c2 c3) = nuevaSecuencia ((posicionDelCabezal (S c1 c2 c3)) - longitud c1) ( celdas (S c1 c2 c3) )


{-
PROPÓSITO: Dada una secuencia de celdas, indica la cantidad de bolitas rojas y azules.
-}
totalDeBolitas :: SecuenciaDeCeldas -> Int
totalDeBolitas sc = nroBolitas' ( celdas (sc) )


nroBolitas' :: [Celda] -> Int
nroBolitas' [] = 0
nroBolitas' (x:xs) = nroBolitasDeCelda x + nroBolitas' xs


nroBolitasDeCelda :: Celda -> Int
nroBolitasDeCelda (C []) = 0
nroBolitasDeCelda (C (c:cs)) = 1 + longitud cs 


{-
PROPÓSITO: Dada una secuencia de celdas, vacía las celdas de la misma, manteniendo el 
cabezal en la posición actual.
-}
vaciar :: SecuenciaDeCeldas -> SecuenciaDeCeldas
vaciar ( S c1 c2 c3 ) = ( S ( vaciar' c1 ) ( C [] ) ( vaciar' c3 ) )

vaciar' :: [Celda] -> [Celda]
vaciar' [] = []
vaciar' (c:cs) = vaciarCelda c : vaciar' cs

vaciarCelda :: Celda -> Celda
vaciarCelda (C cs) = (C [])



-- FUNCIONES AUXILIARES ---------------------------------------------------------------
snoc :: [a] -> a -> [a]
snoc [] y = [y]
snoc (x:xs) y = x : snoc xs y

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty x = False

drop' :: Int -> [Celda] -> [Celda]
drop' n [] = [] -- CASO BORDE
drop' n (c:cs) = if n > 1
				 then drop' (n-1) cs
				 else cs

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

