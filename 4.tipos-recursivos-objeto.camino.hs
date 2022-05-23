data Objeto = Cacharro 
			| Tesoro
			deriving Show

data Camino = Fin 
			| Cofre [Objeto] Camino 
			| Nada Camino
			deriving Show

hayTesoro :: Camino -> Bool
hayTesoro (Fin) = False
hayTesoro (Nada c) = hayTesoro c
hayTesoro (Cofre obj c) = hayTesoroEn' obj || hayTesoro c

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

hayTesoroEn' :: [Objeto] -> Bool
hayTesoroEn' [] = False
hayTesoroEn' (o:os) = esTesoro o || hayTesoroEn' os
-- hayTesoro (Cofre [Cacharro, Cacharro](Nada(Cofre [Cacharro, Tesoro] Fin))) = True



-- PRECONDICIÓN: Hay al menos 1 tesoro, sino devuelve los pasos hasta el final.
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro (Fin) = 0
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre obj c) = if hayTesoroEn' obj
								 then 0
								 else 1 + pasosHastaTesoro c
-- pasosHastaTesoro (Cofre [Cacharro, Cacharro](Nada(Cofre [Cacharro, Tesoro] Fin))) = 2



hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n (Fin) = False
hayTesoroEn n (Nada c) = ( pasosHastaTesoro (Nada c) ) == n
hayTesoroEn n (Cofre obj c) = ( pasosHastaTesoro (Cofre obj c) ) == n
-- hayTesoroEn 2 (Cofre [Cacharro, Cacharro](Nada(Cofre [Cacharro, Tesoro] Fin))) = True



alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n (Fin) = n == 0
alMenosNTesoros n (Nada c) = (tesorosTotales (Nada c)) >= n
alMenosNTesoros n (Cofre obj c) = (tesorosTotales (Cofre obj c)) >= n

tesorosTotales :: Camino -> Int
tesorosTotales (Fin) = 0
tesorosTotales (Nada c) = tesorosTotales c 
tesorosTotales (Cofre obj c) = tesorosEnCofre obj + tesorosTotales c

tesorosEnCofre :: [Objeto] -> Int
tesorosEnCofre [] = 0
tesorosEnCofre (o:os) = if esTesoro o 
						then 1 + tesorosEnCofre os
						else tesorosEnCofre os
-- alMenosNTesoros 1 (Cofre [Cacharro, Cacharro](Nada(Cofre [Cacharro, Tesoro] Fin))) = True
-- alMenosNTesoros 2 (Cofre [Cacharro, Cacharro](Nada(Cofre [Cacharro, Tesoro] Fin))) = False



cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre n m (Fin) = 0
cantTesorosEntre n m (Nada c) = if n > 1
								then cantTesorosEntre (n-1) (m-1) c 
								else tesorosHasta m c 
cantTesorosEntre n m (Cofre obj c) = if n > 1
									 then cantTesorosEntre (n-1) (m-1) c
									 else tesorosHasta m c

tesorosHasta :: Int -> Camino -> Int
tesorosHasta m (Fin) = 0
tesorosHasta m (Nada c) = if m > 1
						  then tesorosHasta (m-1) c
						  else 0
tesorosHasta m (Cofre obj c) = if m > 1
							   then tesorosEnCofre obj + tesorosHasta (m-1) c 
							   else tesorosEnCofre obj
--      					0 pasos 				1 paso(1t) 			  2p(0t)        3p(2t)					4p(3t)					5p(0t)
-- cantTesorosEntre 1 3 (Cofre [Tesoro, Cacharro] (Cofre [Cacharro, Tesoro] (Nada (Cofre [Tesoro, Tesoro] (Cofre [Tesoro, Tesoro, Tesoro] Fin))))) = 3
-- RESOLUCION
-- cantTesoroEntre 1 3 (CAMINO) = tesorosHasta 3 (CAMINO RESTANTE)

-- CAMINO QUE RESTÓ : (Cofre [Cacharro, Tesoro] (Nada (Cofre [Tesoro, Tesoro] (Cofre [Tesoro, Tesoro, Tesoro] Fin))))

-- tesorosHasta 3 (CAMINO) = 1 + tesorosHasta 2 (CAMINO RESTANTE)
-- tesorosHasta 2 (CAMINO) = 0 + tesorosHasta 1 (CAMINO RESTANTE)
-- tesorosHasta 1 (CAMINO) = 2











