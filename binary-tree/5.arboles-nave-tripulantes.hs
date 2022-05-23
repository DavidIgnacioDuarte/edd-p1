--MODELO DE NAVE-----------------------------------------------------------------------------
data Componente = LanzaTorpedos 
			    | Motor Int 
			    | Almacen [Barril] deriving Show

data Barril = Comida 
			| Oxigeno
		    | Torpedo 
		    | Combustible deriving Show

data Sector = S SectorId [Componente] [Tripulante] deriving Show

type SectorId = String

type Tripulante = String

data Tree a = EmptyT 
			| NodeT a (Tree a) (Tree a) deriving Show

data Nave = N (Tree Sector) deriving Show

---------------------------------------------------------------------------------------------
--EJEMPLO------------------------------------------------------------------------------------
nave1 :: Nave 
nave1 = N ( NodeT (S "naveSector1" [LanzaTorpedos, (Motor 20)] ["t1(1)", "t1(2)"])
				(NodeT (S "sector2" [(Almacen [Oxigeno]), (Almacen [Torpedo])] ["t2"])
					   	(EmptyT)
					    (NodeT (S "sector33" [(Motor 10)] ["t33", "Stark"]) 
					    	   (EmptyT)
					    	   (EmptyT)))
				(NodeT (S "sector22" [(Almacen [Comida, Comida])] ["t22(1)", "t22(2)", "t22(3)"])
							(EmptyT)
							(NodeT (S "sector3333" [(Almacen [Combustible]), (Motor 30)] [])
								(NodeT (S "sector4" [LanzaTorpedos] ["t4", "Stark"]) 
									(EmptyT)
									(EmptyT) )
								(EmptyT))) )
--------------------------------------------------------------------------------------------

--FUNCIONES---------------------------------------------------------------------------------

{-  
  PROPOSITO: Devuelve todos los sectores de la nave
-}
sectores :: Nave -> [SectorId]
sectores (N (t)) = (sectores' t) 

sectores' :: Tree Sector -> [SectorId]
sectores' EmptyT = [] 
sectores' (NodeT s t1 t2) = [sectorId s] ++ (sectores' t1) ++ (sectores' t2)

sectorId :: Sector -> SectorId
sectorId (S identificacion componentes tripulantes) = identificacion
------------------------------------------------------------------------------------------


{-
  PROPOSITO: Devuelve la suma de poder de propulsión de todos los motores de la nave.
-}
poderDePropulsion :: Nave -> Int 
poderDePropulsion (N (t)) = (sumaDePropulsionesEn t)

sumaDePropulsionesEn :: Tree Sector -> Int
sumaDePropulsionesEn EmptyT = 0
sumaDePropulsionesEn (NodeT s t1 t2) = (propulsionTotalSiHayMotorEn s) + 
										(sumaDePropulsionesEn t1) +
										(sumaDePropulsionesEn t2)

propulsionTotalSiHayMotorEn :: Sector -> Int
propulsionTotalSiHayMotorEn (S identificacion componentes tripulantes) =
							(propulsionEnComponentes componentes)

propulsionEnComponentes :: [Componente] -> Int 
propulsionEnComponentes [] = 0
propulsionEnComponentes (c:cs) = if esMotor c 
								 then propulsionDeMotor c + propulsionEnComponentes cs 
								 else propulsionEnComponentes cs 

esMotor :: Componente -> Bool
esMotor (Motor _) = True 
esMotor _ = False

propulsionDeMotor :: Componente -> Int 
propulsionDeMotor (Motor n) = n 
------------------------------------------------------------------------------------------


{-
  PROPOSITO: Devuelve todos los barriles de la nave.
-}
barriles :: Nave -> [Barril]
barriles (N (t)) = (barriles' t) 

barriles' :: Tree Sector -> [Barril]
barriles' EmptyT = []
barriles' (NodeT s t1 t2) = (elementosDelBarrilSiHay s) ++ (barriles' t1) ++ (barriles' t2) 

tieneBarril :: Sector -> Bool
tieneBarril (S ide comp trip) = (hayBarrilEnComponentes comp) 

hayBarrilEnComponentes :: [Componente] -> Bool 
hayBarrilEnComponentes [] = False 
hayBarrilEnComponentes (c:cs) = (tieneBarril' c) || (hayBarrilEnComponentes cs)

tieneBarril' :: Componente -> Bool 
tieneBarril' (Almacen _) = True 
tieneBarril' _ = False 

elementosDelBarrilSiHay :: Sector -> [Barril]
elementosDelBarrilSiHay (S ide comp trip) = if (hayBarrilEnComponentes comp)
									   		then elementosDelBarril comp
									   		else []

elementosDelBarril :: [Componente] -> [Barril]
elementosDelBarril [] = []
elementosDelBarril (c:cs) = if (tieneBarril' c)
							then (barril c) ++ (elementosDelBarril cs) 
							else (elementosDelBarril cs)

-- Ya se asume que dicho componente es (Almacen [Barril])
barril :: Componente -> [Barril]
barril (Almacen n) = n
------------------------------------------------------------------------------------------


{-
  PROPOSITO: Añade una lista de componentes a un sector de la nave
-}
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector [] ide (N(t)) = (N(t))
agregarASector componentes ide (N(t)) = (N (agregarASectorSiExiste componentes ide t))

agregarASectorSiExiste :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorSiExiste [] ide tree = tree
agregarASectorSiExiste comp ide EmptyT = EmptyT
agregarASectorSiExiste comp ide (NodeT s t1 t2) = if (esSector ide s)
												  then (NodeT (agregarComponentes comp s) 
												  	   (agregarASectorSiExiste comp ide t1) 
												  	   (agregarASectorSiExiste comp ide t2))
												  else (NodeT s 
												  	   (agregarASectorSiExiste comp ide t1) 
												  	   (agregarASectorSiExiste comp ide t2))

esSector :: SectorId -> Sector -> Bool 
esSector ide (S ide1 comp trip) = ide == ide1

agregarComponentes :: [Componente] -> Sector -> Sector 
agregarComponentes cs (S ide cs1 ts) = (S ide (cs1 ++ cs) ts)
------------------------------------------------------------------------------------------


{-
  PROPOSITO: Incorpora un tripulante a una lista de sectores de la nave
  PRECONDICIONES: Todos los id de la lista existen en la nave
-}
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA trip ids (N(t)) = (N(asignarTripulante trip ids t))

asignarTripulante :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulante trip [] t = t
asignarTripulante trip ids EmptyT = EmptyT 
asignarTripulante trip ids (NodeT s t1 t2) = if (perteneceAlSector ids s)
											 then (NodeT (asignarASector trip s) 
												  (asignarTripulante trip ids t1)
												  (asignarTripulante trip ids t2)) 
											 else (NodeT s 
											 	  (asignarTripulante trip ids t1)
											 	  (asignarTripulante trip ids t2))

perteneceAlSector :: [SectorId] -> Sector -> Bool 
perteneceAlSector [] s = False 
perteneceAlSector (i:is) (S ide cs ts) = (elem ide (i:is))

asignarASector :: Tripulante -> Sector -> Sector 
asignarASector trip (S ide cs ts) = (S ide cs (trip : ts))
------------------------------------------------------------------------------------------



{-
  PROPOSITO: Devuelve los sectores en donde aparece un tripulante dado.
-}
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados trip (N(t)) = (sectoresAsignados' trip t)

sectoresAsignados' :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignados' trip EmptyT = []
sectoresAsignados' trip (NodeT s t1 t2) = if (tripulanteEsDe trip s)
										  then sectorId s :
										  	   sectoresAsignados' trip t1 ++
										  	   sectoresAsignados' trip t2 
										  else sectoresAsignados' trip t1 ++
										  	   sectoresAsignados' trip t2

tripulanteEsDe :: Tripulante -> Sector -> Bool 
tripulanteEsDe trip (S ide cs ts) = (elem trip ts)
------------------------------------------------------------------------------------------


{-
  PROPOSITO: Devuelve la lista de tripulantes, sin elementos repetidos.
-}
tripulantes :: Nave -> [Tripulante]
tripulantes (N(t)) = (sinRepetidos (tripulantesTotales t))

tripulantesTotales :: Tree Sector -> [Tripulante]
tripulantesTotales EmptyT = []
tripulantesTotales (NodeT s t1 t2) = tripulantesDeSector s ++ 
									 tripulantesTotales t1 ++	
									 tripulantesTotales t2

tripulantesDeSector :: Sector -> [Tripulante]
tripulantesDeSector (S ide cs ts) = ts
------------------------------------------------------------------------------------------


--FUNCION AUXILIAR-------------------------------------------------------------------------
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if (elem x xs)
					  then sinRepetidos xs
					  else x : sinRepetidos xs













