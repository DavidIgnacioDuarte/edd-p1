module MaquinaCafe where

-- Interfaz de la máquina de café y su implementación

data TipoCafe = CafeSolo --tipo de datos algebráico enumerativo
			  | CafeDulce
			  | CafeCortado
			  -- sin deriving Show para que el usuario no pueda ver su implementación

data MaquinaCafe = MC Int --cantidad de agua (cc)
					  Int --cantidad de leche (cc)
					  Int --cantidad de cafe (g)
					  Int --cantidad de azucar (g)
					  Int --recaudacion ($)
					  -- sin deriving Show para que el usuario no pueda ver su implementación

nuevaMC :: MaquinaCafe
nuevaMC = MC 0 0 0 0 0

disponibleMC :: MaquinaCafe -> TipoCafe -> Bool
disponibleMC (MC agua leche cafe azucar _) tc = agua >= aguaRequerida tc &&
											 	leche >= lecheRequerida tc &&
												cafe >= cafeRequerido tc &&
												azucar >= azucarRequerido tc 

aguaRequerida :: TipoCafe -> Int 
aguaRequerida CafeSolo = 200
aguaRequerida CafeDulce = 200
aguaRequerida CafeCortado = 150

lecheRequerida :: TipoCafe -> Int 
lecheRequerida CafeSolo = 0
lecheRequerida CafeDulce = 0
lecheRequerida CafeCortado = 50

cafeRequerido :: TipoCafe -> Int 
cafeRequerido CafeSolo = 40
cafeRequerido CafeDulce = 40
cafeRequerido CafeCortado = 30

azucarRequerido :: TipoCafe -> Int 
azucarRequerido CafeSolo = 0
azucarRequerido CafeDulce = 12
azucarRequerido CafeCortado = 12


--PRECONDICION: El tipo de café debe estar disponible.
pedirCafeMC :: MaquinaCafe -> TipoCafe -> MaquinaCafe
pedirCafeMC (MC agua leche cafe azucar recaudacion) tc = MC  (agua - aguaRequerida tc) 
															 (leche - lecheRequerida tc)
															 (cafe - cafeRequerido tc)
															 (azucar - azucarRequerido tc)
															 (recaudacion + precioCafe)

precioCafe :: Int
precioCafe = 100

mantenerMC :: MaquinaCafe -> MaquinaCafe
mantenerMC _ = MC 20000 2000 1000 1000 0 --Se crea una "nueva" máquina, por ende el parámetro
										 --no importa ( y queda como "_" )

recaudacionMC :: MaquinaCafe -> Int 
recaudacionMC (MC _ _ _ _ recaudacion) = recaudacion 
						--Explícito sólo el parámetro que me interesa
            
