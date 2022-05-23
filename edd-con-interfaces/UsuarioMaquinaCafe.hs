import MaquinaCafe

cuantosCortados :: MaquinaCafe -> Int 
cuantosCortados maquina = 
			    if disponibleMC maquina CafeCortado
			    then 1 + cuantosCortados (pedirCafeMC maquina CafeCortado)
			    else 0

totalRecaudacion :: [MaquinaCafe] -> Int 
totalRecaudacion [] = 0
totalRecaudacion (m:ms) = recaudacionMC m + totalRecaudacion ms
