data Pizza = Prepizza
		   | P Ingrediente Pizza 
		   	 deriving Show

data Ingrediente = Salsa
				 | Queso
				 | Jamon
				 | Aceitunas Int -- cantidad de aceitunas
				   deriving Show

soloQueso :: Pizza
soloQueso = P Queso Prepizza

jamonYQueso :: Pizza
jamonYQueso = P Jamon soloQueso

salsaJamonYQueso :: Pizza
salsaJamonYQueso = P Salsa jamonYQueso
--ó
salsaJamonYQueso' :: Pizza
salsaJamonYQueso' = P Salsa ( P Jamon (P Queso Prepizza))



capas :: Pizza -> [Ingrediente]
capas Prepizza = []
capas (P ing pizza) = ing : capas (pizza)



tieneJamon :: Pizza -> Bool
tieneJamon Prepizza = False
tieneJamon (P ing pizza) = esJamon ing || tieneJamon pizza

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False



sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (P ing pizza) = if esJamon ing
						   then sacarJamon pizza
						   else (P ing(sacarJamon pizza))



armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (x:xs) = (P x(armarPizza xs))



duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (P ing pizza) = if sonAceitunas ing
								  then (P (duplicarAceituna ing) (duplicarAceitunas pizza) )
								  else (P ing (duplicarAceitunas pizza))

duplicarAceituna :: Ingrediente -> Ingrediente
duplicarAceituna (Aceitunas n) = Aceitunas(n*2)

sonAceitunas :: Ingrediente -> Bool
sonAceitunas (Aceitunas _) = True
sonAceitunas _ = False
-- duplicarAceitunas (P Jamon(P Queso(P (Aceitunas 3) Prepizza))) = NO OLVIDAR PARÉNTESIS AL TESTEAR
-- P Jamon(P Queso(P (Aceitunas 6) Prepizza))



cantIngredientesPorPizza :: [Pizza] -> [(Int, Pizza)]
cantIngredientesPorPizza [] = []
cantIngredientesPorPizza (p:ps) = agruparPar p p : cantIngredientesPorPizza ps

agruparPar :: Pizza -> Pizza -> (Int, Pizza)
agruparPar pInt pPizza = (cantIngredientes pInt,pPizza)

cantIngredientes :: Pizza -> Int
cantIngredientes Prepizza = 0
cantIngredientes (P ing pizza) = 1 + (cantIngredientes pizza)
-- cantIngredientesPorPizza [ (P Salsa(P Queso Prepizza)), (P Jamon(P (Aceitunas 30)(P Queso Prepizza))), (Prepizza) ] =
-- [(2,P Salsa (P Queso Prepizza)),(3,P Jamon (P (Aceitunas 30) (P Queso Prepizza))),(0,Prepizza)]

























