data Origen = Animal
		    | Vegetal 
		    | Mineral
		    deriving Show


type Nombre = String -- SINONIMO NO LLEVA "deriving Show"
data Ingrediente = Ing Nombre Origen Int deriving Show


data Fuego = Fuerte
		   | Moderado
		   | Suave
		   deriving Show

data MetodoCoccion = Horno Fuego Int
				   | Hervido
				   | Frito
				   deriving Show

data Receta = Empezar
			| Agregar  Int Ingrediente Receta
			| Cocinar  MetodoCoccion Receta
			deriving Show

--ALGUNOS INGREDIENTES---------------------------------------------------
bollo :: Ingrediente
bollo = Ing "Masa de pizza" Vegetal 228

tomate :: Ingrediente
tomate = Ing "Tomate" Vegetal 22

queso :: Ingrediente
queso = Ing "Queso" Animal 245


--FUNCIONES OBSERVADORAS-------------------------------------------------
nombreIng :: Ingrediente -> Nombre
nombreIng (Ing name origin energy) = name

energiaIng :: Ingrediente -> Int
energiaIng (Ing name origin energy) = energy

origenIng :: Ingrediente -> Origen
origenIng (Ing name origin energy) = origin

demoraMC :: MetodoCoccion -> Int
demoraMC (Horno fuego minutos) = minutos
demoraMC Hervido = 30
demoraMC Frito = 15

esHorno :: MetodoCoccion -> Bool
esHorno (Horno f m) = True
esHorno _ = False

esAnimal :: Origen -> Bool 
esAnimal Animal = True
esAnimal _ = False

esVegetal :: Origen -> Bool
esVegetal Vegetal = True
esVegetal _ = False

esMineral :: Origen -> Bool
esMineral Mineral = True
esMineral _ = False


--REPRESENTACION---------------------------------------------------------
recetaDePizza :: Receta
recetaDePizza = Cocinar(Horno Moderado 20)
				 (Agregar 100 queso
				  (Cocinar(Horno Fuerte 10)
				  	(Agregar 150 tomate
				  	 (Agregar 200 bollo Empezar))))

-------------------------------------------------------------------------
--FUNCIONES--------------------------------------------------------------

--Devuelve la cantidad de calorias aportantes de la receta.
kcalTotalesR :: Receta -> Int
kcalTotalesR Empezar = 0
kcalTotalesR (Agregar cantidad ingrediente receta) = calorias cantidad ingrediente + 
															  kcalTotalesR receta
kcalTotalesR (Cocinar metodo receta) = kcalTotalesR receta

calorias :: Int -> Ingrediente -> Int
calorias n i = div ( n * energiaIng i ) 100



--Devuelve el tiempo de coccion, sin tener en cuenta el tiempo en agregar.
tiempoCoccionR :: Receta -> Int
tiempoCoccionR Empezar = 0
tiempoCoccionR (Agregar c i r) = tiempoCoccionR r
tiempoCoccionR (Cocinar m r) = demoraMC m + tiempoCoccionR r


--Devuelve True si la receta no contiene ingredientes de origen animal.
aptaVeganosR :: Receta -> Bool
aptaVeganosR Empezar = True
aptaVeganosR (Agregar c i r) = not (esAnimal (origenIng i)) && aptaVeganosR r
aptaVeganosR (Cocinar m r) = aptaVeganosR r


--Devuelve True si la receta usa horno para su preparación.
usaHorno :: Receta -> Bool
usaHorno Empezar = False
usaHorno (Agregar c i r) = usaHorno r
usaHorno (Cocinar m r) = esHorno m || usaHorno r


--Multiplica los ingredientes de la receta por el factor indicado
escalarR :: Int -> Receta -> Receta
escalarR n Empezar = Empezar
escalarR n (Agregar c i r) = Agregar (c*n) i (escalarR n r)
escalarR n (Cocinar m r) = escalarR n r



















