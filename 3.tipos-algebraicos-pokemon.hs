-- Ejercicio 2 ---------------------------------------------------------------------------
data Pokemon = P TipoDePokemon Int
			   deriving Show

data Entrenador = E String [Pokemon] -- nombre y lista de cokemones
				  deriving Show

data TipoDePokemon = Fuego | Agua | Planta 
					 deriving Show


charmander :: Pokemon
charmander = P Fuego 100
bulbasaur :: Pokemon
bulbasaur = P Planta 80
squirtle :: Pokemon
squirtle = P Agua 70

tipo :: Pokemon -> TipoDePokemon
tipo (P t e) = t
energia :: Pokemon -> Int
energia (P t e) = e


ash :: Entrenador
ash = E "Ash" [charmander, bulbasaur]
red :: Entrenador
red = E "Red" [charmander, bulbasaur, squirtle]
misty :: Entrenador
misty = E "Misty" [squirtle]
jonas :: Entrenador
jonas = E "Jonas" []
david :: Entrenador
david = E "David" [squirtle, bulbasaur, charmander, charmander, charmander, squirtle]

nombre :: Entrenador -> String
nombre (E n px) = n
pokedex :: Entrenador -> [Pokemon]
pokedex (E n px) = px


esTipoSuperiorA :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoSuperiorA Fuego Planta = True
esTipoSuperiorA Fuego Fuego = False
esTipoSuperiorA Fuego Agua = False
esTipoSuperiorA Agua Fuego = True
esTipoSuperiorA Agua Agua = False
esTipoSuperiorA Agua Planta = False
esTipoSuperiorA Planta Agua = True
esTipoSuperiorA Planta Planta = False
esTipoSuperiorA Planta Fuego = False


-- agua > fuego - fuego > planta - planta > agua
superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = esTipoSuperiorA (tipo p1) (tipo p2)


cantPokemones :: Entrenador -> Int
cantPokemones (E n px) = length px 


cantPokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonesDe t (E n []) = 0
cantPokemonesDe t (E n (p:ps)) = if esDeIgualTipo t (tipo p)
								 then 1 + cantPokemonesDe t (E n ps)
								 else cantPokemonesDe t (E n ps)

-- Pattern matching SIN igualdad para un dato construido
esDeIgualTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDeIgualTipo Fuego Fuego = True
esDeIgualTipo Agua Agua = True
esDeIgualTipo Planta Planta = True
esDeIgualTipo _ _ = False


concatPokemon :: [Entrenador] -> [Pokemon]
concatPokemon [] = []
concatPokemon (e:es) = (pokedex e) ++ concatPokemon es
-- concatPokemon [ash, misty] = pokedex ash ++ [squirtle]
-- [charmander, bulbasaur] ++ [squirtle]


esMaestro :: Entrenador -> Bool
esMaestro e = cantPokemonesDe Fuego e >= 1 &&
			  cantPokemonesDe Agua e >= 1 &&
			  cantPokemonesDe Planta e >= 1








