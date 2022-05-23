--DATOS-----------------------------------------------------------------
data Empresa = CE [Rol] -- ej = CE [( Developer Junior "Juego Sims" ), 
						--			( Management Senior "Aplicacion Bancaria" )]

type Proyecto = String

data Seniority = Junior 
			   | SemiSenior
			   | Senior

data Rol = Developer Seniority Proyecto -- ej= Developer Junior "Juego Sims"
		 | Management Seniority Proyecto -- ej= Management Senior "Aplicacion Bancaria"
------------------------------------------------------------------------

-- proyectos (CE [( Developer Junior "Juego Sims" ), ( Management Senior "Aplicacion Bancaria" ),
-- ( Developer SemisSenior "Juego Sims" )])

proyectos :: Empresa -> [Proyecto]
proyectos (CE ps) = proyectosSinRepetir ps

proyectosSinRepetir :: [Rol] -> [Proyecto]
proyectosSinRepetir [] = []
proyectosSinRepetir (p:[]) = proyecto p : []
proyectosSinRepetir (p:ps) = if pertenece (proyecto p) (proyectosTotales ps)
				 			 then proyectosSinRepetir ps  
				 			 else proyecto p : proyectosSinRepetir ps

proyecto :: Rol -> Proyecto
proyecto ( Developer s p ) = p
proyecto ( Management s p ) = p 

proyectosTotales :: [Rol] -> [Proyecto]
proyectosTotales [] = []
proyectosTotales (p:[]) = proyecto p : []
proyectosTotales (p:ps) = proyecto p : proyectosTotales ps




losDevSenior :: Empresa -> Int
losDevSenior (CE ps) = seniors ps 

seniors :: [Rol] -> Int 
seniors [] = 0
seniors (x:xs) = if esSenior x 
				 then 1 + seniors xs
				 else seniors xs

esSenior :: Rol -> Bool
esSenior ( Developer Senior _ ) = True
esSenior ( Management Senior _ ) = True
esSenior ( Developer _ _ ) = False
esSenior ( Management _ _ ) = False



losQueTrabajanEn :: Proyecto -> Empresa -> Int
losQueTrabajanEn p (CE []) = 0
losQueTrabajanEn p (CE (x:xs)) = if esProyecto p (proyecto x) 
								 then 1 + losQueTrabajanEn p (CE (xs))
								 else losQueTrabajanEn p (CE (xs))

esProyecto :: Proyecto -> Proyecto -> Bool
esProyecto (x:xs) [] = False
esProyecto [] (y:ys) = False
esProyecto [] [] = True
esProyecto (x:xs) (y:ys) = True && esIgual x y && esProyecto xs ys

esIgual :: Char -> Char -> Bool
esIgual x y = x == y



asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (CE (x:xs)) = agruparPorPares ( proyectosTotales (x:xs) ) (CE (x:xs))

-- Recursión sobre cada proyecto, siempre tomando como referencia a la empresa 
-- (que posee repetidos y nos ayudará a tomar el número de asignados por cada uno de ellos)
agruparPorPares :: [Proyecto] -> Empresa -> [(Proyecto, Int)]
agruparPorPares [] (CE (x:xs)) = []
agruparPorPares (p:ps) (CE (x:xs)) = if pertenece p ps 
									 then agruparPorPares ps (CE (x:xs))
									 else 
									 ( p, losQueTrabajanEn p (CE (x:xs)) ) : 
									 			agruparPorPares ps (CE (x:xs))


-- asignadosPorProyecto ( CE [(M S "Sims"), (D J "Sims"), ( D S "Tekken"), (M J "Sims")] ) =
-- [("Sims", 3), ("Tekken", 1)]



--FUNCIONES AUXILIARES----------------------------------------------------------------

pertenece :: Eq a => a -> [a] -> Bool
pertenece a [] = False
pertenece a (x:xs) = a == x || pertenece a xs

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = append (reversa xs) [x]

append :: [a] -> [a] -> [a]
append [] lista2 = lista2
append (x:xs) lista2 = x : append xs lista2