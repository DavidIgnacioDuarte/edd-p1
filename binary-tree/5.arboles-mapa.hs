data Dir = Izq | Der deriving Show

data Objeto = Tesoro | Chatarra deriving (Eq, Show) -- para que tengan igualdad

data Cofre = C [Objeto] deriving Show

data Mapa = Fin Cofre
		  | M Cofre Mapa Mapa deriving Show


mapa1 :: Mapa
mapa1 = M (C [Tesoro, Chatarra])
			(Fin (C []))
			(Fin (C []))

mapa2 :: Mapa
mapa2 = M (C [])
			(M (C [Chatarra, Chatarra]) (Fin (C [Chatarra])) (Fin (C [Tesoro]))) 
			(M (C [Tesoro, Tesoro]) (Fin (C [Tesoro])) (Fin (C [Chatarra])))

mapa3 :: Mapa
mapa3 =  M (C [Chatarra])
				(M (C [Chatarra, Chatarra])
					 (Fin (C[])) 
					 (M (C[]) 
					 	(Fin (C [Chatarra]))
					    (Fin (C [Chatarra, Chatarra]))))
				(M (C [Tesoro, Tesoro])
					(M (C [])
						(Fin (C [Tesoro]))
						(Fin (C [])))
					(Fin (C [Tesoro])))



hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = hayTesoroEnCofre c 
hayTesoro (M c m1 m2) = (hayTesoroEnCofre c) || 
						(hayTesoro m1) ||
						(hayTesoro m2)

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (C obj) = elem Tesoro obj -- "elem" es "pertenece x y" 
						 -- donde "x" es el tesoro, e "y" la lista de objetos. 



hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn (d:ds) (Fin c) = error "el numero de direcciones supera a la altura del arbol"
hayTesoroEn [] mapa = hayTesoroEnElPunto mapa
hayTesoroEn (d:ds) mapa = hayTesoroEn ds (avanzar d mapa)

avanzar :: Dir -> Mapa -> Mapa
avanzar Izq (M _ m1 _) = m1
avanzar Der (M _ _ m2) = m2

hayTesoroEnElPunto :: Mapa -> Bool
hayTesoroEnElPunto (Fin c) = hayTesoroEnCofre c 
hayTesoroEnElPunto (M c _ _) = hayTesoroEnCofre c



caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin c) = []
caminoAlTesoro (M c m1 m2) = if hayTesoroEnCofre c 
							 then [] 
							 else (direccionDelTesoro m1 m2) :
							  	  caminoAlTesoro (direccionDelTesoro m1 m2)

direccionDelTesoro :: Mapa -> Mapa -> Dir 
direccionDelTesoro m1 m2 = if (hayTesoro m1) 
						   then Izq 
						   else Der 



















