data Color = Azul | Rojo deriving Show
data Celda = ConsCelda [Color] deriving Show


celdaVacia :: Celda 
celdaVacia = ConsCelda []


nroBolitas :: Color -> Celda -> Int
nroBolitas cr (ConsCelda []) = 0
nroBolitas cr (ConsCelda (c:cs)) = if esDeColor cr c 
								   then 1 + nroBolitas cr (ConsCelda cs)
								   else nroBolitas cr (ConsCelda cs)
-- nroBolitas Rojo ( consCelda [Rojo, Rojo, Azul, Azul, Rojo] ) =
-- if esDeColor Rojo Rojo 
-- then 1 + nroBolitas Rojo ( consCelda [Rojo, Azul, Azul, Rojo] )

esDeColor :: Color -> Color -> Bool
esDeColor Rojo Rojo = True
esDeColor Azul Azul = True
esDeColor _ _ = False 


poner :: Color -> Celda -> Celda
poner cr (ConsCelda cs) = (ConsCelda (cs ++ [cr]))

-- poner Rojo (consCelda [Rojo, Azul]) = (consCelda ([Rojo, Azul] ++ [Rojo]))


sacar :: Color -> Celda -> Celda
sacar cr (ConsCelda cs) = ConsCelda(quitar1 cr cs)

-- sacar Rojo (ConsCelda[Azul, Rojo, Azul, Rojo]) = ConsCelda [Azul, Azul, Rojo]
-- if esDeColor Rojo Azul
-- then consCelda( Azul : ( sacar Rojo( consCelda [Rojo, Azul, Rojo] ) ) )
-- if esDeColor Rojo Rojo
-- then consCelda( Azul : [Azul, Rojo]) 

quitar1 :: Color -> [Color] -> [Color]
quitar1 cc [] = []
quitar1 cc (x:xs) = if esDeColor cc x 
					then xs
					else x : quitar1 cc xs 


ponerN :: Int -> Color -> Celda -> Celda 
ponerN 0 c (ConsCelda (cs)) = (ConsCelda (cs))
ponerN n c (ConsCelda (cs)) = (ConsCelda (ponerN' n c (cs)))

-- ponerN 2 Rojo (ConsCelda [Azul, Azul]) 
-- ConsCelda ( ponerN' 2 Rojo [Azul, Azul] )
-- ConsCelda ( Azul : Rojo : ponerN' 1 Rojo [Azul] )
-- ConsCelda ( Azul : Rojo : Azul : Rojo : ponerN' 0 Rojo [])
-- ConsCelda ( Azul : Rojo : Azul : Rojo : [])

-- ponerN 1 Azul (ConsCelda []) = (ConsCelda( ponerN' 1 Azul [] ))
-- (ConsCelda( [] ))

ponerN' :: Int -> Color -> [Color] -> [Color]
ponerN' 0 cc [] = []
ponerN' 0 cc (x:xs) = (x:xs)
ponerN' n cc [] = cc : ponerN' (n-1) cc []
ponerN' n cc (x:xs) = x : cc : ponerN' (n-1) cc xs

































