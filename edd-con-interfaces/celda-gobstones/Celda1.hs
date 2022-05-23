module Celda1(
	  Celda,
	  celdaVacia,
	  poner,
	  nroBolitas,
	  sacar,
	  hayBolitas
	)
where

import Color

-- Representación debe cumplir que
-- si uso el constructor C
-- todos sus parametros deben ser >= 0

-- Invariantes de Representación:
-- Sea C n1 n2 n3 n4
-- 1) se cumple n1, n2, n3, n4 >= 0

--             azules negras rojas verdes
data Celda = C Int Int Int Int deriving Show

-- Eficiencia: O(1)
celdaVacia :: Celda
celdaVacia = C 0 0 0 0

-- Eficiencia: O(1)
poner :: Color -> Celda -> Celda
poner Azul (C n1 n2 n3 n4) = C (n1+1) n2 n3 n4
poner Negro (C n1 n2 n3 n4) = C n1 (n2+1) n3 n4
poner Rojo (C n1 n2 n3 n4) = C n1 n2 (n3+1) n4
poner Verde (C n1 n2 n3 n4) = C n1 n2 n3 (n4+1)

-- Si crece el parámetro
-- la cantidad de operaciones
-- permanece constante

-- Eficiencia: O(1)
nroBolitas :: Color -> Celda -> Int
nroBolitas Azul (C n1 n2 n3 n4) = n1
nroBolitas Negro (C n1 n2 n3 n4) = n2
nroBolitas Rojo (C n1 n2 n3 n4) = n3
nroBolitas Verde (C n1 n2 n3 n4) = n4

-- Prec.: tiene que haber bolitas de 
-- ese color
-- Eficiencia: O(1)
sacar :: Color -> Celda -> Celda
sacar col cel =
	if nroBolitas col cel > 0
	   then restarC col cel
	   else error "no hay bolitas de ese color"

-- funcion reservada
-- Eficiencia: O(1)
restarC :: Color -> Celda -> Celda
restarC Azul (C n1 n2 n3 n4) = 
	C (n1-1) n2 n3 n4
restarC Negro (C n1 n2 n3 n4) = 
	C n1 (n2-1) n3 n4
restarC Rojo (C n1 n2 n3 n4) = 
	C n1 n2 (n3-1) n4
restarC Verde (C n1 n2 n3 n4) = 
	C n1 n2 n3 (n4-1)

-- Eficiencia: O(1)
hayBolitas :: Color -> Celda -> Bool
hayBolitas col cel = nroBolitas col cel > 0
