module Celda2(
	  Celda,
	  celdaVacia,
	  poner,
	  nroBolitas,
	  sacar,
	  hayBolitas
	)
where

import Color

data Celda = C [Color] deriving Show

-- Inv. de Rep.:
-- No tiene invariantes

-- Eficiencia: O(1)
celdaVacia :: Celda
celdaVacia = C []

-- Eficiencia: O(1)
poner :: Color -> Celda -> Celda
poner col (C cols) = C (col:cols)

-- Eficiencia: O(n), siendo n la cantidad
-- de elementos que tiene la lista
nroBolitas :: Color -> Celda -> Int
nroBolitas col (C cols) = apariciones col cols

-- Eficiencia: O(n), siendo n la cantidad
-- de elementos que tiene la lista
apariciones :: Color -> [Color] -> Int
apariciones col [] = 0
apariciones col (c:cs) = 
	unoSi (col == c) + (apariciones col cs)

-- Eficiencia: O(n), siendo n la cantidad
-- de elementos que tiene la lista
hayBolitas :: Color -> Celda -> Bool
hayBolitas col cel = nroBolitas col cel > 0

unoSi True  = 1
unoSi False = 0

-- Prec.: tiene que haber bolitas de ese
-- color
-- Eficiencia: O(n), siendo n la cantidad
-- de elementos que tiene la lista
sacar :: Color -> Celda -> Celda
sacar col (C cols) =
	if apariciones col cols > 0
	   then C (quitarUna col cols)
	   else error "no hay bolitas de ese color"

-- Eficiencia: O(n), siendo n la cantidad
-- de elementos que tiene la lista
quitarUna :: Color -> [Color] -> [Color]
quitarUna col [] = error "no hay apariciones del elemento"
quitarUna col (c:cs) = 
	if col == c
	   then cs
	   else c : quitarUna col cs

