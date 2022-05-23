import Celda2
import Color

-- Eficiencia con Celda1: lineal
ponerN :: Int -> Color -> Celda -> Celda
ponerN n col cel = 
	if n < 0
	   then error "no puedo poner una cantidad negativa de bolitas"
	   else ponerNPositivo n col cel

-- Eficiencia con Celda1: lineal
-- recorre el número, entonces a medida
-- que crece se incrementa linealmente
-- la cantidad de operaciones y poner es constante

-- Eficiencia con Celda2: lineal
-- recorre el número, entonces a medida
-- que crece se incrementa linealmente
-- la cantidad de operaciones y poner es constante
ponerNPositivo 0 col cel = cel
ponerNPositivo n col cel =
	poner col (ponerNPositivo (n-1) col cel)


-- Prec.: existe esa cantida de bolitas
-- de ese color
-- Eficiencia con Celda1: O(n)
sacarN :: Int -> Color -> Celda -> Celda
sacarN n col cel = 
	if n < 0
	   then error "no puedo poner una cantidad negativa de bolitas"
	   else sacarNPositivo n col cel

-- Eficiencia con Celda1: O(n)
-- recorre el número, entonces a medida
-- que crece se incrementa linealmente
-- la cantidad de operaciones Y sacar es constante

-- Eficiencia con Celda2:
-- O(n) * O(m) = O(n*m), una lineal
-- donde n, es el tamaño del número
-- y m es el tamaño de la lista
-- recorre el número Y sacar es lineal

-- dentro de otra lineal lo dejan
-- cuadrático O(n^2)
sacarNPositivo 0 col cel = cel
sacarNPositivo n col cel =
	sacar col (sacarNPositivo (n-1) col cel)
