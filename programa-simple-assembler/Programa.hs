data Instruccion = MOV Operando Operando 
				 | ADD Operando Operando 
				 | MUL Operando Operando

type Programa = [Instruccion]

data Operando = M Direccion
			  | I Int 

type Direccion = Int 

--INV REP:
-- Sea _ op1 op2,
-- se cumple que op1 siempre tiene que ser una direccion. 

--MODELO------------
prog :: Programa
prog = [ MOV (M 0) (I 10),
		 MUL (M 0) (I 4),
		 MOV (M 1) (I 2),
		 ADD (M 0) (M 1) ]

--MEMORIA = [42, 2, 0, 0, 0, 0, 0, 0, 0, ...]
--			  |  |  |  |  |  |  |
--			  0  1  2  3  4  5  6 ...

--INTERFAZ MAP
--emptyM  :: Map k v  [O(1)]
--insertM :: Eq k => k -> v -> Map k v -> Map k v    [O(log M)]
--lookupM :: Eq k => k => Map k v => Maybe v 
--removeM :: Eq k => k -> Map k v => Map k v 
--keysM   :: Map k v -> [k]    [O(M)]


--PREC:
-- Cada Direccion en las instrucciones del programa debe ser un número del tamaño de la memoria-1
--EFICIENCIA: O((n+M) * log M),
-- donde n son las instrucciones del programa y M las direcciones de memoria.
-- Entonces, si trabajara con la memoria en forma de lista, mirar el valor de una posición
-- o modificarlo, costaria O(M) (muy costoso).
-- En este caso, costaria O(n*M). 
-- SOLUCION: Representar a la memoria usando un Map, así las operaciones de mirar y
-- modificar el valor de una posición de memoria serían de eficiencia logarítmica.
--EFICIENCIA FINAL:
-- O(M log M) + O(n*log M) + O(M log M)
-- O(2M log M) == O(M + log M) + O(N * log M)
-- => O((n*M) log M)
ejecutar :: [Int] -> Programa -> [Int] --La lista representa a la memoria.
ejecutar memoria programa = 
		 mapToList (ejecutarInstrucciones (listToMap memoria) programa)


type Memoria = Map Direccion Int

--EFICIENCIA: O(M * log M), donde M son los elementos de la lista de valores, costo de 
--recorrerla, e insertarla en el map.
listToMap :: [Int] -> Memoria
listToMap valores = listToMap' valores 0

listToMap' :: [Int] -> Direccion -> Memoria 
listToMap' []     direccion = emptyM  
listToMap' (v:vs) direccion = insertM direccion v (listToMap' vs (direccion+1))

--EFICIENCIA: O(M * log M), donde M es el tamaño de la memoria. 
mapToList :: Memoria -> [Int]
mapToList memoria =  
		  mapToList' memoria 0 (length (keysM memoria))

mapToList' :: Memoria -> Direccion -> Int -> [Int]
mapToList' memoria desde hasta = 
		   if desde < hasta 
		   	then fromJust (lookupM desde memoria) : mapToList' memoria (desde+1) hasta
		   	else []



ejecutarInstrucciones :: Memoria -> Programa -> Memoria
ejecutarInstrucciones memoria []       = memoria
ejecutarInstrucciones memoria (i : is) = ejecutarInstrucciones (ejecutarI memoria i) is 

ejecutarI :: Memoria -> Instruccion -> Memoria 
ejecutarI memoria (MOV d y) = insertM d (valor y memoria)                   memoria 
ejecutarI memoria (MUL d y) = insertM d (valor d memoria * valor y memoria) memoria 
ejecutarI memoria (ADD d y) = insertM d (valor d memoria + valor y memoria) memoria

valor :: Operando -> Memoria -> Int 
valor (M d) memoria = fromJust (lookupM d memoria)
valor (I x) memoria = x




















