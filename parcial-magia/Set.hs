module Set
(
Set,
emptyS,
addS,
belongs,
sizeS,
removeS,
unionS,
intersectionS,
setToList
)
where

--Un Set es un tipo abstracto de datos que consta de las siguientes operaciones:

data Set a = ElSet [a] Int
    deriving (Eq, Show)


-- 1.  Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda la cantidad de elementos en la estructura. 

-- Crea un conjunto vacío.
emptyS :: Set a 
emptyS = ElSet [] 0

-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
-- O(n) donde n es el largo de ys porque evalua el elem
addS :: Eq a => a -> Set a -> Set a
addS x (ElSet ys size) = if elem x ys
                         then ElSet ys size
                         else ElSet (x:ys) (size + 1)

-- ejemplo1 = addS 14 emptyS

-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.

--O(n) donde n es la lista, porque hace recursion
--estructural sobre la misma
--ademas por cada instancia de la recursion se evalua
--O(m) donde m es la cantidad de elementos que
--posee el conjunto
belongs :: Eq a => a -> Set a -> Bool
belongs x (ElSet ys size) = elem x ys

-- Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int
--O(n) donde n es el largo de la lista ys
--sizeS (ElSet ys size) = length ys
--O(1)
sizeS (ElSet ys size) = size

-- Borra un elemento del conjunto.
--FIJARSE INVARIANTE DE REPRESENTACION
removeS :: Eq a => a -> Set a -> Set a
removeS x (ElSet ys size) = (ElSet (sinUnaAparicionDe x ys) (length (sinUnaAparicionDe x ys)))

sinUnaAparicionDe :: Eq a => a -> [a] -> [a]
sinUnaAparicionDe x [] = []
sinUnaAparicionDe x (y:ys) =
    if x == y
       then ys
       else y : sinUnaAparicionDe x ys

--ejemplo2 = (addS 1 $ addS 2 $ addS 3 $ emptyS)

-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
-- unionS :: Eq a => Set a -> Set a -> Set a 
-- unionS (ElSet xs sizex) (ElSet ys sizey) = ElSet (sinRepetidos(xs++ys)) (length (sinRepetidos(xs++ys)))

-- sinRepetidos :: Eq a => [a] -> [a]
-- sinRepetidos [] = []
-- sinRepetidos (x:xs) = insertarSiNoPertenece x (sinRepetidos xs)

-- insertarSiNoPertenece :: Eq a => a -> [a] -> [a]
-- insertarSiNoPertenece x ys = if elem x ys
--                              then ys
--                              else x:ys

--solucion final
unionS :: Eq a => Set a -> Set a -> Set a
unionS (ElSet xs sizex) (ElSet ys sizey) = 
  let sinRep = sinRepetidos(xs++ys) in
    ElSet sinRep (length sinRep)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = 
  let resto = sinRepetidos xs in
    if elem x resto
      then resto
      else x : resto

 --(addS 1 $ addS 2 $ addS 5 $ emptyS) (addS 1 $ addS 2 $ addS 4 $ emptyS)

-- Dados dos conjuntos devuelve un conjunto con todos los elementos en común entre ambos.
intersectionS :: Eq a => Set a -> Set a -> Set a
intersectionS (ElSet xs sizex) (ElSet ys sizey) = 
  let sinRep = sinRepetidos(intersection xs ys) in
    ElSet sinRep (length sinRep)

intersection :: Eq a => [a] -> [a] -> [a]
intersection [] _ = []
intersection (x:xs) ys =  if elem x ys 
                          then x : intersection xs ys 
                          else intersection xs ys

-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a]
setToList (ElSet xs size) = xs

--------------------------------------------------------------

-- 3.  Implementar la variante del tipo abstracto Set que posee una lista y admite repetidos. En otras palabras, al agregar no va a chequear que si el elemento ya se encuentra en la lista, pero sí debe comportarse como Set ante el usuario (quitando los elementos repetidos al pedirlos, por ejemplo). Contrastar la eﬁciencia obtenida en la esta implementación, en relación con la versión que no guarda repetidos.

--O(1)
addSS :: Eq a => a -> Set a -> Set a 
addSS x (ElSet ys size) = (ElSet (x:ys) (size+1))
--ejemplo2 = (addSS 1 $ addSS 2 $ addSS 2 $ emptyS)

--belongs es la misma

--O(n) al menos
sizeSS :: Eq a => Set a -> Int
sizeSS (ElSet xs size) = length(sinRepetidos xs)

--O(n) al menos
removeSS :: Eq a => a -> Set a -> Set a 
removeSS x (ElSet ys size) = (ElSet (sinAparicionesDe x ys) (length (sinAparicionesDe x ys)))

sinAparicionesDe :: Eq a => a -> [a] -> [a]
sinAparicionesDe x [] = []
sinAparicionesDe x (y:ys) =
    if x == y
       then sinAparicionesDe x ys
       else y : sinAparicionesDe x ys

--
unionSS :: Eq a => Set a -> Set a -> Set a
unionSS (ElSet xs sizex) (ElSet ys sizey) = 
  (ElSet (sinRepetidos(xs ++ ys)) (length(sinRepetidos(xs ++ ys))))

intersectionSS :: Eq a => Set a -> Set a -> Set a 
intersectionSS (ElSet xs sizex) (ElSet ys sizey) = 
  (ElSet (sinRepetidos(intersection xs ys)) (length(sinRepetidos(intersection xs ys))))

setToListS :: Eq a => Set a -> [a]
setToListS (ElSet xs size) = sinRepetidos xs


-- Comparacion: El primer invariante de representacion resulta mas eficiente que el segundo 