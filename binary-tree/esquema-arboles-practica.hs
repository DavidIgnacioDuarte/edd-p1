
data Objeto = Espada | Oro Int | Escudo deriving Show
data Cofre  = C [Objeto] deriving Show

-- data Mapa =
--       FinMapa
--     | Bifurcacion Cofre Mapa Mapa

-- string es sinonimo de char
-- type String = [Char] 

-- los mapas son sinonimos de...
type Mapa = Tree Cofre

data Tree a =
      EmptyT
    | NodeT a (Tree a) (Tree a)
    deriving Show

sumT :: Tree Int -> Int
sumT EmptyT = 0
sumT (NodeT x ti td) = 
    x + (sumT ti) + (sumT td)

-- la longitud de la lista
-- length :: [a] -> Int

-- el tamaño del arbol
-- (la cantidad de nodos)
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x ti td) =
    1 + sizeT ti + sizeT td

-- la altura es la longitud
-- del camino mas largo
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x ti td) =
    1 + max (heightT ti) (heightT td)

elemsCamMasLargo :: Tree a -> [a]
elemsCamMasLargo EmptyT = []
elemsCamMasLargo (NodeT x ti td) =
    x :
    if heightT ti > heightT td
       then elemsCamMasLargo ti
       else elemsCamMasLargo td

hoja :: a -> Tree a
hoja x = NodeT x EmptyT EmptyT

ejemT1 :: Tree Int
ejemT1 = hoja 1

ejemT2 :: Tree Int
ejemT2 = EmptyT

ejemT3 :: Tree Int
ejemT3 =
    NodeT 1
      (hoja 2)
      (hoja 3)

ejemT4 :: Tree Int
ejemT4 =
    NodeT 1
      (NodeT 2 
        (hoja 4)
        (hoja 5)
      )
      (hoja 3)

ejemT5 :: Tree Int
ejemT5 =
    NodeT 1
      (NodeT 2 
        (hoja 4)
        (NodeT 5 
          (hoja 6)
          (hoja 7)
        )
      )
      (hoja 3)

-- hoja en inglés es leaf
-- thief el plural thieves
-- life el pluar es lives

isEmptyT EmptyT = True
isEmptyT _      = False

-- intercalar e [] = []
-- intercalar e [x] = [x]
-- intercalar e (x:xs) =
--     x : e : intercalar e xs

leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x EmptyT EmptyT) = [x]
leaves (NodeT x ti td) =
    leaves ti ++ leaves td

    -- opcion 2
    -- if isEmptyT ti && isEmptyT td
    --    then x : leaves ti ++ leaves td
    --    else leaves ti ++ leaves td

    -- opcion 3
    -- agregarSiEsHoja x ti td
    --     (leaves ti ++ leaves td)

    -- agregarSiEsHoja :: a -> Tree a -> Tree a -> [a] -> [a]
    -- agregarSiEsHoja x EmptyT EmptyT rs = x : rs
    -- agregarSiEsHoja x _      _      rs = rs

data TernaryTree a =
      EmptyTT
    | NodeTT a (TernaryTree a)
               (TernaryTree a)
               (TernaryTree a)

sizeTT :: TernaryTree a -> Int
sizeTT EmptyTT = 0
sizeTT (NodeTT x t1 t2 t3) =
    1 + sizeTT t1
      + sizeTT t2
      + sizeTT t3
