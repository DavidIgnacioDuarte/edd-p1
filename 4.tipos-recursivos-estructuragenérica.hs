data T a = A -- casos base
		 | B a
		 | C a a
		 | D (T a) -- casos recursivos
		 | E a (T a)
		 deriving Show

ejemploA :: T a
ejemploA  = A

ejemploB :: T Int
ejemploB  = B 12

ejemploC :: T Bool
ejemploC  = C True False

ejemploD :: T Char
ejemploD  = D (B 'a')

ejemploE :: T Int
ejemploE  = E 14 ( C 15 16 )



size :: T a -> Int
size A 			   = 0
size ( B a )	   = 1
size ( C a b )	   = 2
size ( D (t) )   = (size (t)) -- "T a" como parámetro.
size ( E a (t) ) = 1 + (size (t))



sum' :: T Int -> Int
sum' A = 0
sum' (B a) = a 
sum' (C a b) = a + b
sum' (D (t)) = sum' t 
sum' (E a (t)) = a + sum' t



hayD :: T a -> Bool
hayD A = False
hayD (B a) = False
hayD (C a b) = False
hayD (D (t)) = True 
hayD (E a (t)) = hayD t



cantE :: T a -> Int
cantE A = 0
cantE (B a) = 0
cantE (C a b) = 1
cantE (D (t)) = cantE t 
cantE (E a (t)) = cantE t



recolectarC :: T a -> [(a,a)]
recolectarC A = []
recolectarC (B a) = []
recolectarC (C a b) = [(a, b)]
recolectarC (D (t)) = recolectarC t
recolectarC (E a (t)) = recolectarC t



toList :: T a -> [a]
toList A = []
toList (B a) = [a]
toList (C a b) = [a,b]
toList (D (t)) = toList t
toList (E a (t)) = a : toList t

