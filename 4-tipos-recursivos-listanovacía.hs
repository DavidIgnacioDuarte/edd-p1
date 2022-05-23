data ListaNoVacia a = Unit a 
					| Cons a (ListaNoVacia a)
					  deriving Show


length' :: ListaNoVacia a -> Int 
length' (Unit a) = 1
length' (Cons a (ls)) = 1 + length' (ls)
-- length' (Cons 2(Cons 4(Cons 80(Unit 7)))) = 4
-- length' (Unit False) = 1



head' :: ListaNoVacia a -> a
head' (Unit a) = a
head' (Cons a(ls)) = a
-- head' (Cons 2(Cons 4(Cons 80(Unit 7)))) = 2
-- head' (Unit False) = False
-- Función TOTAL ya que la lista nunca es vacía.



tail' :: ListaNoVacia a -> ListaNoVacia a
tail' (Unit a) = error "No se puede devolver el resto de una lista con un elemento"
tail' (Cons a(ls)) = (ls)
-- tail' (Cons 2(Cons 4(Cons 80(Unit 7)))) = Cons 4 (Cons 80 (Unit 7))



minimo :: ListaNoVacia Int -> Int
minimo (Unit n) = n
minimo (Cons n (ln)) = min n (minimo ln)
-- minimo (Cons 2(Cons 4(Cons 80(Unit 7)))) = 2
-- Función PARCIAL ya que sólo vale con números naturales.




















