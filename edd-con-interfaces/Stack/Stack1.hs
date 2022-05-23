module Stack1 where


data Stack a = St [a] deriving Show


emptyS :: Stack a
emptyS = St []



isEmptyS :: Stack a -> Bool 
isEmptyS (St x) = null x



push :: a -> Stack a -> Stack a
push a (St x) = St (a:x)



top :: Stack a -> a
top (St x) = head x 



pop :: Stack a -> Stack a
pop (St x) = St (tail x)






















