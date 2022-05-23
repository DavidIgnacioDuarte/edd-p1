import Stack1
import TreeGenerico

apilar :: [a] -> Stack a
apilar [] = emptyS
apilar (x:xs) = push x (apilar xs) 



desapilar :: Stack a -> [a]
desapilar stack = if not (isEmptyS stack)
				  then top stack : desapilar (pop stack) 
				  else []



treeToStack :: Tree a -> Stack a
treeToStack EmptyT = emptyS
treeToStack tree = apilar (listInOrder tree)


listInOrder :: Tree a -> [a]
listInOrder EmptyT = []
listInOrder (NT n t1 t2) = (listInOrder t1) ++ [n] ++ (listInOrder t2)










