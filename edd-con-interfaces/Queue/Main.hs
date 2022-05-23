import Queue1


lengthQ :: Queue a -> Int
lengthQ q = if not (isEmptyQ q) 
			then 1 + lengthQ (dequeue q)
			else 0



queueToList' :: Queue a -> [a]
queueToList' q      = if 	not (isEmptyQ q)
					  then  firstQ q : queueToList' (dequeue q)
					  else  []



unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if not (isEmptyQ q2)
			   then unionQ (queue (firstQ q2) q1) (dequeue q2)
			   else q1








