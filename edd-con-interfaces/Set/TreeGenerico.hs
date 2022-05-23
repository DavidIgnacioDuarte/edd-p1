module TreeGenerico where

data Tree a = EmptyT
			| NT a (Tree a) (Tree a)
			deriving Show