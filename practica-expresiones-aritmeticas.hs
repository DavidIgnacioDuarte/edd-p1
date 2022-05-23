data ExpL = Constante Bool
		  | Variable String
		  | ExpLBin OpLBin ExpL ExpL
		  | ExpLUn OpLUn ExpL
		  deriving Show

data OpLBin = And 
			| Or
			deriving Show

data OpLUn = Not deriving Show

data Asignacion = Asig String Bool deriving Show


exp1 :: ExpL
exp1 = ExpLUn Not ( ExpLBin Or
						(ExpLBin And (Constante True) (Constante True))
						(ExpLBin And (Constante True) (Constante False)) )

exp2 :: ExpL
exp2 = ExpLUn Not(Variable "valorANegar")

exp3 = ExpLUn Not ( ExpLUn Not ( ExpLBin Or
						 (ExpLBin And (Constante True) (Constante True))
						 (ExpLBin And (Constante True) (Constante False)) ) )

variables :: ExpL -> [String]
variables (Constante b) = []
variables (Variable b) = [b]
variables (ExpLBin op eL1 eL2) = (variables eL1) ++ (variables eL2)
variables (ExpLUn op eL1) = (variables eL1)


sinConstantes :: ExpL -> Bool
sinConstantes (Constante b) = False
sinConstantes (Variable b) = True
sinConstantes (ExpLBin op eL1 eL2) = (sinConstantes eL1) || (sinConstantes eL2)
sinConstantes (ExpLUn op eL1) = (sinConstantes eL1)


operacionesBinarias :: ExpL -> [OpLBin]
operacionesBinarias (Constante b) = []
operacionesBinarias (Variable b) = []
operacionesBinarias (ExpLBin op eL1 eL2) = [op] ++ (operacionesBinarias eL1) ++
												   (operacionesBinarias eL2)
operacionesBinarias (ExpLUn op eL1) = (operacionesBinarias eL1)



evalL :: ExpL -> [Asignacion] -> Bool
evalL (Constante b) as = b
evalL (Variable b) as = (valorDe b as)
evalL (ExpLBin op eL1 eL2) as = (evalLB op (evalL eL1 as) (evalL eL2 as))
evalL (ExpLUn op eL1) as = (evalLU op (evalL eL1 as))


evalLB :: OpLBin -> Bool -> Bool -> Bool 
evalLB And b1 b2 = b1 && b2
evalLb Or b1 b2 = b1 || b2

evalLU :: OpLUn -> Bool -> Bool 
evalLU Not b = not b

valorDe :: String -> [Asignacion] -> Bool
valorDe bv [] = error ("la variable" ++ bv ++ "no esta asignada")
valorDe bv (a:as) = if (esVariable bv a)
					then (valor a)
					else (valorDe bv as)

esVariable :: String -> Asignacion -> Bool
esVariable bv (Asig nombre valor) = (bv == nombre)

valor :: Asignacion -> Bool
valor (Asig nombre valor') = valor'



simplificarL :: ExpL -> ExpL 
simplificarL (Constante b) = (Constante b)
simplificarL (Variable bv) = (Variable bv)
simplificarL (ExpLBin opB eL1 eL2) = (simplBin opB 
											   (simplificarL eL1)
											   (simplificarL eL2))
simplificarL (ExpLUn opU eL) = (simplUn opU (simplificarL eL))


simplBin :: OpLBin -> ExpL -> ExpL -> ExpL
simplBin And eL1 eL2 = (simplAnd eL1 eL2)
simplBin Or eL1 eL2 = (simplOr eL1 eL2)

simplUn :: OpLUn -> ExpL -> ExpL
simplUn Not eL = (simplNot eL)

simplAnd :: ExpL -> ExpL -> ExpL
simplAnd (Constante True) eL2 = eL2
simplAnd eL1 (Constante True) = eL1
simplAnd (Constante False) eL2 = (Constante False)
simplAnd eL1 (Constante False) = (Constante False)
simplAnd eL1 eL2 = (ExpLBin And eL1 eL2)

simplOr :: ExpL -> ExpL -> ExpL
simplOr (Constante False) eL2 = eL2
simplOr eL1 (Constante False) = eL1
simplor eL1 eL2 = (ExpLBin Or eL1 eL2)

simplNot :: ExpL -> ExpL
simplNot (ExpLUn Not(eL)) = eL
simplNot eL = (ExpLUn Not eL)














