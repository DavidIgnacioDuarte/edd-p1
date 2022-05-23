import EscuelaDeMagia
import Set

-- Propósito: Retorna todos los hechizos aprendidos por los magos.
-- Eficiencia: O(M ∗ (log M + H log H))
hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
hechizosAprendidos escuela =
    hechizosAprendidos' (magos escuela) escuela

hechizosAprendidos' :: [Nombre] -> EscuelaDeMagia -> Set Hechizo
hechizosAprendidos' [] _ = emptyS
hechizosAprendidos' (m:ms) esc =
    unionS (hechizosDe m esc) (hechizosAprendidos' ms esc)

-- Propósito: Indica si existe un mago que sabe todos los hechizos enseñados por la escuela.
-- Eficiencia: O(log M)
hayUnExperto :: EscuelaDeMagia -> Bool
hayUnExperto escuela = hayUnExperto' escuela (magos escuela)

--no se eficiencia
hayUnExperto' :: EscuelaDeMagia -> [Nombre] -> Bool
hayUnExperto' escuela [] = False
hayUnExperto' escuela (m:ms) =
    if esExperto escuela m
        then True
        else hayUnExperto' escuela ms

--Eficiencia: O(log M)
esExperto :: EscuelaDeMagia -> Nombre -> Bool
esExperto escuela m =
    (leFaltanAprender m escuela) == 0

-- Propósito: Devuelve un par con la lista de magos que saben todos los hechizos dados por la escuela y la escuela sin dichos magos.
-- Eficiencia: O(M log M)
egresarExpertos :: EscuelaDeMagia -> ([Nombre], EscuelaDeMagia)
egresarExpertos escuela = ((magosExpertos escuela (magos escuela)) , (escuelaSinExpertos escuela))

--O(log M)
magosExpertos :: EscuelaDeMagia -> [Nombre] ->  [Nombre]
magosExpertos escuela [] = []
magosExpertos escuela (m:ms) =
    case hayUnExperto escuela of
        True -> (agregarSiEsExperto m escuela) ++ magosExpertos escuela ms
--     True -> agregarSiEsExperto m (magosExpertos escuela ms)

-- --O(log M)
agregarSiEsExperto :: Nombre -> EscuelaDeMagia -> [Nombre]
agregarSiEsExperto mago escuela = if esExperto escuela mago
    then [mago]
    else []

escuelaSinExpertos :: EscuelaDeMagia -> EscuelaDeMagia
escuelaSinExpertos escuela = case hayUnExperto escuela of 
    True -> egresarUno (escuelaSinExpertos escuela)
    False -> escuelaSinExpertos escuela




ejemplo = 
    enseniar "ocus pocus" "Hermione" $
    registrar "Hermione" $
    enseniar "abracadabra" "Ron" $
    registrar "Ron" $
    enseniar "ocus pocus" "Harry" $
    enseniar "abracadabra" "Harry" $
    registrar "Harry"
    fundarEscuela