module EscuelaDeMagia (
    EscuelaDeMagia,
    Mago,
    MaxHeap,
    Nombre,
    Hechizo,
    fundarEscuela,
    estaVacia,
    registrar,
    magos,
    hechizosDe,
    leFaltanAprender,
    egresarUno,
    enseniar
)
where

import Set
import Map
import Heap
import Mago

--Escuela de Magia

-- El objetivo de este examen es modelar una escuela de magos. Para ello, definiremos un tipo abstracto llamado EscuelaDeMagia.
-- Y damos por hecho que:

-- 1. Existe un tipo abstracto llamado Mago, ya implementado, cuya interfaz se adjunta en el anexo de interfaces. Un mago puede aprender hechizos, e informarnos su nombre y qué hechizos conoce.
-- 2. El tipo Hechizo es sinónimo de String, y se corresponde con el nombre de un hechizo.
-- 3. El tipo Nombre es sinónimo de String, y se corresponde con el nombre de un mago.
-- 4. Podemos suponer que dos magos son iguales si poseen el mismo nombre, y un mago es más poderoso que otro si conoce más hechizos (lo que permite ordenarlos por la cantidad de hechizos que saben).
-- 5. En la escuela no existen dos magos con el mismo nombre.

-- Representación

-- Dicho esto, la representación que utilizaremos será la siguiente (que no es posible modificar ):

data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (Heap Mago) deriving (Eq,Show)

type MaxHeap = Heap 
type Nombre = String
type Hechizo = String



instance Ord Mago where
    m `compare` n = (sizeS (hechizos m)) `compare` (sizeS (hechizos n))

instance Eq Mago where
    m == n = (nombre m) == (nombre n)

-- Esta representación utiliza:
-- 1. Un Set de hechizos que la escuela ha enseñado a lo largo de su historia.
-- 2. Un Map que asocia magos con su respectivo nombre.
-- 3. Una estructura llamada MaxHeap que posee a todos los magos de la escuela y permite obtenerlos de forma eficiente de mayor a menor en base a la cantidad de hechizos que saben.

-- Ejercicios
-- Invariantes

-- a) Dar invariantes de representación válidos según la descripción de la estructura.

-- 1. en la escuela no puede haber dos magos con el mismo nombre
-- 2. la cantidad de elementos del Map es la misma que la del MaxHeap
-- 3. si maxheap esta vacia, la escuela esta vacia
-- 3. cualquier hechizo que haya aprendido un mago debe estar en el Set

-- Implementación
-- Implementar la siguiente interfaz de EscuelaDeMagia, utilizando la representación y los costos dados, calculando los costos de cada subtarea, y siendo M la cantidad de magos y H la cantidad de hechizos:

-- b) 
-- Propósito: Devuelve una escuela vacía.
-- Eficiencia: O(1)
fundarEscuela :: EscuelaDeMagia
fundarEscuela = EDM (emptyS) (emptyM) (emptyH)

-- c) 
-- Propósito: Indica si la escuela está vacía.
-- Eficiencia: O(1)
estaVacia :: EscuelaDeMagia -> Bool
estaVacia (EDM s m h) = isEmptyH h

-- d)
-- Propósito: Incorpora un mago a la escuela (si ya existe no hace nada).
-- Eficiencia: O(log M)
registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
registrar nombre (EDM setH map heap) = 
    case lookupM nombre map of
        (Just _) -> EDM setH map heap
        Nothing  -> let mago = crearM nombre in
            EDM setH (assocM nombre mago map) (insertH mago heap) 


-- registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
-- registrar nombre (EDM set map heap) =  if pertenece nombre (domM map)
--                                           then (EDM set map heap)
--                                         else (EDM set (assocM nombre (crearM nombre) map) (insertH (crearM nombre) heap))

-- e) 
-- Propósito: Devuelve los nombres de los magos registrados en la escuela.
-- Eficiencia: O(M)
magos :: EscuelaDeMagia -> [Nombre]
magos (EDM setH map heap) = domM map

-- f) 
-- Propósito: Devuelve los hechizos que conoce un mago dado.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(log M)
hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
hechizosDe nombre (EDM setH map heap) =
    hechizos $ fromJust $ lookupM nombre map

fromJust :: Maybe a -> a
fromJust (Just x) = x

-- g) 
-- Propósito: Dado un mago, indica la cantidad de hechizos que la escuela ha dado y él no sabe.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(log M)
leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
leFaltanAprender nombre (EDM setH map heap) =
    sizeS setH - (sizeS $ hechizos $ fromJust $ lookupM nombre map)

-- h) 
-- Propósito: Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
-- Precondición: Hay al menos un mago.
-- Eficiencia: O(log M)
egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
egresarUno (EDM setH map heap) =
    let egresado = maxH(heap) in
        let escuela = EDM (setH) (deleteM (nombre egresado) map) (deleteMaxH heap) in
            (egresado, escuela)

-- i) 
-- Propósito: Enseña un hechizo a un mago existente, y si el hechizo no existe en la escuela es incorporado a la misma.
-- Nota: No importa si el mago ya conoce el hechizo dado.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(M log M + log H)
enseniar :: Hechizo -> String -> EscuelaDeMagia -> EscuelaDeMagia
enseniar h n (EDM setH map heap) =
    let mago = fromJust $ lookupM n map in
        let magoMejorado = aprender h mago in
            EDM
                (addS h setH)
                (assocM n magoMejorado map)
                (reemplazarH magoMejorado heap)

reemplazarH :: Mago -> MaxHeap Mago -> MaxHeap Mago
reemplazarH m mh =
    let magoMasPoderoso = maxH mh in
        if nombre m == nombre magoMasPoderoso
            then insertH m $ deleteMaxH mh
            else insertH magoMasPoderoso $ reemplazarH m (deleteMaxH mh)

ejemplo = 
    enseniar "ocus pocus" "Hermione" $
    registrar "Hermione" $
    enseniar "abracadabra" "Ron" $
    registrar "Ron" $
    enseniar "ocus pocus" "Harry" $
    enseniar "abracadabra" "Harry" $
    registrar "Harry"
    fundarEscuela

-- Usuario en el archivo EscuelaDeMagiaC.hs
-- Implementar las siguientes funciones como usuario del tipo EscuelaDeMagia:

-- j) 
-- Propósito: Retorna todos los hechizos aprendidos por los magos.
-- Eficiencia: O(M ∗ (log M + H log H))
--hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo


-- k) 
-- Propósito: Indica si existe un mago que sabe todos los hechizos enseñados por la escuela.
-- Eficiencia: O(log M)
--hayUnExperto :: EscuelaDeMagia -> Bool


-- l) 
-- Propósito: Devuelve un par con la lista de magos que saben todos los hechizos dados por la escuela y la escuela sin dichos
-- magos.
-- Eficiencia: O(M log M)
--egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)

-- Bonus
-- m) Dar una posible representación para el tipo Mago, de manera de que se pueda cumplir con el orden dado para cada operación de la interfaz, pero sin implementarlas.