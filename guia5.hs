-- Ej 1
-- a
longitud :: [t] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

--b
ultimo :: [t] -> t
ultimo [x] = x
ultimo (_:xs) = ultimo xs

-- c
principio :: [t] -> [t]
principio [x] = []
principio (x:xs) = x : principio xs


-- d
reverso :: [t] -> [t]
reverso [] = []
reverso lista = ultimo lista : reverso (principio lista)

-- Ej 2
-- a
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x lista | head lista == x = True
                  | otherwise = False || pertenece x (tail lista)

-- b
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [x] = True
todosIguales lista | head lista /= head (tail lista) = False
                   | otherwise = True && todosIguales (tail lista)

--c
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [x] = True
todosDistintos lista | pertenece (head lista) (tail lista) == True = False
                     | otherwise = True && todosDistintos (tail lista)

-- d
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [x] = False
hayRepetidos lista | pertenece (head lista) (tail lista) == True = True
                     | otherwise = False || hayRepetidos (tail lista)

-- e 
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar x lista | x == head lista = tail lista
               | otherwise = head lista : quitar x (tail lista) 

-- f
quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x lista | x == head lista = quitarTodos x (tail lista)
               | otherwise = head lista : quitarTodos x (tail lista) 

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitar x xs) 

-- g
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [x] _ = True
mismosElementos _ [y] = True
mismosElementos l1 l2 | pertenece (head l1) l2 /= True = False
                      | otherwise =  True && mismosElementos (tail l1) l2 && mismosElementos (tail l1) (tail l2)