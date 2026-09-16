module Guia5 where
import GHC.Float (fromRat'')


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
mismosElementos l1 l2 = mismosElementosAuxiliar l1 l2 && mismosElementosAuxiliar l2 l1 

mismosElementosAuxiliar :: (Eq t) => [t] -> [t] -> Bool
mismosElementosAuxiliar [] _ = True
mismosElementosAuxiliar (x:xs) lista | pertenece x lista == False = False
                                     | otherwise = True && mismosElementosAuxiliar xs lista 
lista1 = [1,2,3,4,5]
lista2 = [1..10]

-- h
capicua :: (Eq t) => [t] -> Bool
capicua lista = igualExacto lista (reverso lista)

igualExacto :: (Eq t) => [t] -> [t] -> Bool
igualExacto [] [] = True
igualExacto (x:xs) (y:ys) | longitud xs /= longitud ys = False
                          | x /= y = False
                          | otherwise = True && igualExacto xs ys

-- Ejercicio 3 a
sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- b
productoria :: [Integer] -> Integer
productoria [] = 0
productoria (x:xs) = x * productoria xs

--c 
maximo :: [Integer] -> Integer
maximo [] = 0
maximo (x:xs) | mayorQueTodos x xs == True = x
              | otherwise = maximo xs

mayorQueTodos :: Integer -> [Integer] -> Bool
mayorQueTodos _ [] = True
mayorQueTodos n (x:xs) | n >= x = True && mayorQueTodos n xs
                       | otherwise = False

-- d
sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN n (x:xs) = x+n : (sumarN n xs)

-- e
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (x:xs) = sumarN x (x:xs)

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo lista = sumarN (ultimo lista) lista

-- f
pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | mod x 2 == 0 = x : pares xs
             | otherwise = pares xs

-- g
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (x:xs) | mod x n == 0 = x : multiplosDeN n xs
                      | otherwise = multiplosDeN n xs

-- h
ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar lista = minimo lista : ordenar (quitar (minimo lista) lista)

minimo :: [Integer] -> Integer
minimo [] = 0
minimo (x:xs) | menorQueTodos x xs == True = x
              | otherwise = minimo xs

menorQueTodos :: Integer -> [Integer] -> Bool
menorQueTodos _ [] = True
menorQueTodos n (x:xs) | n <= x = True && menorQueTodos n xs
                       | otherwise = False

--Ej 4 a
sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [c] = [c]
sacarBlancosRepetidos (c1:c2:cs) | c1 == ' ' && c2 == ' ' = sacarBlancosRepetidos cs
                             | otherwise = c1 : c2 : sacarBlancosRepetidos cs
-- b
contarPalabras :: [Char] -> Integer
contarPalabras [] = 1
contarPalabras (c:cs) | c == ' ' = 1 + contarPalabras cs
                      | otherwise = contarPalabras cs
-- c
palabras :: [Char] -> [[Char]]
palabras [] = []
palabras caracteres = extraerPalabra caracteres : palabras (quitarPrimerPalabra caracteres)

extraerPalabra :: [Char] -> [Char]
extraerPalabra [c] = [c]
extraerPalabra (c1:c2:cs) | c2 == ' ' = [c1]
                          | otherwise = c1 : extraerPalabra (c2:cs)

quitarPrimerPalabra :: [Char] -> [Char]
quitarPrimerPalabra [] = []
quitarPrimerPalabra (c:cs) | c == ' ' = cs
                           | otherwise = quitarPrimerPalabra cs

-- d
palabraMasLarga :: [Char] -> [Char]
palabraMasLarga [] = []
palabraMasLarga caracteres | mayorQueTodos (contarCaracteres (extraerPalabra caracteres)) (cantidadCaracteresPalabras caracteres) == True = extraerPalabra caracteres
                           | otherwise = palabraMasLarga (quitarPrimerPalabra caracteres)

contarCaracteres :: [Char] -> Integer
contarCaracteres [] = 0
contarCaracteres (c:cs) = 1 + contarCaracteres cs

cantidadCaracteresPalabras :: [Char] -> [Integer]
cantidadCaracteresPalabras [] = []
cantidadCaracteresPalabras caracteres = contarCaracteres (extraerPalabra caracteres) : cantidadCaracteresPalabras (quitarPrimerPalabra caracteres)

aplanar :: [[Char]] -> [Char]
aplanar [] = []
aplanar (s:ss) =  s ++ aplanar ss

aplanarConBlancos :: [[Char]] -> [Char]
aplanarConBlancos [s] = s
aplanarConBlancos (s:ss) =  s ++ ' ' : aplanarConBlancos ss

