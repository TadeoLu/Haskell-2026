--Ejercicio 1
doble :: Integer -> Integer
doble x = x + x

f :: Integer -> Integer
f x | x == 1 = 8
    | x == 4 = 131
    | x == 16 = 16
    | otherwise = undefined

g :: Integer -> Integer
g x | x == 8 = 16
    | x == 16 = 4 
    | x == 131 = 1
    | otherwise = undefined

h :: Integer -> Integer
h x = f (g x)

k :: Integer -> Integer
k x = g (f x)

--Ejercicio 2

-- a)

absoluto :: Integer -> Integer
absoluto x | x >= 0 = x
            | otherwise = x * (-1)

-- b)

maximoAbsoluto :: Integer -> Integer -> Integer
maximoAbsoluto x y | (absoluto x) >= (absoluto y) = (absoluto x)
                    | otherwise = (absoluto y)

--c) 

maximo3 :: Integer -> Integer -> Integer -> Integer 
maximo3 n1 n2 n3 | n1 > n2 && n1 > n3 = n1
                    | n2 >= n1 && n2 > n3 = n2
                    | n3 >= n1 && n3 >= n2 = n3

-- d)

algunoEsCero :: Rational -> Rational -> Bool
algunoEsCero x y | x == 0 = True
                | y == 0 = True
                | otherwise = False

algunoEsCeroPattern :: Rational -> Rational -> Bool
algunoEsCeroPattern 0 y = True
algunoEsCeroPattern x 0 = True
algunoEsCeroPattern x y = False

-- e)

ambosCero :: Rational -> Rational -> Bool
ambosCero 0 0 = True
ambosCero x y = False

ambosCero2 :: Rational -> Rational -> Bool
ambosCero2 x y | x == 0 && y == 0 = True
                | otherwise = False

-- f) 

enMismoIntervalo :: Float -> Float -> Bool
enMismoIntervalo x y | x <= 3 && y <= 3 = True
                    | x > 3 && x < 7 && y > 3 && y < 7 = True
                    | x >= 7 && y >= 7 = True
                    | otherwise = False

-- g)

sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos a b c | a == b = a+c 
                    | a == c = a+b 
                    | a == b && a == c = a 
                    | b == a = b + c
                    | b == c = b + a 
                    | b == a && b == c = b 
                    | c == a = c+b 
                    | c == b = c+a 
                    | c == a && c == b = c 

--  h)

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe a b | mod a b == 0 = True
                | otherwise = False

-- i)

digitoUnidades :: Integer -> Integer
digitoUnidades a = mod (absoluto(a)) 10

-- j)

digitoDecimas :: Integer -> Integer
digitoDecimas a = div (mod (absoluto(a)) 100) 10

-- Ejercicio 3 
estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados a b | a == 0  || b == 0 = False
                        | mod (-(a * a)) (a * b) == 0 = True
                        | otherwise = False

-- ejercicio 4
-- a)

productoInterno ::  (Float,Float) -> (Float,Float) -> Float
productoInterno (x,y) (a,b) = (x*a) + (y*b)

-- b)   

esParMenor :: (Float,Float) -> (Float,Float) -> Bool
esParMenor (x,y) (a,b) | x < a && y < b = True
                        | otherwise = False

-- c)

restaTupla :: (Float,Float) -> (Float,Float) -> (Float,Float)
restaTupla (x,y) (a,b) = ((x-a),(y-b))

hipotenusaTupla :: (Float,Float) -> Float
hipotenusaTupla (x,y) = sqrt (x*x + y*y)

distancia :: (Float,Float) -> (Float,Float) -> Float
distancia (x,y) (a,b) = hipotenusaTupla (restaTupla (x,y) (a,b))

-- d)

sumaTerna :: (Integer,Integer,Integer) -> Integer
sumaTerna (a,b,c) = a + b + c

-- e) 

sumarSoloMultiplos :: (Integer,Integer,Integer) -> Integer -> Integer
sumarSoloMultiplos (a,b,c) n | mod a n == 0 && not(mod b n == 0) && not(mod c n == 0) = a 
                            | mod a n == 0 && mod b n == 0 && not(mod c n == 0) = a + b 
                            | mod a n == 0 && mod b n == 0 && mod c n == 0 = a + b + c 
                            | mod a n == 0 && not(mod b n == 0) && mod c n == 0 = a + c 
                            | not(mod a n == 0) && mod b n == 0 && not(mod c n == 0) = b 
                            | not(mod a n == 0) && mod b n == 0 && mod c n == 0 = b + c
                            | not(mod a n == 0) && not(mod b n == 0) && mod c n == 0 = c  
                            | otherwise = 0

-- f)

posPrimerPar :: (Integer,Integer,Integer) -> Integer
posPrimerPar (a,b,c) | mod a 2 == 0 = 1
                    | mod b 2 == 0 = 2
                    | mod c 2 == 0 = 3
                    | otherwise = 4

-- g)

crearPar :: Float -> Float -> (Float,Float)
crearPar a b =(a,b)

-- h)

invetir :: (Float,Float) -> (Float,Float)
invetir (x,y) = (y,x)

-- i)

type Punto2D =(Float,Float)

{-
productoInterno ::  (Punto2D -> Punto2D -> Float
productoInterno (x,y) (a,b) = (x*a) + (y*b)

esParMenor :: Punto2D -> Punto2D -> Bool
esParMenor (x,y) (a,b) | x < a && y < b = True
                        | otherwise = False

distancia :: Punto2D -> Punto2D -> Float
distancia (x,y) (a,b) = hipotenusaTupla (restaTupla (x,y) (a,b))
-}

-- ejercicio 5

f2 ::  Integer -> Integer
f2 n | n <= 7 = n * n
        |otherwise = 2*n - 1

g2 ::  Integer -> Integer
g2 n | mod n 2 == 0 = div n 2
        |otherwise = 3*n + 1

todosMenores :: (Integer,Integer,Integer) -> Bool
todosMenores (a,b,c) | (f2 a) > (g2 a)  && ((f2 b) > (g2 b) && (f2 c) > (g2 c)) = True
                    | otherwise = False

-- ejercicio 6

type Anio = Integer
type EsBisiesto = Bool

bisiesto :: Anio -> EsBisiesto
bisiesto anio | mod anio 4 /= 0 = False
                | mod anio 100 == 0 && mod anio 400 /= 0 = False
                | otherwise = True

absolutoF :: Float -> Float
absolutoF x | x >= 0 = x
            | otherwise = x * (-1) 

distanciaManhattan:: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (x,y,z) (a,b,c) = absolutoF(x - a) + absolutoF(y - b) + absolutoF(z - c)

type Punto3D = (Float, Float, Float)

distanciaManhattanP:: Punto3D -> Punto3D -> Float
distanciaManhattanP (x,y,z) (a,b,c) = absolutoF(x - a) + absolutoF(y - b) + absolutoF(z - c)

-- Ejercicio 8

sumaUltimosDosDigitos :: Integer -> Integer
sumaUltimosDosDigitos x = mod (absoluto(x)) 10 + mod (absoluto(div x 10)) 10

comparar :: Integer -> Integer -> Integer
comparar a b | sumaUltimosDosDigitos(a) < sumaUltimosDosDigitos(b) = 1
                | sumaUltimosDosDigitos(a) > sumaUltimosDosDigitos(b) = -1 
                | sumaUltimosDosDigitos(a) == sumaUltimosDosDigitos(b) = 0


