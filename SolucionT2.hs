module SolucionT2 where
import Data.Bits (Bits(xor))

-- Ejercicio 1
f1 :: Integer -> Integer -> Integer
f1 n m | n > m = 0
       | sumaInterna(divisoresPropios n 1) > n = 1 + f1 (n+1) m
       | otherwise = f1 (n+1) m

divisoresPropios :: Integer -> Integer -> [Integer]
divisoresPropios n i | i >= n = []
                     | mod n i == 0 = i : divisoresPropios n (i+1)
                     | otherwise = divisoresPropios n (i+1)
                     
sumaInterna :: [Integer] -> Integer
sumaInterna [] = 0
sumaInterna (x:xs) = x + sumaInterna xs  

-- Ejercicio 2
f2 :: [(String, Integer, Integer)] -> [String]
f2 [] = []
f2 ((s,n,m):ls) | ((n == 2021 && m <= 1) || n < 2021) && pertenece s ls == False = s : f2 ls
                | otherwise = f2 ls

pertenece :: String -> [(String, Integer, Integer)] -> Bool
pertenece _ [] = False
pertenece palabra ((s,_,_):ls) | s == palabra = True 
                   | otherwise = False || pertenece palabra ls

-- Ejercicio 3
f3 :: [Integer] -> Integer -> [Integer]
f3 [] _ = []
f3 [n] u = f3 (n:[-1]) u
f3 (n:m:xs) u | not consecutivoPositivo && (n >= 0 && n <= u) = [n]
              | not consecutivoPositivo && n > u = [u]
              | consecutivoPositivo && (n >= 0 && n <= u) = n : f3 (m:xs) u
              | consecutivoPositivo && n > u = u : f3 (m:xs) u
              | n < 0 = []
              where consecutivoPositivo = m >= 0

-- Ejercicio 4
f4 :: [[Integer]] -> Integer -> Integer
f4 [] _ = 0
f4 (c:cs) col | mod (posicionN c (col-1)) 2 == 0 = 1 + f4 cs col
              | otherwise = f4 cs col

posicionN :: [Integer] -> Integer -> Integer
posicionN [] _ = 0
posicionN (x:xs) n | n == 0 = x
                   | otherwise = posicionN xs (n-1)