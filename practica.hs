--Ejercicio 1a
f :: Integer -> Integer
f 1 = 8 
f 4 = 131
f 16 = 16

--Ejercicio 1b
g :: Integer -> Integer
g 8 = 16 
g 16 = 4
g 131 = 1

--Ejercicio1c 
--h=fog
h :: Integer -> Integer
h x = f (g x)

--Ejercicio2 
absoluto :: Float -> Float
absoluto num | num < 0 = -num
             | otherwise = num


absoluto :: Integer -> Integer
absoluto num | num < 0 = -num
             | otherwise = num
maximoAbsoluto :: Integer -> Integer -> Integer
maximoAbsoluto x y 
           | absoluto x > absoluto y = absoluto x
           | otherwise = absoluto y 