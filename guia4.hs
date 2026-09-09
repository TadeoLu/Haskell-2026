-- Ejercicio 16 a

menorDivisor :: Integer ->Integer
menorDivisor 1 = 1
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n i | mod n i == 0 = i 
                        | otherwise = menorDivisorDesde n (i+1)
-- b
esPrimo :: Integer ->Bool
esPrimo 1 = False
esPrimo n | menorDivisor n == n = True
            | otherwise = False
-- c
sonCoprimos :: Integer ->Integer ->Bool
sonCoprimos n m | mod m (menorDivisor n) == 0 = False
                | mod n (menorDivisor m) == 0 = False
                | otherwise = True
-- d
nEsimoPrimo :: Integer ->Integer
nEsimoPrimo n = nEsimoPrimoAuxiliar n 2 0

nEsimoPrimoAuxiliar :: Integer -> Integer -> Integer -> Integer
nEsimoPrimoAuxiliar n p i | n == i = (p-1)
                        | esPrimo p == True = nEsimoPrimoAuxiliar n (p+1) (i+1)
                        | esPrimo p == False = nEsimoPrimoAuxiliar n (p+1) i
-- 14
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m = sumaPotenciasAuxiliar 0 q n m

sumaPotenciasAuxiliar :: Integer -> Integer -> Integer -> Integer -> Integer
sumaPotenciasAuxiliar suma q n m | n == 1 && m == 1 = suma
                                 | n >= m = sumaPotenciasAuxiliar (suma + q^(n+m)) q (n-1) m
                                 | otherwise = sumaPotenciasAuxiliar(suma + q^(n+m)) q n (m-1)
-- 19
esSumaInicialDePrimos :: Integer ->Bool
esSumaInicialDePrimos n | n < 0 = False
                        | otherwise = esSumaInicialDePrimosAuxiliar n 1 0

esSumaInicialDePrimosAuxiliar :: Integer -> Integer -> Integer -> Bool
esSumaInicialDePrimosAuxiliar n i suma | suma == n = True
                                  | suma > n = False
                                  | otherwise = esSumaInicialDePrimosAuxiliar n (i+1) (suma + (nEsimoPrimo i))
-- 14 version posado
posadopotencia :: Integer -> Integer -> Integer -> Integer
posadopotencia q n m | n == 1 = q^(1+m)
                     | otherwise = posadopotenciaAux q n m + posadopotencia q (n-1) m

posadopotenciaAux :: Integer -> Integer -> Integer -> Integer
posadopotenciaAux q n m | m == 1 = q^(n+1)
                        | otherwise = q^(n+m) + posadopotenciaAux q n (m-1)

esSumaInicialDePrimosp :: Integer -> Bool
esSumaInicialDePrimosp n | n < 1 = False
    | otherwise = esSumaInicialDePrimosAuxp n 1


esSumaInicialDePrimosAuxp :: Integer -> Integer -> Bool
esSumaInicialDePrimosAuxp n m | (n - nEsimoPrimo m) > 0 = esSumaInicialDePrimosAuxp (n - nEsimoPrimo m) (m+1)
        | (n - nEsimoPrimo m) == 0 = True
        | otherwise = False