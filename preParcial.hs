
















-- Ejercicio 2
f2 :: [(String, Integer, Integer)] -> [String]
f2 s = aux s []

aux :: [(String, Integer, Integer)] -> [String] -> [String]
aux [] res = res

aux ((nombre, año, cuatri):xs) res
    | año < 2021 || (año == 2021 && cuatri <= 1) = agregar nombre xs res
    | otherwise = aux xs res

agregar :: String -> [(String, Integer, Integer)] -> [String] -> [String]
agregar nombre xs res
    | pertenece nombre res = aux xs res
    | True                 = aux xs (res ++ [nombre])
    -- otherwise :o

pertenece :: String -> [String] -> Bool
pertenece _ [] = False
pertenece nombre (x:xs) = (nombre == x)|| (pertenece nombre xs)



