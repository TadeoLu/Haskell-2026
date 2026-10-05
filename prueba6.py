def triadaPitagorica(a:int, b:int, c:int) -> bool:
    return (a**2 + b**2) == c**2

def esMultiploDe(n:int, m:int) -> bool:
    return n%m == 0

def dobleSiEsPar(n:int) -> int:
    if n%2 == 0:
        return n*2
    return n

def fahrenheitACelsius(t:float) -> float:
    return (t-32)*(5/9)