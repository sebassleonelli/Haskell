valorAbsoluto::Int->Int
valorAbsoluto x = if x < 0 then -x else x

valorAbsoluto2::Int->Int
valorAbsoluto2 x | x < 0 = -x
                | otherwise = x

bisiesto::Int->Bool
bisiesto x | mod x 4 == 0 && mod x 100 /= 0 = True
           | mod x 400 == 0 = True
           | otherwise = False

factorial::Int->Int
factorial 0 = 1
factorial x = x * factorial (x-1)

-- Función que cuenta los divisores primos de un número
cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = length (filter esPrimo (divisores n))

-- Función que devuelve todos los divisores de un número
divisores :: Int -> [Int]
divisores n = [d | d <- [1..n], mod n d == 0]

-- Función que verifica si un número es primo
esPrimo :: Int -> Bool
esPrimo x
  | x < 2 = False
  | otherwise = not (tieneDivisor x 2)

-- Función que verifica si un número tiene un divisor
tieneDivisor :: Int -> Int -> Bool
tieneDivisor x d
  | d * d > x = False  -- Si d^2 es mayor que x, no hay más divisores
  | mod x d == 0 = True  -- Si encontramos un divisor, entonces no es primo
  | otherwise = tieneDivisor x (d + 1)  -- Verificamos el siguiente divisor

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)

aEntero :: Either Bool Int -> Int
aEntero (Right x) = x
aEntero (Left True) = 1
aEntero (Left False) = 0

limpiar :: [Char] -> [Char] -> [Char]
limpiar [] _ = []
limpiar xs (y:ys) = if elem y xs then limpiar xs ys else y : limpiar xs ys

difPromedio :: [Int] -> [Int]
difPromedio xs = map (\x -> x - promedio xs) xs

promedio :: [Int] -> Int
promedio xs = div (sum xs) (length xs)

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales (x:xs) = all (== x) xs

data AB a = Nil | Bin (AB a) a (AB a)

esVacio :: AB a -> Bool
esVacio Nil = True
esVacio _   = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq x der) = Bin (negacionAB izq) (not x) (negacionAB der)

productoAB :: AB Int -> Int
productoAB Nil = 0
productoAB (Bin izq x der) = x * productoAB izq * productoAB der