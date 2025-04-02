import Prelude  
--curry :: ((a, b) -> c) -> (a -> b -> c)
--(curry f) x y = f (x, y)  -- como la asociacion es a que se aplica primero, se puede omitir el parentesis, devuelve una funcion que recibe un argumento y devuelve el resultado de aplicar f a x y y
                          -- posible solucion, se puede escribir como una lambda
--curry :: ((a, b) -> c) -> (a -> b -> c)
--curry f = \x y -> f (x, y) -- calculo lambda que recibe dos argumentos y devuelve el resultado de aplicar f a x y y
-- curry f = \x -> \y -> f (x, y) -- otra forma de escribirlo, se puede escribir como una lambda

--uncurry :: (a -> b -> c) -> ((a, b) -> c)
--(uncurry f) (x, y) = f x y -- devuelve una funcion que recibe una tupla y devuelve el resultado de aplicar f a los elementos de la tupla

--uncurry :: (a -> b -> c) -> ((a, b) -> c)
--uncurry f = \(x, y) -> f x y -- calculo lambda que recibe una tupla y devuelve el resultado de aplicar f a los elementos de la tupla
--funcion inversa del curry

-- ejemplo :: curry prod 2 5 = prod (2, 5)
-- ejemplo :: uncurry (prod´(2, 5) = prod 2 5 -- donde prod´ es una funcion ya curryficada, en especifico es la currificacion de prod
-- tuplas vs funciones que se aplican varias veces con un solo argumento 
-- esto se usa para ver el metodo de evaluacion parcial

prod :: Int -> Int -> Int
prod x y = x * y

-- doble x = prod 2 x devuelve tipo de dato INT 
-- doble y prod 2 son redundantes,como se aplican a la misma x se puede obviar y pomner doble = prod 2
-- (+) 1 es una funcion que suma uno y va de Int a Int
triple ::Float -> Float
triple = (*) 3  -- funcion que multiplica por 3

esMayorDeEdad :: Int -> Bool
esMayorDeEdad = (<) 17 -- funcion que verifica si es mayor de edad (>=) 18 __ Lo que va en los guiones bajos es el argumento que se va a comparar con respecto a 18, 17 menor igual a 20 es notacion infija

-- (.) toma dos funciones y devuelve una funcion que es la composicion de las dos funciones 
(.) :: (b -> c) -> (a -> b) -> a -> c --(a->b) es la funcion que aplico primero
(.) f g x = f (g x) -- f(g(x)) es la composicion de f y g, sin los parentesis, hubiera hecho (f g) x y eso no es lo que se quiere, se quiere f(g(x))

-- (.) f g = \x -> f (g x) -- otra forma de escribirlo, se puede escribir como una lambda y x es de tipo a

-- flip toma una funcion que recibe dos argumentos y devuelve una funcion que recibe los argumentos en orden inverso, por ejemplo si tengo una resta 1 - 5, con flip hace 5 - 1, devuelve una funcion que hace las cosas al reves y los toma de forma currificada
-- flip f x y = f y x -- f es la funcion que se va a invertir, x y y son los argumentos que se van a invertir
-- haskell asocia los parametros a derecha por defecto, por eso se puede omitir el parentesis t1 -> t2 -> t3 -> t4 = t1 -> (t2 -> (t3 -> t4))
flip :: (a -> b -> c) -> b -> a -> c
flip f = \x y -> f y x 

-- ($) toma un argumento y lo aplica a una funcion, es una funcion de aplicacion de funciones, se usa para evitar parentesis, f(g(h(x))) = f $ g $ h x (podria no poner la x), el pesos asocia a derecha y es de menor prioridad que los operadores aritmeticos
($) :: (a -> b) -> a -> b
($) f = f -- no hace falta poner el argumento, se puede omitir, la f recibe algo de tipo a y devuelve algo de tipo b

-- const toma un valor y devuelve una funcion que siempre devuelve ese valor, es una funcion constante, se usa mucho para casos base en funciones recursivas
const :: a -> b -> a
const x _ = x -- devuelve el primer argumento y no importa el segundo argumento

--const x = \_ -> x -- otra forma de escribirlo, se puede escribir como una lambda
--const = \x -> \_ -> x -- otra forma de escribirlo, se puede escribir como una lambda

-- flip ($) 0 f = ($) f 0 = f 0 
-- los parentesis se usan cuando estan en notacion infija, si se usa en notacion prefija, se puede omitir

-- (== 0) . (flip mod 2) es mod al reves, compuesta con una funcion que devuelve true si el argumento es 0, es una funcion que verifica si un numero es par
-- (== 0) . (flip mod 2) 6  = (== 0) (flip mod 2 6) = (== 0) (mod 6 2) = (== 0) 0 = True

-- listas infinitas, se pueden hacer listas infinitas en haskell, por ejemplo [1..] es una lista infinita que empieza en 1 y sigue hasta el infinito, se puede hacer con cualquier numero, [2..] empieza en 2 y sigue hasta el infinito

-- listas por extension, se pueden hacer listas por extension, por ejemplo [1, 2, 3, 4, 5] es una lista que contiene los numeros del 1 al 5, se pueden hacer listas de cualquier tipo de dato, [True, False, True] es una lista de booleanos
-- listas por comprension, se pueden hacer listas por comprension, por ejemplo [x | x <- [1..5]] es una lista que contiene los numeros del 1 al 5, se pueden hacer listas de cualquier tipo de dato, [x | x <- [True, False, True]] es una lista de booleanos, [expresion | selectores, condiciones]
-- secuencias, se pueden hacer secuencias, por ejemplo [1, 3..10] es una secuencia que empieza en 1 y sigue de 3 en 3 hasta llegar a 10, se pueden hacer secuencias de cualquier tipo de dato, [True, False, True, False] es una secuencia de booleanos

-- como trabajar con listas infinitas sin que se cuelgue?
-- take 5 [1..] = [1, 2, 3, 4, 5] toma los primeros 5 elementos de la lista infinita [1..]
-- take 5 [1, 3..] = [1, 3, 5, 7, 9] toma los primeros 5 elementos de la secuencia [1, 3..]

-- evaluacion lazy reduce a expreseiones finitas, si o fuera lazy podria no terminar

-- funciones de alto orden, son funciones que reciben funciones como argumentos o devuelven funciones como resultado, se pueden hacer funciones de alto orden en haskell, por ejemplo map, filter, foldl, foldr, zipWith, etc

-- maximo :: Ord a => [a] -> a
-- maximo [x] = x
-- maximo (x:xs) = if x > maximo xs then x else maximo xs

-- minimo :: Ord a => [a] -> a
-- minimo [x] = x
-- minimo (x:xs) = if x < minimo xs then x else minimo xs

-- listaMasCorta :: [[a]] -> [a]
-- listaMasCorta [x] = x
-- listaMasCorta (x:xs) = if length x < length (listaMasCorta xs) then x else listaMasCorta xs

--mejorSegun:: (a -> a -> Bool) -> [a] -> a
--mejorSegun f [x] = x
--mejorSegun f (x:xs) = if f x (rec) then x else rec
        --where rec = mejorSegun f xs

maximo:: Ord a => [a] -> a
maximo = mejorSegun(>) -- maximo = mejorSegun(>) es lo mismo que maximo xs = mejorSegun(>) xs

minimo:: Ord a => [a] -> a
minimo = mejorSegun(<) -- minimo = mejorSegun(<) es lo mismo que minimo xs = mejorSegun(<) xs

listaMasCorta:: [[a]] -> [a]
listaMasCorta = mejorSegun(\x y -> length x < length y) -- listaMasCorta = mejorSegun(\x y -> length x < length y) es lo mismo que listaMasCorta xs = mejorSegun(\x y -> length x < length y) xs

-- funcion filter
-- filter toma una funcion que devuelve un booleano y una lista y devuelve una lista que contiene los elementos de la lista que cumplen la condicion de la funcion, por ejemplo filter even [1, 2, 3, 4, 5] = [2, 4], filter odd [1, 2, 3, 4, 5] = [1, 3, 5], filter (== 0) [1, 2, 3, 0, 4, 0, 5] = [0, 0]
-- filter f [x1, x2, x3, ..., xn] = [x | x <- [x1, x2, x3, ..., xn], f x]
-- filter f [] = [] -- caso base, si la lista esta vacia, devuelve una lista vacia

deLongitudN:: Int -> [[a]] -> [[a]]
deLongitudN n = filter(\x -> length x == n) -- deLongitudN n = filter(\x -> length x == n) es lo mismo que deLongitudN n xs = filter(\x -> length x == n) xs

soloPuntosFijosEnN:: Int -> [Int -> Int] -> [Int -> Int]
soloPuntosFijosEnN n = filter(\f -> f n == n) -- soloPuntosFijosEnN n = filter(\f -> f n == n) es lo mismo que soloPuntosFijosEnN n xs = filter(\f -> f n == n) xs, me devuelve la listas de funciones las cuales al aplicarles un n me devuelven n

-- funcion map
-- map toma una funcion y una lista y devuelve una lista que es el resultado de aplicar la funcion a cada elemento de la lista, por ejemplo map (+1) [1, 2, 3] = [2, 3, 4], map (*2) [1, 2, 3] = [2, 4, 6], map (== 0) [1, 2, 3] = [False, False, False]
-- map f [x1, x2, x3, ..., xn] = [f x1, f x2, f x3, ..., f xn]
-- map f [] = [] -- caso base, si la lista esta vacia, devuelve una lista vacia

reverseAnidado :: [[Char]] -> [[Char]]
reverseAnidado xs = reverse(map reverse xs) -- en este caso no se puede obviar el xs ya que esta dentro de una funcion
-- reverseAnidado = reverse . map(reverse) -- otra forma de escribirlo, escrito como composicion de funciones
-- reverseAnidado = \xs -> map reverse (reverse xs) -- otra forma de escribirlo, escrito como lambda

paresCuadrados :: [Int] -> [Int]
paresCuadrados xs = map(\x -> if even x then x*x else x) xs -- si es par, devuelve el cuadrado, si no, devuelve el mismo numero

-- funcion foldl y foldr
--foldl y foldr son funciones que toman una funcion, un valor inicial y una lista y devuelven un valor, la diferencia entre foldl y foldr es el orden en el que se aplican los argumentos, foldl aplica la funcion de izquierda a derecha y foldr aplica la funcion de derecha a izquierda, por ejemplo foldl (+) 0 [1, 2, 3, 4, 5] = 15, foldr (+) 0 [1, 2, 3, 4, 5] = 15, foldl (-) 0 [1, 2, 3, 4, 5] = -15, foldr (-) 0 [1, 2, 3, 4, 5] = 3
-- foldl f z [x1, x2, x3, ..., xn] = f (... (f (f z x1) x2) ...) xn
-- foldr f z [x1, x2, x3, ..., xn] = f x1 (f x2 (f x3 (... (f xn z) ...)))
-- foldl f z [] = z -- caso base, si la lista esta vacia, devuelve el valor inicial
-- foldr f z [] = z -- caso base, si la lista esta vacia, devuelve el valor inicial

--ejemplo de foldr

sumar :: [Int] -> Int
sumar [] = 0
sumar (x:xs) = x + sumar xs --recursion explicita

--sumar' :: [Int] -> Int
--sumar' = foldr (+) 0 --recursion implicita, agarro el resultado de la recursion y le sumo la cabeza de la lista, el cero corresponde al caso base

--filter' :: (a -> Bool) -> [a] -> [a]
--filter' f = foldr (\x recursion-> if f x then x:recursion else  recursion) []--si cumple la condicion, lo agrego a la lista, si no, no lo agrego

--map' :: (a -> b) -> [a] -> [b]
--map' f = foldr (\x recursion -> f x : recursion) [] --aplico la funcion a la cabeza de la lista y la agrego a la lista, luego sigo con la recursion

--listaComp:: (a -> Bool) -> (a -> b) -> [a] -> [b]
--listaComp f g = map g (.) filter f --composicion de funciones


-- Ejercicio 1

--1 max2 es de tipo (a,a) -> a -, recibe una tupla y devuelve el mayor de los dos elementos, esta funcion no esta currificada
--2 normaVectorial se comporta de la misma manera que max2, recibe una tupla y devuelve un numero, esta funcion no esta currificada
--3 subtract = flip (-) como se le esta aplicando flip a la resta y el tipo de dato de la resta es (-) :: Num a => a -> a -> a, el tipo de dato de subtract es Num a => a -> a -> a, es decir, recibe dos numeros x e y, y los invierte a y x, luego devuelve un numero, esta funcion esta currificada
--4 predecesor = subtract 1, como predescesor es igual a subtract 1, predecesor debe recibir un parametro y restarle 1, es decir, predecesor es de tipo Num a => a -> a ya que subtract tiene un parametro fijo
--5 evaluarEnCero = \f-> f 0, como toma una funcion que recibe un numero de tipo a y devuelve uno de tipo b, ya que solamente evalua en cero, el tipo de dato de la funcion va a ser Num a => (a -> b) -> b
--6 dosVeces = \f-> f . f,  como toma una funcion sabemos que tener el estilo de Num a => a -> a, ya que recibe una funcion y devuelve una funcion, la funcion que devuelve es la composicion de la funcion que recibe consigo misma, es decir, dosVeces es de tipo Num a => (a -> a) -> a -> a, ademas usa la notacion (.) que es de tipo (b -> c) -> (a -> b) -> a -> c, para este caso cambiamos los nombres de las variables para que tenga sentido la composicion consigo misma
--7 flipAll = map flip, como flipAll es igual a map flip, flipAll recibe una lista de funciones y devuelve una lista de funciones, es decir, flipAll es de tipo (a -> b -> c) -> [a -> b -> c] -> [b -> a -> c], ya que flip recibe una funcion de dos argumentos y devuelve una funcion que los invierte
--8 flipRaro = flip flip, volvemos al comportamiento original de flip que es de tipo (a -> b -> c) -> b -> a -> c

-- Las primeras 2 funciones no estan currificadas, el resto si lo estan
max2 :: (Ord a) => a -> a -> a
max2 x y | x >= y    = x
         | otherwise = y

normaVectorial :: (Floating a) => a -> a -> a
normaVectorial x y = sqrt (x^2 + y^2)
 -- Ejercicio 2
curry :: ((a, b) -> c) -> (a -> b -> c)
(curry f) x y = f (x, y) 

uncurry1 :: (a -> b -> c) -> ((a, b) -> c)
(uncurry1 f) (x, y) = f x y

-- El tipo de curryN seria curryN :: ((a1 -> a2 -> ... -> an -> b) -> (a1 -> (a2 -> ... -> (an -> b)))), tomando funciones que reciben un solo argumento de uno en uno
-- la implementacion de curryN seria como la de curry solamente qeu se aplicaria n veces, es decir, curryN f x1 x2 ... xn = f x1 x2 ... xn

--Ejericio 3

sumar' :: [Int] -> Int
sumar' = foldr (+) 0 --recursion implicita, agarro el resultado de la recursion y le sumo la cabeza de la lista, el cero corresponde al caso base

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x recursion-> if f x then x:recursion else  recursion) []--si cumple la condicion, lo agrego a la lista, si no, no lo agrego

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x recursion -> f x : recursion) []

elem :: Eq a => a -> [a] -> Bool
elem x = foldr (\elemetoLista valorAcumulado -> elemetoLista == x || valorAcumulado) False --el valor acumulado es los anteriores elementos de la lista, si ya pertenecia todo el resto es true, si no, sigue siendo false

concat :: [a] -> [a] -> [a]
concat xs ys = foldr (:) ys xs --agrego los elementos de xs a ys, va hacia el ultimo elemento de xs, hace (:) y sigue con el resto de la lista ys

mejorSegun:: (a -> a -> Bool) -> [a] -> a
mejorSegun f [x] = x
mejorSegun f (x:xs) = foldr(\x booleanoAcumulado -> if f x booleanoAcumulado then x else booleanoAcumulado) x xs

sumasParciales :: Num a => [a]-> [a]
sumasParciales [] = []
sumasParciales [x] = [x]
sumasParciales xs = foldr(\x listaAcumulada -> x : map (+x) listaAcumulada) [] xs --preguntar

--sumaAlt :: Num a => [a] -> a
--sumaAlt xs = foldr(-) 0 xs --resta los elementos de la lista, el cero es el caso base

sumaAlt2 :: Num a => [a] -> a
sumaAlt2 xs = foldr(-) 0 (reverse xs) --resta los elementos de la lista, el cero es el caso base

--Ejercicio 4
permutaciones :: [a]-> [[a]]
permutaciones [] = [[]]
permutaciones [x] = [[x]]
permutaciones xs = foldr (\x listaAcumulada -> concatMap(insertar x) listaAcumulada) [[]] xs
        where insertar :: a -> [a] -> [[a]]
              insertar x xs = [take n xs ++ [x] ++ drop n xs | n <- [0..length xs]]

partes :: [a] -> [[a]]
partes [] = [[]]
partes (x:xs) = partes xs ++ map (x:) (partes xs) 

prefijos :: [a] -> [[a]]
prefijos [] = [[]]
prefijos (x:xs) = [] : map (x:) (prefijos xs) -- el primer elemento es la lista vacia, luego le agrego el primer elemento a los prefijos de la lista restante, y asi sucesivamente
--prefijos xs = foldr (\x listaAcumulada -> [] : map (x:) listaAcumulada) [] xs --preguntar

sublistas :: [a] -> [[a]]
sublistas xs = filter (\x -> length x > 0) (partes xs) --preguntar

--Ejercicio 5

--La primera es de recursion estructural, usa las propiedades de la lista para hacer la recursion, en cambio la segunda no ya que usa cosas por fuera de las propiedades de la lista, como el null, ademas de estar escrita en notacion lambda
--elementosEnPosicionesPares :: [a]-> [a]
--elementosEnPosicionesPares [] = []
--elementosEnPosicionesPares (x:xs) = if null xs
                                     --then [x]
                                     --else x : elementosEnPosicionesPares (tail xs)
--entrelazar :: [a]-> [a]-> [a]
--entrelazar [] = id
--entrelazar (x:xs) = \ys-> if null ys
                          --then x : entrelazar xs []
                          --else x : head ys : entrelazar xs (tail ys)

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
--elementosEnPosicionesPares (_:xs) = foldr (\x lista []-> if null xs
                                     --then [x]
                                     --else x : elementosEnPosicionesPares (tail xs)) --chequear esto pero funciona
--elementosEnPosicionesPares (x:xs) = foldr(\y lista -> if [n| n <- [0..length xs], n `mod` 2 == 0] == [] then [x] else x : lista) [] xs 

-- recr :: (a-> [a]-> b-> b)-> b-> [a]-> b
 --recr _ z [] = z
 --recr f z (x : xs) = f x xs (recr f z xs)

-- Ejercico 6
sacarUna :: Eq a => a-> [a]-> [a]
sacarUna y = recr (\x xs rec -> if x == y then xs else x : rec) []

--foldr no permite cortar la recursion temprana ya que recorre y aplica la funcion a todos los elementos de la lista sin importar si ya se encontro el elemento o no, en cambio recr permite cortar la recursion temprana ya que se puede usar una condicion para salir de la recursion antes de llegar al final de la lista, ademas de que permite usar el resultado de la recursion en la funcion que se aplica a cada elemento de la lista, lo que no es posible con foldr.

insertarOrdenado :: Ord a => a-> [a]-> [a]
insertarOrdenado y = recr (\x xs rec -> if x <= y then x : rec else y : x : xs) []

--Ejercicio 7

mapPares :: (a -> b -> c) -> [(a,b)] -> [c]
mapPares f xs = map (uncurry1 f) xs 

armarPares :: [a] -> [b] -> [(a,b)]
armarPares [] _ = []
armarPares _ [] = []
armarPares (x:xs) (y:ys) = (x,y) : armarPares xs ys -- no lo hice como una lambda por que no queria usar la funcion zip

armarPares' :: [a] -> [b] -> [(a, b)]
armarPares' xs ys = map (\(x, y) -> (x, y)) (take (min (length xs) (length ys)) (zip xs ys)) --version lambda que no usa zip como parte de la definicion directa.

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f xs ys = foldr (\(x, y) recursion -> f x y : recursion) [] (cortarLista xs ys)
  where
    cortarLista (x:xs) (y:ys) = (x, y) : cortarLista xs ys
    cortarLista _ _ = []
-- en este caso cortarLista es una funcion auxiliar que crea las tuplas hasta la lista que tenga menor longitud, por requiere sabemos que tienen el mismo tamaño, por lo tanto, no genera un problema.
--mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c] con recursion explicita
--mapDoble _ [] [] = []
--mapDoble f (x:xs) (y:ys) = f x y : mapDoble f xs ys
--mapDoble _ _ _ = []  -- Caso cuando las listas no tienen la misma longitud
--Ejercicio 8
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat xs ys = zipWith (zipWith (+)) xs ys

trasponer :: [[Int]] -> [[Int]]
trasponer [] = []  -- Caso base: si la matriz está vacía, la traspuesta también lo es.
trasponer (xs:xss) = map head (xs:xss) : trasponer (map tail (xs:xss))

--Ejercicio 9
foldNat :: (Integer -> Integer) -> Integer -> Integer -> Integer
foldNat _ z 0 = z 
foldNat f z n = f  (foldNat f z (n-1)) 
--Como devuelve un valor fijo, no depende de g y el caso recursivo no puede usar las variables g ni xs, salvo en la expresion (g xs), es decir, es una recursion estructural.

potencia :: Integer -> Integer -> Integer
potencia x = foldNat (\ac -> ac * x) 1 

-- Ejercicio 10
genLista :: a-> (a-> a)-> Integer-> [a]
genLista x f 0 = [x] -- caso base, devuelve una lista con el elemento x
genLista x f n = x : genLista (f x) f (n-1) 

desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta x y = genLista x (+1) (y-x) 

--Ejercicio 11
-- preguntar si tengo que aplicar esas funciones en recursion estructural


--Ejercicio 12
data AB a = Nil | Bin (AB a) a (AB a)
foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cnill cbin nill = cnill
foldAB cnill cbin (Bin izq x der) = cbin (foldAB cnill cbin izq) x (foldAB cnill cbin der)

recAB :: b -> (b -> a -> b -> Arbol a -> Arbol a -> b) -> Arbol a -> b
recAB cnill cbin Nill = cnill
recAB cnill cbin (Bin izq x der) = cbin (recAB cnill cbin izq) x (recAB cnill cbin der) izq der

esNil :: AB a -> Bool
esNil arbol = case arbol of
    Nil -> True
    _   -> False

altura :: AB a -> Integer
altura arbol = foldAB 0 (\izq _ der -> 1 + max izq der) arbol

cantNodos :: AB a -> Integer
cantNodos arbol = foldAB 0 (\izq _ der -> 1 + izq + der) arbol

mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB f arbol = foldAB id (\izq x der -> if f x izq then if f x der then x else der else if f izq der then izq else der) arbol


--Apuntes de clase
-- la expresion map filter es [c -> Bool] -> [[c] -> [c]], es decir, recibe una funcion que devuelve un booleano y una lista y devuelve una lista,funcion que recibe una lista de predicados y devuelve una lista con las funciones que cumplen con ese 
-- map aplicado a filter podria servir para por ejemplo,map filter [esPar,esImpar] [1,2,3,4] = [2,4] ya que esPar y esImpar son funciones que devuelven un booleano y [1,2,3,4] es una lista de enteros
-- notacion lambda \x -> expresion en funcion de x 
-- se pueden escribir funciones de n parametros aunque en realidad tienen uno solo
-- Esquemas de recursion sobre listas
-- Recursion estructural: Decimos que es asi si el caso base devuelve un valor z fijo, no depende de g y el caso recursivo no puede usar las variables g ni xs, salvo en la expresion (g xs)
-- (++) []  = \ys -> ys -- caso base, es una funcion que devuelve ys
-- (++) (x:xs) = \ys -> x ((++) xs ys) -- caso recursivo, es una funcion que devuelve x y la concatenacion de xs con ys, es decir, la lista que se pasa como argumento
-- funcion foldr
-- para recursion estructural abstrae el Esquemas
-- tipo de foldr es (a -> b -> b)(caso recursivo) -> b(caso base) -> [a] -> b
-- en el caso recursivo combina la cabeza de la lista con el resultado de la recursion, en el caso base devuelve el valor inicial
-- foldr f z [] = z -- caso base, si la lista esta vacia, devuelve el valor inicial
-- foldr f z (x:xs) = f x (foldr f z xs) -- caso recursivo, combina la cabeza de la lista con el resultado de la recursion
-- toda recursion estructural es una instancia de foldr, pero no toda instancia de foldr es una recursion estructural, ya que foldr puede ser usado para listas infinitas y la recursion estructural no, ya que no se puede llegar al caso base
-- foldr es una funcion a aplicar, mas un caso base, mas una lista
-- recursion: la funcion aplicada a la cola de la lista, x concatenado con el resultado de la recursion, y ys es la lista que se pasa como argumento
-- reverse = foldr(x\ resultadoDeLaRecursion -> resultadoDeLaRecursion ++ [x]) []

-- Recursion primitiva:
-- el caso base devuelve un valor fijo, no depende de g y el caso recursivo no se puede llamar a si misma,no puede usar g, si puede usar la variable xs, permite referirse a la cola de la lista
-- todas las definiciones dadas por recursion estructural tambien estan dadas por recursion primitiva
-- hay definiciones dadas por recursion primitiva que no estan dadas por recursion estructural
-- trim :: String -> String
-- trim [] = []
-- trim (x:xs) = if x == ' ' then trim xs else x : xs  
-- esta funcion no es recursion estructural pero si primitiva 
-- trimm hecha con foldr
--trim = foldr(\x lista -> if x == ' ' then lista else x : (falata algo aca)) [] (si pongo lista en el else me borraria todos los espacios y no quiero eso)
recr :: (a -> [a]-> b -> b) -> b -> [a] -> b
recr f z [] = z 
recr f z (x:xs) = f x xs (recr f z xs) -- es la escritura de foldr
-- trim = recr(\x xs recursion -> if x == ' ' then recursion else x : xs) [] (es de la forma char -> string -> string -> string)
-- con recr se comporta como una funcion primitiva

-- Recursion iterativa:
-- g :: b -> [a] -> b (el primer b es un acumulador)
-- el caso base se devuelve el acumulador 
-- el caso recursivo invoca a (g ac' xs), donde ac' es el nuevo acumulador, y xs es la lista que se pasa como argumento
-- foldl :: (b -> a -> b)(funcion que calcula el nuevo acumulador) -> b -> [a] -> b
-- foldl f acumulador [] = acumulador
-- foldl f acumulador (x:xs) = foldl f (f acumulador x) xs (pongo foldl f por que es la funcion que se va a aplicar, y el nuevo acumulador es el resultado de aplicar f al acumulador y a la cabeza de la lista)
-- ejemplo pasaje binario a decimal
bin2dec:: [Int] -> Int
bin2dec = foldl(\acum x -> acum * 2 + x) 0 -- el acumulador es el resultado de la conversion, y el nuevo acumulador es el resultado de aplicar f al acumulador y a la cabeza de la lista, el cero es el caso base
-- recursion estructural == foldr
-- recursion primitiva == recr
-- recursion iterativa == foldl
-- los ejercicios para pensar de las diapos a veces entran en el final

--Tipos de datos algebraicos
-- tipos de datos enumerados tienen muchos constructores y son las unicas maneras de construirlos 
-- tipos producto tienen un solo constructor con muchos parametros 
-- un tipo puede tener muchos constructores con muchos parametros
-- algunos constructores pueden ser recursivos como el arbol binario o las listas
-- doble :: Nat -> Nat
-- doble zero = zero
-- doble (suc x) = suc(suc (doble x)) 
-- en Haskell las definiciones recursivas se interpretan de manera coinductiva en lugar de inductiva
-- los constructores base no reciben parametros de tipo T
-- los constructores recursivos reciben al menos un parametro de tipo T
-- ejemplo listas:
-- data [a] = []| a : [a]
productoCartesiano :: [a] -> [b] -> [(a,b)]
productoCartesiano [] ys = []
productoCartesiano (x:xs) ys = productoCartesiano xs ys ++ map (\y -> (x,y)) ys --preguntar por que funciona escribir productoCartesiano xs ys

--data AB a = nill | Bin (AB a) a (AB a) 
--inorder :: AB a -> [a]
--inorder nill = []
--inorder (Bin izq x der) = inorder izq ++ [x] ++ inorder der

--insertar :: Ord a => a -> AB a -> AB a
--insertar x nill = Bin nill x nill
--insertar x (Bin izq y der)  | x < y = Bin (insertar x izq) y der | x > y = Bin izq y (insertar x der) | otherwise = Bin izq y der

-- Esquemas de recursion sobre otras estructuras
-- Decimos que g esta dada por recursion estructural si:
-- Cada caso base se escribe combinando los parametros.
-- Cada caso recursivo:
-- No usa la funcion g.
-- No usa los parametros del constructor que son de tipo T.
-- Pero puede:
-- Hacer llamados recursivos sobre parametros de tipo T.
-- Usar los parametros del constructor que no son de tipo T.
-- constructores:
-- Nill :: AB a
-- Bin :: AB a -> a -> AB a -> AB a
-- va a haber una funcion combinadora por cada contructor
-- foldAB :: b -> (b(hacer recursion sobre el hijo izq) -> a -> b(hacer recursion sobre el hijo der) -> b) -> AB a -> b
-- foldAB cnill cbin nill = cnill
-- foldAB cnill cbin (Bin izq x der) = cbin (foldAB cnill cbin izq) x (foldAB cnill cbin der)
-- no puedo ver los nodos pero si puedo llamarlos recursivamente 

--Clase practica // presentacion del TP
-- foldr1 :: (a -> a -> a) -> [a] -> a no toma un caso base con la precondicion de que la lista no este vacia, como se devuelve el mismo tipo de la lista Foldr1 debe teneir el mismo tipo
-- una funcion es estructural cuando el argumento inductivo de la lista (la cola xs) es utilizada
-- subListaQueMasSuma :: [Int] -> Int
-- subListaQueMasSuma = recr (\x xs res -> if (sum . prefijoQueMasSuma)(x:xs) >= sum res then prefijoQueMasSuma (x:xs) else res) [] 
-- usa en esquema de recursion primitiva ya que necesitamos acceder a las subestructuras, en este caso xs 

-- Generacion Infinita
pares :: [(Int, Int)]
pares = [(x,y) | x <- [0..], y <- [0..]] -- lista infinita de pares (x,y) donde x e y son enteros no negativos, esto es unalista por comprension,x e y son los selectores de los elementos que van a ir en la tupla
-- tambien le puedo agregar condiciones, por ejemplo x + y == 2

pares2 :: [(Int, Int)]  
pares2 = [ p | k <- [0..], p <- paresQuesuman k]

paresQuesuman :: Int -> [(Int, Int)]
paresQuesuman k = [(i, k-i) | i <- [0..k]] 
-- otra idea armar las tuplas que suman k, por ejemplo (0,2), (1,1), (2,0) y asi sucesivamente

--data AEB a = Hoja a | Bin (AEB a) a (AEB a)
-- Estamos ante un tipo de dato inductivo con un constructor no recursivo y un constructor recursivo, el arbol bin llama al tipo de dato AEB

--foldAEB ::  (a -> b)(tomamos el a de la hoja, computamos algo y devolvemos el resultado b) -> (b (subarbol izquierdo)-> a(raiz) -> b(subarbol derecho) -> b)(funcion que trabaja sobre el arbol) -> AEB a(el arbol en si) -> b
--foldAEB cHoja cBin Hoja e = cHoja e -- caso base, si es una hoja, devuelve el resultado de aplicar la funcion a la hoja
--foldAEB cHoja cBin (bin izq x der) = cBin (foldAEB cHoja cBin izq) x (foldAEB cHoja cBin der)

-- foldAEB cHoja cBin t = case t of Hoja e -> cHoja e
-- Bin izq x der -> cBin (foldAEB cHoja cBin izq) x (foldAEB cHoja cBin der)


--ramas :: AEB a -> [[a]]
--ramas t = foldAEB (\e -> [[e]]) (\izq x der -> map (x:) (izq ++  der)) t

-- altura :: AEB a -> Int
-- altura  = foldAEB (const 1) (\recizq _ recder -> 1 + max recizq recder) 

-- take' :: [a] -> Int -> [a]
-- take' [] _ = const []
-- take' (x:xs) = \n -> if n == 0 then [] else x : take' xs (n-1) 
-- take' xs n = foldr (\x rec n -> if n == 0 then [] else x : rec (n-1)) const []

-- Insertar abb de la practica es recursion primitiva por que necesitamos las subestructuras como las ramas del arbol, tenes el arbol, no haces recursion sobre el arbol
-- este es el ejercicio 11 de la practica
-- data Polinomio a = x | Constante a | Suma (Polinomio a) (Polinomio a) | Producto (Polinomio a) (Polinomio a) 
-- e p = case p of 
-- x -> e
-- Constante y -> y
-- Suma p1 p2 -> Suma (evaluar e p1) + (evaluar e p2)
-- producto p1 p2 -> Producto (evaluar e p1) * (evaluar e p2)

-- foldPolinomio :: b -> 
--                    (a -> b) ->
--                    (b -> b -> b) -> 
--                    (b -> b -> b) -> 
--                    Polinomio a -> 
--                    b 
-- la ventaja del case es que se escribe menos codigo
-- foldPolinomio cX cCte fsuma gprod p = case p of
-- x -> cX
-- Cte y -> cCte y
-- Suma p1 p2 -> (fsuma (rec p1) (rec p2))


-- where 
-- rec = foldPolinomio cX cCte fsuma gprod 

-- evaluar :: Num a => a -> Polinomio a -> a
-- evaluar e = foldPolinomio e id (\r1 r2 -> r1 + r2) (\r1 r2 -> r1 * r2)
-- foldPolinomio e id (+) (*) otra opcion mas corta ya que en esos parametros van funciones


--Ejercicio 15
-- data RoseTree a = Rose a(primera variable) [RoseTree a(posibles rosetrees que son hijos de la raiz)] 
-- Esquema de recursion estructural
-- foldRose :: (a -> [b]) -> b) -> RoseTree a -> b
-- foldRose f (Rose x xs) = f x (map (foldRose f) xs) le pasamos map por que rose usa una lista, el resto de los rose estan en xs


-- AlturaRose :: RoseTree a -> Int
-- AlturaRose = foldRose (\_ xs -> 1 + maximum xs) 
-- AlturaRose = foldRose (\_ rec -> if null rec then 1 else 1 + maximum rec) \\ otra forma de hacerlo, maximum toma el maximo de una lista, si la lista es vacia devuelve 0  y usamos maximum para los casos en los que tiene dos hijos y que le entrarian dos listas, va a ir armando listas de uno que son casos de la recursion, es decir va a armar una lista con [1(para el ultimo nodo),2 (para el penultimo),3(para el antepenultimo)] y va a devolver el maximo de esa lista, es decir, la altura del arbol

-- type conj a = (a -> Bool)
-- vacio :: conj a 
-- vacio = const False

-- agregar :: Eq a => a -> conj a -> conj a
-- agregar x c = \y -> y == x || c y -- esta funcion no guarda nada, decide si deberia agregarse o no, por ejemplo si hago parsCon5 = agregar 5 even y despues hago paresCon5 5, eso es igual a true

-- union :: Eq a => conj a -> conj a -> conj a
-- union c1 c2 = \x -> c1 x || c2 x


