-- reverse . reverse = id
-- Como probariamos esto? la idea es mostrar que ciertas expresiones son equivalentes, computar una puede ser mas facil\eficiente que computar la otra, pero en el fondo son equivalentes.
-- Sirve para hacer optimizaciones y para ver que dos programas son iguales, otro metodo para ver la correctitud de un programa


-- Marco teorico:
-- trabajamos con estructuras de datos finitas, tecnicamente con tipos de datos inductivos
-- trabajamos sobre funciones totales, es decir, las ecuaciones cubren todos los casos posibles y la recursion siempre termina
-- el programa no depende del orden de las ecuaciones


--Igualdades por definicion:

-- Principio de reemplazo 
-- sea e1=e2 una ecuacion incluida en el programa, las siguientes operaciones conservan la igualdad
-- 1) Sustitucion de e1 por e2 en cualquier parte del programa
-- 2) Sustitucion de e2 por e1 en cualquier parte del programa
-- si solo lo reemplazamos decimos que la igualdad vale por definicion
-- Hay que aclarar en cada caso que principio se aplica
-- las ecuaciones se pueden usar en ambos sentidos ya que no estamos computando
-- Ver ejemplo de las diapos
-- No alcanza para todos los casos

-- Induccion sobre booleanos
-- not True = False nt
-- not False = True nf
-- not (not x) = x
-- a la expresion anterior no se le puede aplicar ninguna ecuacion
-- para esto separamos en casos
-- si vale una propiedad para true y vale para false, vale para cualquier booleano
-- es decir, hay que probar que not (not true) = true y not (not false) = false
-- por las ecucaciones nt y nf esto se cumple
-- fst p = snd (swap p), donde fst(x,_) = x, snd(_,y) = y y swap(x,y) = (y,x)

-- Principio de induccion sobre pares:
-- si vale la propiedad para (x,y), luego vale para todo par (a,b)


-- Principio de induccion sobre natrurales:
-- 1) Base: la propiedad vale para 0
-- 2) Induccion: si vale para n, vale para n+1
-- 3) Conclusión: la propiedad vale para todo natural n

-- Induccion estructural:
-- Si tengo una propiedad que vale para expresiones de tipo t tal que p vale para todos los constructores de t y vale sobre los constructores recursivos asumiendo como hipotesis inductiva que vale para los parametros de tipo t
-- principio de induccion sobre listas:
--  1) Base: la propiedad vale para la lista vacia
--  2) Induccion: si vale para xs, vale para (x:xs)
--  3) Conclusión: la propiedad vale para toda lista xs
-- En el ejemplo sobre arboles asumo que la HI vale para los subarboles, y la base es que vale para el arbol vacio


-- foldr y foldl
-- propiedad: foldr f z xs = foldl (flip f) z (reverse xs)
-- podemos hacer induccion sobre xs y depende de una f y z fijas, seria literalmente la expresion
-- por definicion de foldr aplicamos foldr
-- luego viene la HI 
-- aplicas la definicion de flip
-- foldl g z (xs ++ [x]) = g (foldl g z xs) x lema que vale, por eso podemos sacar el flip afuera


-- Lemas de generacion:
-- lema de generacion para pares: si p :: (a,b), entonces existe x e y de tipo a y b tal que p =(x,y)
-- lema de generacion para sumas: si e :: Either a b, entonces o bien existe x de tipo a tal que e = Left x o existe y de tipo b tal que e = Right y 
-- Los lemas de generacion se pueden demostrar pero no hacen falta, se toman como presentados y son conocidos

-- Extensionalidad:
-- punto de vista intensional vs punto de vista extensional
-- punto de vista intensional (va con “s”),dos valores son iguales si est ́an construidos de la misma manera.
-- punto de vista extensional dos valores son iguales si son indistinguibles al observarlos


-- principio de extensionalidad funcional: 
-- Si (∀x ::a.f x = g x) entonces f = g
-- es decir, si dos funciones son iguales para todos los argumentos, entonces son iguales


-- Supongamos que logre demostrar que e1 = e2, que nos asegura eso?
-- *que e1 = e2 no signifa que den el mismo dato, ejemplo quicksort e insertion sort, son codigos distintos que representan la misma funcion matematica*
-- Nos refermos con observacion a una funcion total para la cual le pasamos e1 y el resutado debe ser igual al de aplicarle la observacion a e2
-- si dos expresiones son observacionalmente iguales, entonces son iguales
-- si dos expresiones son iguales deberian ser indistinguibles al observarlas    

-- Isomorfismos de tipos:
-- misma informacion, escrita de distinta manera
-- es decir, dos tipos son isomorfos si existe una funcion que los transforma entre si y viceversa
-- no se pierde informacion, solo la estoy describiendo de otra manera
-- Decimos que dos tipos de datos A y B son isomorfos si:
-- 1. Hay una funci ́on f ::A -> B total.
-- 2. Hay una funci ́on g ::B -> A total.
-- 3. Se puede demostrar que g . f = id ::A -> A.
-- 4. Se puede demostrar que f . g = id ::B -> B.
-- Escribimos A ≃B para indicar que A y B son isomorfos
-- ejemplo de curry uncurry
-- por extensionalidad tenemos que sea f :: (a,b) -> c estos serian todos los argumentos que queremos ver 
-- (uncurry.curry) f = id f (def de composicion)
-- por extensionalidad tenemos que sea p :: (a,b), quiero ver que uncurry (curry f) p = id f p :: c
-- por induccion sobre pares, basta ver que si x :: a, y :: b, entonces uncurry (curry f) (x,y) = id f (x,y)
-- por definicion de uncurry, curry f x y = f (x,y)
-- por definicion de curry, f (x,y) 
-- por definicion de identidad, id f (x,y)
-- que sean isomorfos no significa que sean iguales, quiere decir que existe una funcion que los transforma entre si y viceversa


-- Casos de estudio
-- demostremos que ceros.reverse = ceros. reverse
-- por extensionalidad, sea xs :: [a], veamos que (ceros.reverse) xs = (reverse.ceros) xs
-- por definicion de composicion dos veces tenemos que: ceros (reverse xs) = reverse (ceros xs)
-- por induccion en xs con P(xs) = [ceros (reverse xs) = reverse (ceros xs)]
-- caso base p([])
-- paso inductivo quiero ver que para todo x :: a y xs :: [a] P(xs) implica p(x:xs)
-- sean x::a y xs::[a], suponemos p(xs) 
-- vemos que p(x:xs): ceros(reverse (x:xs)) = ceros (reverse xs ++ [x]) por R1
-- = ceros(reverse xs) ++ ceros [x] por lema
-- = ceros(reverse xs ++ [0]) Z1 Z2
-- = reverse(ceros xs ++ [0]) HI
-- = reverse (0:ceros xs) R1
-- = reverse(ceros(x:xs)) Z1
-- lema: ceros(xs ++ ys) = ceros xs ++ ceros ys


-- Como probamos que doble 2 = cuadrado 2?

-- doble:: integer -> integer
-- doble x = 2*x

-- cuadrado:: integer -> integer
-- cuadrado x = x*x

-- doble 2 =  2* 2 = cuadrado 2 

-- qvq cury.uncurry = id
-- curry:: ((a,b) -> c) -> (a -> b -> c)
-- curry f = \x y -> f (x,y)

-- uncurry:: (a -> b -> c) -> ((a,b) -> c)
-- uncurry f = (\(x,y) -> f x y)

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (f.g) x = f (g x)

-- id:: a -> a
-- id x = x

--por extensionalidad, dadas f,g probar que f = g si f x = g x para todo x
{-
F :: a-> b . G :: a-> b . Y :: b . Z :: a .
F = G
F = x-> Y 
x::a.F x = G x
x::a.F x = Y
(x-> Y) Z = Y reemplazando x por Z (por beta)
x-> F x = F (por eta)
F, G, Y y Z pueden ser expresiones complejas, siempre que la variable x no aparezca libre en F,
G, ni Z (mas detalles cuando veamos Calculo Lambda)-}

--para demostrar que curry.uncurry = id hacemos:
-- (curry.uncurry) f = id f (para todo f :: (a,b) -> c)
-- (curry.uncurry) f = curry (uncurry f) (por composicion)
-- curry (\(x,y) -> f x y) (por definicion de uncurry)
-- = \x' y' -> (\(x,y) -> f x y) (x',y') (por definicion de curry)
-- = \x' y' -> f x' y' (por beta)
-- = \x' -> f x' = f (por eta)
-- = id f (por definicion de identidad)

-- prod :: Either Int (Int, Int)-> Either Int (Int, Int)-> Either Int (Int, Int)
-- prod (Left x) (Left y) = Left (x*y)
-- prod (Left x) (Right (y,z)) = Right (x*y, x*z)
-- prod (Right (y,z)) (Left x) = Right (x*y, x*z)
-- prod (Right (w,x)) (Right (y,z)) = left (w*y + x*z)
-- podemos probar que prod p q = prod q p
-- por lema de generacion de sumas puede ser left n con n :: int o right (x,y) con x,y :: int (par1)
-- que puede ser left m con m :: int o right (z,w) con z,w :: int (par2)
-- por lema de generacion de pares tenemos que par1 = (w,x) con w :: int y x :: int
-- o par2 = (y,z) con y :: int y z :: int
-- Caso p = Left n, q = Left m
-- prod p q = left(n*m) = left (m*n) = prod q p (usamos p0 y la definicion de int)
-- Caso p = Left n, q = Right (y,z)
-- prod p q = right (n*y, n*z) = right (y*n , z*n) = prod q p (usamos p1, p2 y la definicion de int dos veces)
-- Caso p = Right (w,x), q = Left m
-- usamos p2 , luego la conmutatividad dos veces y por ultimo p1
-- caso p = Right (w,x), q = Right (y,z)
-- prod p q = left (w*y + x*z) = left (y*w + z*x) = prod q p (usamos p3 al principio, al final y la definicion de int dos veces)


-- Funciones como estructuras de datos:
-- interseccion d (diferencia c d) = vacio (propiedad a demostrar)
-- interseccion d = interseccion d (\e -> c e && not (d e)) (por definicion de diferencia)
-- = \h -> d h &&(ch && not (c h)) (por definicion de interseccion y beta)
-- = \h -> d h && not (d h) && c h (por conmutatividad de bool)
-- = \h -> false = vacio (por definicion de vacio)

-- por predicado unario se refiera a sacar uno de los parametros del constructor y hago induuccion sobre ese , se saca un para todo y solo uno
{-
Veamos que estas dos definiciones de length son equivalentes:
length1 :: [a]-> Int
length1 [] = 0
length1 (_:xs) = 1 + length1 xs
length2 :: [a]-> Int
length2 = foldr (\_ res-> 1 + res) 0
Recordemos:
foldr :: (a-> b-> b)-> b-> [a]-> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)}

-- quiero ver que length1 xs = length2 xs para todo xs :: [a]
-- length2 = foldr (\_ res-> 1 + res) 0 (por l2)
-- por extensionalidad quiero ver que para todo xs :: [a] length1 xs = foldr (\_ res-> 1 + res) 0 xs
-- por induccion en xs con P(xs) = length1 xs = foldr (\_ res-> 1 + res) 0 xs
-- caso base P([])
-- foldr (\_ res-> 1 + res) 0 [] = 0 (ya que le estamos aplicando foldr a la lista vacia, que es el caso base de foldr)
-- = length1 [] (por caso base de length1)
-- caso inductivo xs = y : ys (una lista no puede ser si misma mas un elemento, por eso lo cambiamos a y:ys)
-- HI : foldr(\_ res-> 1 + res) 0 ys = length1 ys
-- HI = P(YS) y TI = P(y:ys)
-- foldr (\_ res-> 1 + res) 0 (y:ys) = length1 (y:ys) (por HI)
-- = (\_ res-> 1 + res) y (foldr (\_ res-> 1 + res) 0 ys) (por definicion de foldr y beta dos veces)*
-- 1 + foldr (\_ res-> 1 + res) 0 ys = 1 + length1 ys = length1 (y:ys) (por HI y por l1)
-- *(\res-> 1 + res) (foldr f 0 ys) (dos betas)

-- diapo 36
-- Probamos por induccion en ys
-- caso base ys = []
-- P([]) = elem e [] => e <= maximum []
-- elem e [] = False (por definicion de elem, por definicion de bool)
-- caso ys = x:xs
-- HI = P(xs)= Ord a⇒∀e::a . (elem e (x:xs) ⇒ e ≤ maximum (x:xs))
-- = e == x || elem e xs => e <= maximum (x:xs) (por e1)********
-- por lema de generacion de bool
-- = e == x es true o false
-- por lema de generacion xs es [] o z:zs
-- caso e == x es true, xs es []
-- ***** == {bool} true => e <= maximum (x:[]) = e <= maximum (x:[]) = e <= x = true (definicion de bool, m0 y ord e == x)
-- caso e == x es false, xs es []
-- ***** = elem e [] => e <= maximum [] vale por caso base
-- caso e == x es true, xs es z:zs
-- ***** == {bool} e <= maximum (x:z:zs) = {por m1} e <= if x < maximum (z:zs) then maximum (z:zs) else x 
-- por lema de generacion de bool x < maximum (z:zs) es true o false
-- caso true 
-- por ord e == x < maximum (z:zs) => e <= maximum (z:zs) {por m3} = e <= if x < maximum (z:zs) then maximum (z:zs) else x 
-- caao false
-- e <= por ord e == x es false, xs es z:zs
-- = {bool} elem e (z:zs) => e <= maximum (x:z:xs) = {m1} elem e (z:zs) => e <= if x < maximum (z:zs) then maximum (z:zs) else x
-- lema de generacion de bool 
-- caso true elem e (z:zs) => e <= maximum (z:zs) vale por H
-- caso false elem e (z:zs) => e <= x 
-- lema de generacion de bool
-- false => es verdadera 
-- caso true qvq e <= x por hi bool e <+= maximum (z:zs) < x tal que e <= x


-- lema mas general ya que no puedo usar la hi, esta misma usa la lista vacia pero en la recursion deja de ser vacia, es x:[]
-- lema para todo ac :: [a] y para todo ys :: [a] : length(fold(flip(:))ac ys) = length ys + length ac
-- P(ys) = para todo ac :: [a] : length(fold(flip(:))ac ys) = length ys + length ac (no hay que poner el para todo de ys)
-- caso base ys = []
-- P([]) = length(fold(flip(:))ac []) = length [] + length ac = length ac (por definicion de fold y de flip)
-- terminar caso ys = x:xs

-- diapo 45
-- por extensionalidad vamos a probar que para todo xs :: [a] take' xs = fliptake xs
-- por extensionalidad
-- para todo xs :: [a], para todo n int, take'xs n = flip take xs n
-- por induccion en xs
-- p(xs) = para todo n :: int take' xs n = flip take xs n
-- caso base xs = []
-- take' [] n = [] 
-- flip take [] n = foldr (\x rec....)(const []) [] n = const [] n = []
-- caso inductivo xs = y:ys
-- hi = p(xs) = take' (y:ys) n = if n == 0 then [] else y : take' ys (n-1) 
-- fliptake(y:ys) n = foldr (\x rec -> if n == 0 then [] else y : rec (n-1)) [] (y:ys) n = f y (foldr f [] ys) n = {por beta} 
-- (\rec n -> if n == 0 then [] else y : rec (n-1)) y (foldr f (const []) ys) n = {por beta} = if n == 0 then [] else y : foldr f (const []) ys (n-1) = if n == 0 then [] else y : flip take ys (n-1) = {por hi} = if  n == 0 then [] else y : take' ys (n-1) 