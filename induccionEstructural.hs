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

