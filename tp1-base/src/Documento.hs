module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio


foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc cVacio cTexto cLinea d = case d of
          Vacio -> cVacio
          Texto s d -> cTexto s (rec d)
          Linea n d -> cLinea n (rec d)
          where rec = foldDoc cVacio cTexto cLinea

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.


infixr 6 <+>


(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 auxiliarCasoTexto Linea d1 


auxiliarCasoTexto :: String -> Doc -> Doc
auxiliarCasoTexto str1 recD2 = case recD2 of 
  Texto str2 d2' -> Texto (str1 ++ str2) d2'
  _ -> Texto str1 recD2 


{-
INVARIANTES DE TEXTO
-s no debe ser el string vacıo. Se mantiene porque los parametros d1 y d2 cumplen el invariante y en la funcion auxiliarCasoTexto estamos concatenando dos strings no vacios

-s no debe contener saltos de linea. Se mantiene porque los parametros d1 y d2 cumplen el invariante, entonces los strings que se usan para generar la concatenacion no tienen saltos de linea

-d debe ser Vacio o Linea i d’. Se mantiene porque los parametros d1 y d2 cumplen el invariante y solo se utiliza la funcion Texto en caso de que d2 sea linea vacia. En caso de que d2 provenga de texto se concatena el string para luego utilizar texto sobre un documento que sea vacio o linea (debido a que d2 ya cumplia el invariante)

INVARIANTES DE LINEA
-i >= 0. Se mantiene porque los parametros d1 y d2 cumplen el invariante y solo reutilizamos los valores de linea de d1 y d2 para el resultado, por lo que en el resultado si hay un linea i, i va a ser mayor a 0.

-}
 
-- foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b

indentar :: Int -> Doc -> Doc
indentar i | i <= 0 = error "La cantidad de espacio debe ser positiva"
           | otherwise = foldDoc Vacio Texto (\n -> Linea (i + n))

{-
INVARIANTES DE TEXTO
 el documento era una instancia que cumplia los invariantes y no se modifican los parametros pasados a Texto. 


INVARIANTES DE LINEA
-i >= 0. Se mantiene porque en las instancias de Linea ya tenia un numero mayor o igual a 0 y luego solo le sumo otro numero mayor a 0. por lo que el resultado es un numero de positivo.
-}


mostrar :: Doc -> String
mostrar = foldDoc "" (++) (\n  -> (++) ("\n" ++  replicate n ' '))

-- | Función dada que imprime un documento en pantalla
-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
