{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use any" #-}
module PPON where

import Documento
import Control.Arrow (Arrow(second))

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico (ObjetoPP _) = False
pponAtomico _ = True

-- foldPPON :: (String -> b) -> (Int -> b) -> ([(String, b)] -> b) -> PPON -> b
-- foldPPON cTexto cInt cObjeto p = case p of 
--                 TextoPP s -> cTexto s
--                 IntPP n -> cInt n
--                 ObjetoPP [(str, x)] -> cObjeto  map (\(str, ppon) ->  (str, rec ppon))
--                 where rec = foldPPON cTexto cInt cObjeto


-- recPPON :: (String -> b) -> (Int -> b) ->  ([(String, PPON)] -> b -> b) -> PPON -> b
-- recPPON :: cTexto cInt cObjeto p = case p of
--                 TextoPP s -> cTexto s
--                 IntPP n -> cInt n
--                 ObjetoPP [(str, ppon)] recPPON -> cObjeto [(str, ppon)] rec 
--                 where rec = recPPON cTexto cInt cObjeto

-- pponObjetoSimple :: PPON -> Bool
-- pponObjetoSimple p = recPPON (const True) (const True) (\(str, ppon) recPPON  -> case ppon of 
--                                                         ObjetoPP _ -> False
--

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple p = case p of
  IntPP _ -> False
  TextoPP _ -> False
  ObjetoPP l -> null (filter (not . pponAtomico . snd) l)

pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams = ObjetoPP [("0", pericles), ("1", merlina)]

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z [] = z
-- foldr f z (x : xs) = f x (foldr f z xs)


intercalar :: Doc -> [Doc] -> Doc
intercalar _ [] = vacio 
intercalar s (x:xs) = x <+> foldr (\d recD-> s <+> d <+> recD) vacio xs

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"


-- foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b

aplanar :: Doc -> Doc
aplanar = foldDoc vacio (\str recD -> texto str <+> recD) (\n recD -> texto " " <+> recD)


--   | ObjetoPP [(String, PPON)]

pponADoc :: PPON -> Doc
pponADoc p = case p of
  IntPP n -> texto (show n)
  TextoPP str -> texto $ show str
  ObjetoPP l -> if pponObjetoSimple p then aplanar (intercalar vacio (map (funcionComplicada) l)) else 
    entreLlaves (map (funcionComplicada) l)
  

--funcionComplicada :: (String, PPON) -> Doc
--funcionComplicada (str,ppon) = if pponObjetoSimple ppon 
--                               then texto  ("{ " ++ (show str) ++ ": ") <+> pponADoc ppon
--                               else texto ((show str) ++ ": ") <+> entreLlaves [pponADoc ppon]

funcionComplicada :: (String, PPON) -> Doc
funcionComplicada (str,ppon) | pponAtomico ppon = texto ((show str) ++ ": ") <+> pponADoc ppon
                             | otherwise = texto ((show str) ++ ": ") <+> aplanar (entreLlaves [pponADoc ppon])