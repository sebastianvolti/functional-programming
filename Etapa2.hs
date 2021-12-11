module Etapa2 where

import Data.Char (isSpace)
import Debug.Trace (trace, traceShow, traceStack)
import Tipos
import Etapa1

--- El programa leeMOE lee una lista hasta encontrar una marca que no
--- esté protegida por un ESCAPE. Si no encuentra
--- marcas, devuelve Nothing en el segundo argumento y la lista vacía en el
--- tercero. Si el ESCAPE es el ultimo elemento, devuelve Nothing.
--- TODO

leeMOE :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe ([a], Maybe a, [a])
leeMOE m e [] = traceStack ("leeMOE m e []") Just ([], Nothing, [])
leeMOE m e [x]
   | e x       =traceStack ("leeMOE m e [] e x") Nothing
   | m x       =traceStack ("leeMOE m e [] m x") Just ([ ],  Just x, [])
   | otherwise =traceStack ("leeMOE m e [] otherwise") Just ([x], Nothing, [])
leeMOE m e (x:y:xs)
   | (m x)                    =traceStack ("leeMOE m e x:y:xs mx") Just ([],  Just x, [y]++xs)
   | (e x == False) && (m y)  =traceStack ("leeMOE m e x:y:xs e x false") Just ([x], Just y, xs)
   | (e y)                    = do (ws, z, zs) <- traceStack ("leeMOE m e x:y:xs ey") leeMOE m e ([y]++xs)
                                   return (x:ws, z, zs)
   | otherwise                = do (ws, z, zs) <- traceStack ("leeMOE m e x:y:xs oth")leeMOE m e xs
                                   return (x:y:ws, z, zs)

--- El programa leeMXE lee una lista hasta encontrar una marca que no
--- esté protegida por un ESCAPE. Si no encuentra
--- marcas, devuelve Nothing. Si el ESCAPE es el ultimo elemento,
--- devuelve Nothing.
--- TODO

leeMXE :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe ([a], a, [a])
leeMXE m e xs = case leeMOE m e xs of Nothing ->  Nothing
                                      Just (lista ,elem, finlista) -> (leeMXEaux (lista ,elem, finlista))

leeMXEaux :: ([a], Maybe a, [a]) -> Maybe ([a], a, [a])
leeMXEaux (lista ,elem, finlista) =  case elem of Nothing ->Nothing
                                                  Just x  ->Just (lista,x,finlista)


--- El programa readFV recibe un String, y consume del mismo
--- una palabra que representa un valor booleano de acuerdo
--- a la gramatica de GIFT*.
--- TODO

readFV :: String -> Maybe (Bool, String)
readFV str
   | elem ppalabra wTrue =traceShow(str) Just(True,tail (trimmbne rString))
   | elem ppalabra wFalse =traceShow(str) Just(False,tail (trimmbne rString))
   | otherwise =traceShow(str) Nothing
  where wTrue  = [ "VERDADERO", "VERDAD", "V", "TRUE", "T" ]
        wFalse = [ "FALSO", "FALSE", "F" ]
        (ppalabra,rString) = primPalabra "" (trimmbne str)

primPalabra :: String -> String -> (String, String)
primPalabra prefijo "" = (prefijo, "")
primPalabra prefijo all@(x:xs)
    | elem x finpalabra = (prefijo, all)
    | otherwise         = primPalabra (prefijo ++ [x]) xs
    where finpalabra = " \n}"

getSeq :: ( a -> Maybe (b, a) ) -> a -> Maybe ([b], a)
getSeq f xs =
  case (f xs) of
       Nothing       -> Just ([], xs)
       Just (fx, zs) -> do (bs, a) <- getSeq f zs
                           return (fx:bs, a)



---- str2qas consume todo un string y lo convierte en [ QA ]
---- Devuelve Nothing si no se puede consumir toda la entrada.

str2qas :: String -> Maybe [ QA ]
str2qas xs = do (qs, zs) <- getSeq str2qa (traceShow(xs) xs)
                if (zs /= "") then Nothing else Just qs

---- Un QAs puede comenzar con una respuesta (marcada { ... }) o con
---- una pregunta (sin  marcas).
--- TODO


str2qa :: String -> Maybe (QA, String)
str2qa [    ]    = Nothing
str2qa (' ':xs)= do (q, sx) <-str2q (' ':xs)
                    return (Q q, sx)
str2qa (x:xs)=
    case  str2a (x:xs) of
          Nothing -> auxstr (x:xs)
          Just(a, sx) -> Just(A a, sx)      

auxstr:: String -> Maybe (QA, String)     
auxstr (x:xs) = case str2q (x:xs) of
                    Just ([],x:xs) -> Nothing
                    otherwise -> do (q, sx) <-str2q (x:xs) -- preguntar si str2q da just(listavacia,xs), y retornar nothing
                                    return (Q q, sx)
          

---- str2q procesa una Pregunta.

str2q :: String -> Maybe ( Pregunta  , String )
str2q = getSeq getFragmento 

---- Analizador de Fragmento
---- No se aceptan caracteres especiales sin escape dentro de un fragmento.
---- Los mismos son: {, }, =, ~
--- TODO

getFragmento :: String -> Maybe (Fragmento, String)
getFragmento      ""  = Nothing
{-getFragmento      "\n"  = Nothing-}
getFragmento ( x :xs)
    | x `elem` ['{', '}', '=', '~'] =  Nothing
    | x == '$'                      = trace("getFragmento elem")str2Math       xs
    | x == '`'                      = trace("getFragmento `")traceShow(xs)str2Code          xs
    | otherwise                     = trace("getFragmento oth")traceShow(x:xs)str2Txt           (x:xs)

--- Los MATH contienen cualquier caracter con escape que no sea $
str2Math :: String -> Maybe ( Fragmento , String )
str2Math str = do (math, _ , zs) <- trace("str2Math do") traceShow(str) leeMXE marca escape str
                  return ( MATH math , zs )
         where marca x = x == '$'

--- Los CODE contienen cualquier caracter con escape que no sea `
--- TODO
str2Code :: String -> Maybe ( Fragmento , String )
str2Code str = do (code, _ , zs) <- trace("str2Code do") traceShow(str) leeMXE marca escape str
                  return ( CODE code , zs )
         where marca x = x == '`'

--- Los TXT en Q contienen cualquier caracter con escape que no sea ` $ {
--- TODO
str2Txt :: String -> Maybe ( Fragmento , String )
str2Txt "\n" = Just(TXT "\n", "")
str2Txt str = do (txt, segunda) <- trace("str2Txt")traceShow(str)leeMOE2 (leeMOE marca escape ( str))
                 return ( TXT  txt, segunda )
        where marca x = x `elem` ['`','$','{','}','~','=']

leeMOE2 :: Maybe ([a], Maybe a, [a]) -> Maybe([a], [a])
leeMOE2 x = case x of Nothing -> Nothing
                      Just (fst, mrk, tl) -> case mrk of Nothing -> Just(fst, [])
                                                         Just(marc) -> Just(fst, marc:tl)


---- str2a procesa una respuesta.
--- TODO

str2a :: String -> Maybe ( Respuesta , String )
str2a    ""  = Nothing
str2a xxs@(x:xs)
  | isSpace x      = str2a xs
  | x == '{'       = str2a xs
  | x == '='       = do (b, ys)     <- trace("str2a=") leerOpciones xxs
                        return (MO b, ys)
  | x == '~'       = do (b, ys)     <- trace("str2a~") leerOpciones xxs
                        return (MO b, ys)
  | x == '}'       = Just(ESSAY, xs)
  | otherwise      = do (b, ys)     <-  trace("str2a oth")readFV xxs
                        return (FV b, ys)

leerOpciones :: String -> Maybe ( [Opcion] , String )
leerOpciones    ""  = Nothing
leerOpciones xxs@(x:xs)
  | elem x " \n"      = leerOpciones xs
  | x == '='       = do (b, ys)     <- leerOpcion xxs
                        (optLst, yys) <- leerOpciones ys
                        return (b:optLst,yys)
  | x == '~'       = do (b, ys)     <- leerOpcion xxs
                        (optLst, yys) <- leerOpciones ys
                        return (b:optLst,yys)
  | x == '}'       = Just([], xs)
  | otherwise      = Nothing
--- TODO
leerOpcion :: String -> Maybe ( Opcion , String )
leerOpcion ('}':xs) = Nothing
leerOpcion (' ':xs) = leerOpcion xs
leerOpcion ('=':xs) = do (op, zs) <-getSeq getFragmento xs
                         return (OK op , zs)
leerOpcion ('~':xs) = do (op, zs) <-getSeq getFragmento xs
                         return (NOK op ,zs)


instance CCuerpo QA where
   getCuerpo xs = do (ys, zs) <- getCuerpo xs
                     qas <- str2qas (ys::Cuerpo Char)
                     return (qas, zs)

--- Marca para detectar el escape

escape = (== '\\')
trimmbne :: [Char] -> [Char]
trimmbne [] = []
trimmbne (x:xs)
     | x `elem` prohibidos    = trimmbne xs
     | x `elem` marca         = (x:xs)
     | otherwise    = x : trimmbne xs
   where prohibidos = " "
         marca = "}"