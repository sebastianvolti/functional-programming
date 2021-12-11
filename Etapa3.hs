module Etapa3 where
import Debug.Trace (trace, traceShow, traceStack)
import Tipos
import Data.Char (isSpace)

mo2short  :: Cuestionario QA -> Cuestionario QA
mo2short [    ]     =  []
mo2short (x:xs)     =  [(getCuerpoMO x)] ++ mo2short xs

sortMO    :: Cuestionario QA -> Cuestionario QA
sortMO [    ]     =  []
sortMO (x:xs)     =  [(getCuerpoSort x)] ++ sortMO xs

trim      :: Cuestionario QA -> Cuestionario QA
trim [    ]     =  []
trim (x:xs)     =  [(getCuerpoTrim x)] ++ trim xs

nodupMO   :: Cuestionario QA -> Cuestionario QA
nodupMO [    ]  =   []
nodupMO (x:xs) = (Ejercicio c1 n nwlst c2):nodupMO xs
  where Ejercicio c1 n lst c2 = x
        nwlst = nodupMOQA lst



filtroFV  :: Cuestionario QA -> Cuestionario QA
filtroFV (qa) 
   | cantv > 1 && cantf < 1 = trace("fin quitar")traceShow(show qa)quitarFV qa
   | cantv < 1 && cantf > 1 = trace("fin quitar")traceShow(show qa)quitarFV qa
   | otherwise = trace("filtroFV oth")traceShow(show qa)qa
  where
    cantv =traceShow(contar True qa) contar True qa
    cantf =traceShow(contar False qa) contar False qa


transformaciones = mo2short . sortMO . nodupMO . filtroFV . trim


-- Funciones Auxiliares trim

getCuerpoTrim :: Ejercicio QA -> Ejercicio QA
getCuerpoTrim (Ejercicio a b c d)  = (Ejercicio a b (listaQA c) d)

listaQA :: Cuerpo QA -> Cuerpo QA  
listaQA [    ]     =  []
listaQA (x:xs)     =  [tipoQA x]++(listaQA xs)
 
tipoQA :: QA -> QA 
tipoQA (Q x) = (Q (trimListaFrag x))
tipoQA (A y) = (A (trimRespuesta y))


trimRespuesta :: Respuesta -> Respuesta
trimRespuesta (ESSAY)   = (ESSAY)
trimRespuesta (MO m)    = (MO (trimListaOpcion m))
trimRespuesta (FV f)    = (FV f)
trimRespuesta (SHORT s) = (SHORT (trimListaOpcion s))

trimListaOpcion :: [Opcion] -> [Opcion]
trimListaOpcion [    ]    = []
trimListaOpcion (x:xs)    = [(trimOpcion x)] ++ trimListaOpcion(xs)  

trimOpcion :: Opcion -> Opcion 
trimOpcion (OK op1)  = (OK (trimListaFrag op1)) 
trimOpcion (NOK op2) = (NOK (trimListaFrag op2))

trimListaFrag :: [Fragmento] -> [Fragmento]
trimListaFrag [    ]    = []
trimListaFrag (x:xs) = [(trimFragmento x)] ++ (trimListaFrag xs)  

trimFragmento :: Fragmento -> Fragmento 
trimFragmento (TXT tstr)  = (TXT  (trimAuxTxt tstr)) 
trimFragmento (MATH mstr) = (MATH (trimAux mstr)) 
trimFragmento (CODE cstr) = (CODE (trimAux cstr)) 

trimAux :: String -> String
trimAux str = trimEnd (trimBegin str)

trimAuxTxt :: String -> String
trimAuxTxt str = trimEndTxt (trimBeginTxt str)

trimBegin :: String -> String 
trimBegin [    ]    = []
trimBegin (x:xs) 
   | x /= ' '   = (x:xs)   
   | otherwise    = do ws <- trimBegin xs
                       return ws 
trimBeginTxt :: String -> String 
trimBeginTxt [    ]    = []
trimBeginTxt (x:xs) 
   | notSpace x   = (x:xs)   
   | otherwise    = trimBegin xs

trimEnd :: String -> String 
trimEnd [    ]    = []
trimEnd (xs) 
   | (last xs) /= ' '   = (xs)   
   | otherwise          = do ws <- trimEnd (init xs)
                             return ws 
trimEndTxt :: String -> String 
trimEndTxt [    ]    = []
trimEndTxt (xs) 
   | notSpace (last xs)   = (xs)   
   | otherwise          = trimEndTxt (init xs)
notElement :: Char -> String -> Bool
notElement e s = not (elem e s)

notSpace :: Char -> Bool
notSpace e = notElement e s
  where s =" \n"
-- Funciones auxiliares mo2short
-- Funciones auxiliares mo2short
getCuerpoMO :: Ejercicio QA -> Ejercicio QA
getCuerpoMO (Ejercicio a b c d)  = (Ejercicio a b (listaQAshort c) d)


listaQAshort :: Cuerpo QA -> Cuerpo QA  
listaQAshort [    ]     =  []
listaQAshort (x:xs)     =  [tipoQAshort x]++(listaQAshort xs)
 
tipoQAshort :: QA -> QA 
tipoQAshort (Q x) = (Q x)
tipoQAshort (A y) = (A (shortRespuesta y))


shortRespuesta :: Respuesta -> Respuesta
shortRespuesta (ESSAY)   = (ESSAY)
shortRespuesta (MO m)    = (shortMOlista m)
shortRespuesta (FV f)    = (FV f)
shortRespuesta (SHORT s) = (SHORT s)

shortMOlista :: [Opcion] -> Respuesta
shortMOlista (x:xs)    = if ((length (x:xs) > 0) && (isOK (x:xs))) then (SHORT (x:xs)) else (MO (x:xs))    

isOK :: [Opcion] -> Bool
isOK [    ]    = True
isOK (x:xs)    = if (isOK2 x) then (isOK xs) else False

isOK2 :: Opcion -> Bool 
isOK2 (OK op1)  = True 
isOK2 (NOK op2) = False


-- Funciones auxiliares sort

getCuerpoSort :: Ejercicio QA -> Ejercicio QA
getCuerpoSort (Ejercicio a b c d)  = (Ejercicio a b (listaQAsort c) d)


listaQAsort :: Cuerpo QA -> Cuerpo QA  
listaQAsort [    ]     =  []
listaQAsort (x:xs)     =  [tipoQAsort x]++(listaQAsort xs)

tipoQAsort :: QA -> QA 
tipoQAsort (Q x) = (Q x)
tipoQAsort (A y) = (A (sortRespuesta y))


sortRespuesta :: Respuesta -> Respuesta
sortRespuesta (ESSAY)   = (ESSAY)
sortRespuesta (MO m)    = (sortMOlista m)
sortRespuesta (FV f)    = (FV f)
sortRespuesta (SHORT s) = (SHORT s)

sortMOlista :: [Opcion] -> Respuesta
sortMOlista lst = (MO ((getOKS lst) ++ (getNOKS lst)))

getOKS :: [Opcion] -> [Opcion]
getOKS [] = []
getOKS (x:xs)
  | isOK2 x = x:(getOKS xs)
  | otherwise =getOKS xs

getNOKS :: [Opcion] -> [Opcion]
getNOKS [] = []
getNOKS (x:xs)
  | isOK2 x = getNOKS xs
  | otherwise =x:(getNOKS xs)

-- Funciones Auxiliares nodupMO
nodupMOQA :: Cuerpo QA -> Cuerpo QA
nodupMOQA [] = []
nodupMOQA (Q x:xs) = (Q x):nodupMOQA xs
nodupMOQA (A x:xs) = (A (nodupMOA x)):nodupMOQA xs

nodupMOA :: Respuesta -> Respuesta
nodupMOA (MO opciones) = MO (nodupMOOpciones opciones [])
nodupMOA x = x

nodupMOOpciones :: [Opcion] -> [String] -> [Opcion]
nodupMOOpciones [] y = []
nodupMOOpciones (x:xs) y
  |elem strx y = nodupMOOpciones xs y
  |otherwise = x:(nodupMOOpciones xs (strx:y))
  where strx = show x

-- Funciones Auxiliares para filtroVOF
contar :: Bool -> [Ejercicio QA] -> Int
contar _ [] = 0
contar vof (x:xs) = (elementis vof x) + (contar vof xs)

elementis :: Bool -> Ejercicio QA -> Int
elementis vof x = cuerpois vof cuerpo
  where Ejercicio _ _ cuerpo _ = x

cuerpois :: Bool -> [QA] -> Int
cuerpois _ [] = 0
cuerpois vof (x:xs) = (qais vof x) + (cuerpois vof xs)

qais :: Bool -> QA -> Int
qais vof x = case x of (A (FV x)) -> case x == vof of True  -> 1
                                                      False -> 0
                       otherwise -> 0


quitarFV :: [Ejercicio QA] -> [Ejercicio QA]
quitarFV [] =trace("[>>>>>>>>>>>>>>>>..]") []
quitarFV (x:xs)
    | (ver + fal) > 0 = trace("quitarFV")traceShow(show (x:xs))quitarFV xs
    |otherwise =trace("quitarFV")traceShow(show (xs)) x:(quitarFV xs)
  where ver = trace("fin elementis")elementis True x
        fal = trace("fin elementis")elementis False x
{-
quitarFVEjercicio :: Ejercicio QA -> Ejercicio QA
quitarFVEjercicio x = Ejercicio c1 n (quitarFVCuerpo cuerpo) c2
  where Ejercicio c1 n cuerpo c2 = x
        verdaderas = quitarFVCuerpo

quitarFVCuerpo :: [QA] -> [QA]
quitarFVCuerpo [] = []
quitarFVCuerpo (x:xs)
  | isFV x = quitarFVCuerpo xs
  | otherwise = x: (quitarFVCuerpo xs)

isFV :: QA -> Bool
isFV x = case x of (A (FV x)) -> True
                   otherwise -> False-}