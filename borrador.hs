data Comentario     = COM [ String ]
  deriving Show


funcionMayorDiez :: Int -> Bool
funcionMayorDiez a 
    | a > 10 = True
    | otherwise = False


funcionEscape :: Int -> Bool
funcionEscape a 
    | a == 1 = True
    | otherwise = False


--- El programa leeMX lee una lista hasta encontrar una marca. Si no encuentra
--- marcas, devuelve Nothing.

leeMX :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
leeMX m [] = Nothing
leeMX m (x:xs)
   | m x       = Just ([ ],  x, xs)
   | otherwise = do (ws, z, zs) <- leeMX m xs
                    return (x:ws, z, zs)


--- El programa leeMO lee una lista hasta encontrar una marca. Si no encuentra
--- marcas, devuelve Nothing en el segundo argumento y la lista vacía en el
--- tercero.

leeMO :: (a -> Bool) -> [a] -> ([a], Maybe a, [a])
leeMO m xs = case leeMX m xs of Nothing -> ([], Nothing, [])
                                Just (lista ,elem, finlista) -> (lista,Just elem, finlista)


toList :: String -> [String]
toList cadena = map (:[]) cadena


isCom :: [String] -> Bool
isCom xs = (length xs > 2) && (head xs == "/") && (head(tail xs) == "/") && (last xs == "\n")


esComentario :: String -> Bool
esComentario xs = isCom(toList xs)


removeBarra :: [String] -> String 
removeBarra xs
    | xs == []         = ""
    | (head xs == "/") = " " ++ removeBarra (tail xs) 
    | otherwise        = toString xs 


toString :: [String] -> String 
toString xs 
    | xs == []         = ""
    | otherwise        = (head xs) ++ toString (tail xs)


removeSlash :: String -> [String]
removeSlash cadena = drop 2 (toList cadena)


getComs :: [ String ] -> Maybe (Comentario, [ String ])
getComs xs 
    | esComentario (head xs) = Just (COM (getComsAux xs), getTrashAux xs)
    | otherwise              = Nothing


getComsAux :: [ String ] -> [ String ] 
getComsAux xs
    | xs == []               = []
    | esComentario (head xs) = [(removeBarra (removeSlash (head xs)))] ++ getComsAux(tail xs) 
    | otherwise              = [] ++ getComsAux(tail xs) 


getTrashAux :: [ String ] -> [ String ]
getTrashAux xs 
    | xs == []               = []
    | esComentario (head xs) = [] ++ getTrashAux(tail xs) 
    | otherwise              = [(head xs)] ++ getTrashAux(tail xs)

