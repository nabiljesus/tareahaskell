{-
    Tres diferentes implementaciones de filter
    Creado por:
        Javier López
        Nabil  Márquez
-}

--Haciendo uso de listas por c.
filterC :: (a -> Bool) -> [a] -> [a]
filterC p l = [x | x<-l, p x]

--Haciendo uso de map
filterM :: (a -> Bool) -> [a] -> [a]
filterM p l = concat $ map addIfTrue l  
    where addIfTrue e = if p e then [e]
                               else []

--Haciendo uso de fold
filterF :: (a -> Bool) -> [a] -> [a]
filterF p l = foldr addIfTrue [] l
    where addIfTrue e l = if p e then e : l
                                 else l