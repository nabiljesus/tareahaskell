filterC :: (a -> Bool) -> [a] -> [a]
filterC p l = [x | x<-l, p x]

filterM :: (a -> Bool) -> [a] -> [a]
filterM p l = concat $ map addIfTrue l      -- Concat usar foldr... dunno
    where addIfTrue e = if p e then [e]
                               else []

filterF :: (a -> Bool) -> [a] -> [a]
filterF p l = foldr addIfTrue [] l
    where addIfTrue e l = if p e then e : l
                                 else l