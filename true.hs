data Proposition = Cons Bool
                 | Var  String
                 | Neg  Proposition
                 | And Proposition Proposition
                 | Or  Proposition Proposition
                 | Imp Proposition Proposition
                deriving(Show,Eq)

type Environment = [(String,Bool)]

find :: Environment -> String -> Maybe Bool
find e k = if null found then Nothing
                         else Just $ (snd . head) found
    where found = dropWhile (\ (elem,_) -> elem /= k) e

addOrReplace :: Environment -> String -> Bool -> Environment
addOrReplace e k v = if null tl then (k,v) : hd
                                else replaced
    where (hd,tl)  = span (\ (elem,_) -> k/=elem) e
          replaced = if (snd . head) tl == v then e
                                             else hd ++ [(k,v)] ++ tail tl
b = (replicate 10000 ("b",True)) ++ [("a",False)] ++ (replicate 10000 ("b",True))

remove :: Environment -> String -> Environment
remove e k = if null tl then hd
                        else hd ++ tail tl
    where (hd,tl)  = span (\ (elem,_) -> k/=elem) e

evalP :: Environment -> Proposition -> Maybe Bool
evalP e p = undefined -- Javier

vars :: Proposition -> [String]
vars p = undefined -- Nab

isTautology :: Proposition -> Bool
isTautology p = undefined -- Nab Jav
