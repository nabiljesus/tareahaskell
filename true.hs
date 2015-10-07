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

remove :: Environment -> String -> Environment
remove e k = if null tl then hd
                        else hd ++ tail tl
    where (hd,tl)  = span (\ (elem,_) -> k/=elem) e

-- Aux functions ---------------------------------------
maybeBin :: (a->b->c) -> Maybe a -> Maybe b -> Maybe c
maybeBin f (Just op1) (Just op2) = Just $ f op1 op2
maybeBin f _ _                   = Nothing

maybeUn :: (a->b) -> Maybe a -> Maybe b
maybeUn f (Just op1) = Just $ f op1
maybeUn _ Nothing    = Nothing
--------------------------------------------------------
evalP :: Environment -> Proposition -> Maybe Bool
evalP _ (Cons b)          = Just b
evalP e (Var str)         = find e str
evalP e (Neg prop)        = maybeUn   not  (evalP e prop)
evalP e (And prop1 prop2) = maybeBin  (&&) (evalP e prop1)       (evalP e prop2)
evalP e (Or prop1 prop2)  = maybeBin  (||) (evalP e prop1)       (evalP e prop2)
evalP e (Imp prop1 prop2) = maybeBin  (||) (evalP e (Neg prop1)) (evalP e prop2)

vars :: Proposition -> [String]
vars p = undefined -- Nab

isTautology :: Proposition -> Bool
isTautology p = undefined -- Nab Jav

b = (replicate 10000 ("b",True)) ++ [("a",False)] ++ (replicate 10000 ("b",True))
