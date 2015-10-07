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

-- evalP Aux functions ---------------------------------------
maybeBin :: (a->b->c) -> Maybe a -> Maybe b -> Maybe c
maybeBin f (Just op1) (Just op2) = Just $ f op1 op2
maybeBin f _ _                   = Nothing

maybeUn :: (a->b) -> Maybe a -> Maybe b
maybeUn f (Just op1) = Just $ f op1
maybeUn _ Nothing    = Nothing
---------------------------------------------------------------
evalP :: Environment -> Proposition -> Maybe Bool
evalP _ (Cons b)          = Just b
evalP e (Var str)         = find e str
evalP e (Neg prop)        = maybeUn   not  (evalP e prop)
evalP e (And prop1 prop2) = maybeBin  (&&) (evalP e prop1)       (evalP e prop2)
evalP e (Or prop1 prop2)  = maybeBin  (||) (evalP e prop1)       (evalP e prop2)
evalP e (Imp prop1 prop2) = maybeBin  (||) (evalP e (Neg prop1)) (evalP e prop2)

-- vars Aux functions ---------------------------------------
union :: (Eq a) => [a] -> [a] -> [a]
union [] b = b
union a [] = a
union a (b:bn) = if elem b a then union a bn
                 else union (b:a) bn
-------------------------------------------------------------
vars :: Proposition -> [String]
vars (Cons _)          = []
vars (Var str)         = [str]
vars (Neg prop)        = vars prop
vars (And prop1 prop2) = union (vars prop1) $vars prop2
vars (Or prop1 prop2)  = union (vars prop1) $vars prop2
vars (Imp prop1 prop2) = union (vars prop1) $vars prop2

isTautology :: Proposition -> Bool
isTautology p = undefined -- Nab Jav

b = (replicate 10000 ("b",True)) ++ [("a",False)] ++ (replicate 10000 ("b",True))
unAnd = And (Or (Var "a") (Var "b")) (Imp (Cons True) (Neg (And (Var "b") (Var "i"))))
