{-
    true.hs contiene el ejercicio 2, asociado a proposiciones y sus
    repectivas evaluaciones.
    Creado por:
        Javier López
        Nabil  Márquez
-}

import Data.Maybe(fromJust)

data Proposition = Cons Bool                    --Constante Bool
                 | Var  String                   --Variable
                 | Neg  Proposition               -- Negado de una prop
                 | And Proposition Proposition     
                 | Or  Proposition Proposition
                 | Imp Proposition Proposition      --Implicacion
                deriving(Show,Eq)

type Environment = [(String,Bool)]

-- Intenta encontrar el valor Booleano asociado a un string en un ambiente
find :: Environment -> String -> Maybe Bool
find e k = if null found then Nothing
                         else Just $ (snd . head) found
    where found = dropWhile (\ (elem,_) -> elem /= k) e

-- Dadado un ambiente, una clave y un booleano se agrega o reemplaza 
-- la asociación
addOrReplace :: Environment -> String -> Bool -> Environment
addOrReplace e k v = if null tl then (k,v) : hd
                                else replaced
    where (hd,tl)  = span (\ (elem,_) -> k/=elem) e
          replaced = if (snd . head) tl == v then e
                                             else hd ++ [(k,v)] ++ tail tl

-- Remueve la asociación al String en el ambiente
remove :: Environment -> String -> Environment
remove e k = if null tl then hd
                        else hd ++ tail tl
    where (hd,tl)  = span (\ (elem,_) -> k/=elem) e

-- funciones auxiliaiares para evalP --------------------------
maybeBin :: (a->b->c) -> Maybe a -> Maybe b -> Maybe c
maybeBin f (Just op1) (Just op2) = Just $ f op1 op2
maybeBin f _ _                   = Nothing

maybeUn :: (a->b) -> Maybe a -> Maybe b
maybeUn f (Just op1) = Just $ f op1
maybeUn _ Nothing    = Nothing
---------------------------------------------------------------
-- Dado un ambiente, se intenta evaluar la proposición dada
evalP :: Environment -> Proposition -> Maybe Bool
evalP _ (Cons b)          = Just b
evalP e (Var str)         = find e str
evalP e (Neg prop)        = maybeUn   not  (evalP e prop)
evalP e (And prop1 prop2) = maybeBin  (&&) (evalP e prop1)       (evalP e prop2)
evalP e (Or  prop1 prop2) = maybeBin  (||) (evalP e prop1)       (evalP e prop2)
evalP e (Imp prop1 prop2) = maybeBin  (||) (evalP e (Neg prop1)) (evalP e prop2)

-- vars Aux functions ---------------------------------------
union :: (Eq a) => [a] -> [a] -> [a]
union [] b = b
union a [] = a
union a (b:bn) = if b `elem` a then  a `union` bn
                               else (b:a) `union` bn
-------------------------------------------------------------

-- Extrae todas las variables usadas en la proposición
vars :: Proposition -> [String]
vars (Cons _)          = []
vars (Var str)         = [str]
vars (Neg prop)        = vars prop
vars (And prop1 prop2) = union (vars prop1) $vars prop2
vars (Or  prop1 prop2) = union (vars prop1) $vars prop2
vars (Imp prop1 prop2) = union (vars prop1) $vars prop2


-- isTautology aux functions ----------------------------------------
type Binary = [Bool]

-- Convierte un entero en su representación binaria
numToBinary :: Int -> Binary 
numToBinary 0 = [False]
numToBinary n = convert n []
                where convert num l = if num==0
                                        then l
                                        else convert q ((r==1):l)
                        where (q,r) = quotRem num 2

-- Dada una lista de nombres de variables y un numero de caso
-- se genera un ambiente usando la presentacion binaria del entero
generateCase :: [String] -> Int -> Environment  -- Ambiente asociado
generateCase vars n = snd $ foldr zipRight (numToBinary n,[]) vars
    where zipRight var ([],acc)  = ([],(var,False):acc)
          zipRight var (bin,acc) = (tail bin,(var,head bin):acc)

-- Indica si el ambiente dado es el último caso de evaluacion
isLastCase :: Environment -> Bool
isLastCase = all snd -- Extracción de todos los booleanos y and
----------------------------------------------------------------------

-- Se intenta demostrar que la proposición es verdadera
isTautology :: Proposition -> Bool
isTautology p = check p 0
    where check p n 
            | isLastCase env = fromJust (evalP env p)
            | otherwise      = case evalP env p of 
                                   (Just True ) -> check p (n+1)
                                   (Just False) -> False
                                   (Nothing )   -> error "Error 404 var not found"
            where env = generateCase (vars p) n

{-
  También realizamos esta segunda implementación que pensamos que 
  debería ejecutarse en menos tiempo. Pero en la práctica no 
  ocurrió. No estamos seguros de a que se debe.
-}

generateCase' :: [String] -> Int -> (Bool,Environment)  -- Ambiente asociado
generateCase' vars n = snd $ foldr zipRight (numToBinary n,(True,[])) vars
    where zipRight var ([],(b,acc))  = ([],(False,((var,False):acc)))
          zipRight var (bin,(b,acc)) = if b then (tail bin,(b && head bin,((var,head bin):acc)))
                                            else (tail bin,(False,((var,head bin):acc)))

isTautology' :: Proposition -> Bool
isTautology' p = check p 0
    where check p n 
            | isLastC        = fromJust (evalP env p)
            | otherwise      = case evalP env p of 
                                   (Just True ) -> check p (n+1)
                                   (Just False) -> False
                                   (Nothing )   -> error "Error 404 var not found"
            where (isLastC,env) = generateCase' (vars p) n