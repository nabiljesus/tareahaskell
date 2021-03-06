{-
    Módulo que define las operaciones con las cartas, el tipo
    jugador y el barajar. 
    
    Creado por:
        Javier López
        Nabil  Márquez
-}

module LambdaJack where

import Cards hiding(value)
import qualified Cards(value)
import System.Random(randomR,StdGen)
import Data.Maybe(fromJust)

data Player = LambdaJack | You
  deriving(Eq)

-- Función para obtener el valor de una mano
value :: Hand -> Int
value (H cards) = sumCards (foldl accVal (0,0) cards)
    where accVal (accC,accA) c = case Cards.value c of 
                                    Numeric n -> (accC+n,accA) 
                                    Ace       -> (accC,accA+1) 
                                    _         -> (accC+10,accA)
          sumCards (acc,0)    = acc
          sumCards (acc,aces) = if (acc+aces*11) > 21 
                                    then acc+aces
                                    else acc+aces*11

-- Función que indica si se una mano superó los 21 puntos
busted :: Hand -> Bool
busted = (>21) . value

-- Dadas las manos de cartas de dos usuarios se indica
-- si hay ganador al 
winner :: Hand -> Hand -> Player
winner myh lh = case (busted myh, busted lh) of
                    (True ,True ) -> LambdaJack
                    (True ,False) -> LambdaJack
                    (False,True ) -> You
                    _             -> if mval > lval then You
                                                    else LambdaJack
                where mval = value myh
                      lval = value lh

-- Nuevo mazo de cartas
fullDeck :: Hand
fullDeck = H [ Card v s | v<- values, s<-suites ]
            where values = Ace : Queen : Jack : King : map Numeric [2..10]
                  suites =  [Clubs, Diamonds, Spades, Hearts]

-- Dado un mazo y una mano se entrega el resultado de pasar
-- la carta en el tope a la mano
draw :: Hand -> Hand -> Maybe (Hand,Hand)
draw (H [])      _      = Nothing              -- O es eso o es error
draw (H (d:ds)) (H us)  = Just (H ds ,H (d:us))

-- Juego automático de Lambda, o la 'computadora'
playLambda :: Hand -> Hand    -- Juego de la computadora, la cague D:
playLambda h =  till16 h empty
              where till16 (H []) lbH  = lbH
                    till16 deck   lbH  = if value lbH < 16 
                                            then till16 nDeck nHand
                                         else lbH
                      where (nDeck,nHand) = fromJust $ draw deck lbH


-- Dado un generador de números al azar y un mazo, se obtiene
-- el mismo mazo barajado
shuffle :: StdGen -> Hand -> Hand
shuffle rs (H deck) =  H sDeck
    where (_,sDeck,_)   = foldl takeCard (rs,[],deck) [52,51..1]
          takeCard (genR,acc,d) n = ( nexGen , shuffledDeck , restOfDeck)
            where (rndNum,nexGen) = randomR (1,n) genR
                  (fhalf,shalf)   = splitAt (rndNum-1) d
                  shuffledDeck    = head shalf : acc 
                  restOfDeck      = fhalf ++ tail shalf