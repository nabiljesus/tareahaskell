module LambdaJack where

import Cards as C 
import System.Random(randomR,StdGen)
import Data.Maybe(Maybe,fromJust)

data Player = LambdaJack | You
  deriving(Eq)

value :: Hand -> Int
value (H cards) = sumCards (foldl accVal (0,0) cards)
    where accVal (accC,accA) c = case (getVal c) of 
                                    Numeric n -> (accC+n,accA) 
                                    Ace       -> (accC,accA+1) 
                                    _         -> (accC+10,accA)
          sumCards (acc,0)    = acc
          sumCards (acc,aces) = if (acc+aces*11) > 21 
                                    then acc+aces
                                    else acc+aces*11

busted :: Hand -> Bool
busted = (>21) . value

winner :: Hand -> Hand -> Player
winner myh lh = case (busted myh, busted lh) of
                    (True ,True ) -> LambdaJack
                    (True ,False) -> LambdaJack
                    (False,True ) -> You
                    _             -> if mval > lval then You
                                                    else LambdaJack
                where mval = value myh
                      lval = value lh

fullDeck :: Hand
fullDeck = H [ Card v s | v<- values, s<-suites ]
            where values = Ace : Queen : Jack : King : map Numeric [2..10]
                  suites = Clubs : Diamonds : Spades : Hearts : []

draw :: Hand -> Hand -> Maybe (Hand,Hand)
draw (H [])      myH    = Just (empty, myH)       -- O es eso o es error
draw (H (d:ds)) (H us)  = Just (H ds ,H (d:us))

playLambda :: Hand -> Hand    -- Juego de la computadora, la cague D:
playLambda h =  till16 h empty
              where till16 (H []) lbH  = lbH
                    till16 deck   lbH  = if (value lbH) < 16 then till16 nDeck nHand
                                          else lbH
                      where (nDeck,nHand) = fromJust $ draw deck lbH


shuffle :: StdGen -> Hand -> Hand
shuffle rs (H deck) =  H sDeck
    where (newGen,sDeck,emptyD)   = foldl takeCard (rs,[],deck) [52,51..1]
          takeCard (genR,acc,d) n = ( nexGen , shuffledDeck , restOfDeck)
            where (rndNum,nexGen) = randomR (1,n) genR
                  (fhalf,shalf)   = splitAt (rndNum-1) d
                  shuffledDeck    = (head shalf) : acc 
                  restOfDeck      = fhalf ++ tail shalf

--randomList :: (Random a) => (a,a) -> Int -> StdGen -> [a]
--randomList bnds n = take n . randomRs bnds  

--my a g b      = case a of 
--                0 -> b
--                _ -> my (a-1) (snd ans) $ (fst ans):b

--                where ans = randomR (1,a) g