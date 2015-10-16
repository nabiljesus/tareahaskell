module LambdaJack where

import Cards as C
import System.Random 

data Player = LambdaJack | You

value :: Hand -> Int
value (H cards) = sumCards (foldl accVal (0,0) cards)
    where accVal (accC,accA) c = case (C.value c) of 
                                    Numeric n -> (accC+n,accA) 
                                    Ace       -> (accC,accA+1) 
                                    _         -> (accC+10,accA)
          sumCards (acc,0)    = acc
          sumCards (acc,aces) = if (acc+aces*11) > 21 
                                    then acc+aces
                                    else acc+aces*11

busted :: Hand -> Bool
busted = (>21) . LambdaJack.value

winner :: Hand -> Hand -> Player
winner myh lh = case (busted myh, busted lh) of
                    (True ,True ) -> error "No winner, wtf?"
                    (True ,False) -> You
                    (False,True ) -> LambdaJack
                    _             -> if mval > lval then You
                                                    else LambdaJack
                where mval = LambdaJack.value myh
                      lval = LambdaJack.value lh

fullDeck :: Hand
fullDeck = H [ Card v s | v<- values, s<-suites ]
            where values = Ace : Queen : Jack : Ace : map Numeric [2..10]
                  suites = Clubs : Diamonds : Spades : Hearts : []

draw :: Hand -> Hand -> Maybe (Hand,Hand)
draw (H [])        _    = Nothing              -- Not sure about this
draw (H (d:ds)) (H us)  = Just (H ds ,H (d:us))

playLambda :: Hand -> Hand     -- didnt get it
playLambda = undefined

shuffle :: StdGen -> Hand -> Hand
shuffle rs (H deck) =  H sDeck
    where (newGen,sDeck,emptyD)   = foldl takeCard (rs,[],deck) [52,51..1]
          takeCard (genR,acc,d) n = ( nexGen , shuffledDeck , restOfDeck)
            where (rndNum,nexGen) = randomR (1,n) genR
                  (fhalf,shalf)   = splitAt (rndNum-1) d
                  shuffledDeck    = (head shalf) : acc 
                  restOfDeck      = fhalf ++ tail shalf