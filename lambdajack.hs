{-
    Módulo que define la comunicación entre el mundo y las funciones puras
    necesarias para jugar blackjack definidos en los módulos LambdaJack y
    Cards

    Creado por:
        Javier López
        Nabil  Márquez
-}

module Main where 

import Cards         as C
import LambdaJack    as L
import System.Random(newStdGen,StdGen)
import System.IO

-- Implementación algo robusta de fromJust
getJust :: Maybe a -> a
getJust (Just a) = a
getJust Nothing  = error "oops, I couldn't convert that `Nothingness`"

-- Estado de un juego
data GameState = GS {
    games     :: Int,    --Partidas jugadas
    lamdaWins :: Int,    --Cantidad de partidas ganadas por lambda
    name      :: String, --Nombre del jugador
    generator :: StdGen  --Generador de números pseudorandom
}
instance Show GameState where
    show (GS g lw n _) = "Después de " ++ show g         ++ 
                         " partida(s) Lambda ha ganado " ++ 
                         show lw ++ " y " ++ n           ++ 
                         " ha ganado " ++ show (g-lw)

--Impresión del estado del juego
currentState :: GameState -> IO ()
currentState gs = putStrLn $ show gs

--Construcción para consultar si se desea seguir jugando
continuePlaying :: IO Bool
continuePlaying = 
    do putStrLn "Desea seguir jugando? (s/n): "
       opt <- getChar
       putStrLn ""
       if opt == 's' then return True
                     else return False

--Loop principal del juego
gameloop :: GameState -> IO ()
gameloop gs =
    do currentState gs
       let (deck,hand) = getJust $ draw (shuffle (generator gs) fullDeck) empty -- Barajar mazo
       drawNPlay deck hand gs -- Loop de tomar una carta hasta querer detener, y permitir a lambda jugar
        
        where drawNPlay d h gs = 
                do let Just (nDeck,nHand) =  draw d h -- Tomar una carta
                   putStrLn $ name gs ++ ", tu mano es "++ show nHand ++" suma "++ (show . L.value) nHand
                   newRand <- newStdGen 
                   if busted nHand 
                        then do putStrLn "Perdiste :("  --En caso de perder se intenta inciar otra partida
                                oneMoreRound gs { games=(games gs)+1,
                                                  lamdaWins=(lamdaWins gs)+1,
                                                  generator=newRand}
                        else do putStrLn "Carta o listo? (c/l): " -- Si no se ha perdido toma de decisión
                                opt <- getChar
                                putStrLn ""
                                if opt == 'c' 
                                      then drawNPlay nDeck nHand gs --Al detener la toma de cartas juega lambda
                                      else 
                                        do let lambdaHand = (playLambda nDeck)
                                           putStrLn $ "Mi mano fue "++ show lambdaHand ++", que suma "++ (show . L.value) lambdaHand
                                           if (winner nHand lambdaHand) == You
                                              then 
                                                do putStrLn ("Ganaste! " ++ name gs) 
                                                   oneMoreRound gs { games=(games gs)+1,
                                                                     generator=newRand}
                                              else 
                                                do putStrLn "Gané >:]"
                                                   oneMoreRound gs { games=(games gs)+1,
                                                                     lamdaWins=(lamdaWins gs)+1,
                                                                     generator=newRand}

              oneMoreRound gs = do cont <- continuePlaying
                                   if cont then gameloop gs
                                           else putStrLn "Hasta luego, aunque no creo que desees regresar."

-- Programa principal
main = do  putStrLn  "Bienvenido a LamdaJack"
           putStrLn  "Cuál es su nombre?"
           yourName <- getLine
           c <- newStdGen
           putStrLn ""
           hSetBuffering stdin NoBuffering
           gameloop (GS 0 0 yourName c)


