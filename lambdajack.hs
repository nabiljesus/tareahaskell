{-
    Módulo que define la comunicación entre el mundo y las funciones puras
    necesarias para jugar blackjack definidos en los módulos LambdaJack y
    Cards

    Creado por:
        Javier López
        Nabil  Márquez
-}

module Main where 

import Cards(empty)                        
import LambdaJack    as L
import System.Random(StdGen,mkStdGen,next)
import System.IO
import Data.Char(toUpper)

-- Semila utilizada para generar números aleatorios
seedInt :: Int
seedInt = 42

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
    show (GS g lw n _) = "\nDespués de " ++ show g         ++ 
                         " partida(s) Lambda ha ganado " ++ 
                         show lw ++ " y " ++ n           ++ 
                         " ha ganado " ++ show (g-lw)

--Impresión del estado del juego
currentState :: GameState -> IO ()
currentState = print

--Construcción para consultar si se desea seguir jugando
continuePlaying :: IO Bool
continuePlaying = 
    do putStrLn "\nDesea seguir jugando? (S/N): "
       ynLoop
       where ynLoop = do opt <- getChar
                         case toUpper opt of
                            'S' -> return True
                            'N' -> return False
                            _   -> ynLoop


--Loop principal del juego
gameloop :: GameState -> IO ()
gameloop gs = do 
    if games gs > 0
        then currentState gs
        else putStr ""
    -- barajar mazo
    let (deck,hand) = getJust $ 
                        draw (shuffle (generator gs) fullDeck) empty 
    drawNPlay deck hand -- Loop de tomar una carta hasta querer detener, y permitir a lambda jugar
        where drawNPlay d h = do 
                let Just (nDeck,nHand) =  draw d h -- Tomar una carta
                putStr $ "\n" ++ name gs ++ ", tu mano es "++
                     show nHand ++", suma "++ (show . L.value) nHand ++ "."
                -- newRand <- newStdGen
                let (_,newRand) = next $ generator gs
                if busted nHand 
                     then do putStrLn " Perdiste :("  --En caso de perder se intenta inciar otra partida
                             oneMoreRound gs { games     = games gs + 1,
                                               lamdaWins = lamdaWins gs + 1,
                                               generator = newRand}
                     else do putStrLn "\n¿Carta o listo? (C/L): " -- Si no se ha perdido toma de decisión
                             cnLoop nDeck nHand newRand
                             where
                               cnLoop nDeck nHand newRand = do
                                 opt <- getChar
                                 case toUpper opt of
                                   'C' -> drawNPlay nDeck nHand --Al detener la toma de cartas juega lambda
                                   'L' -> do let lambdaHand = playLambda nDeck
                                             putStrLn $ "\nMi mano es "++ show lambdaHand ++
                                                        ", suma " ++ (show . L.value) lambdaHand ++"."
                                             if winner nHand lambdaHand == You
                                                then 
                                                  do putStrLn ("¡Tú Ganas! Seguro hiciste trampa, " ++ 
                                                             name gs ++".") 
                                                     oneMoreRound gs { games     = games gs + 1,
                                                                       generator = newRand}
                                                else 
                                                  do if value nHand == value lambdaHand
                                                         then putStrLn "Empatamos, así que yo gano. >:)"
                                                         else putStrLn "YO gano. Como era de esperarse."
                                                     oneMoreRound gs { games     = games gs     + 1,
                                                                       lamdaWins = lamdaWins gs + 1,
                                                                       generator = newRand}
                                   _   -> cnLoop nDeck nHand newRand

-- Mensaje final. Si lambda ganó más partidas muestra un mensaje o en caso contrario usa otro.
oneMoreRound :: GameState -> IO ()
oneMoreRound gs = do 
          cont <- continuePlaying;
                    if cont then gameloop gs;
                            else 
                              if  lw > tgm-lw ;
                                then putStrLn "\n\nHasta luego, aunque no creo que desees regresar."
                                else putStrLn "\n\nHas sido un rival honorable. Esperare con ansias la próxima."

          where lw   = lamdaWins gs;
                tgm  = games gs;

-- Mensaje de bienvenida y solicita el nombre
welcome :: IO String
welcome = do  
    putStrLn  "\nBienvenido a LamdaJack"
    putStrLn  "¿Cuál es su nombre?"
    getLine

-- Programa principal
main :: IO ()
main =  do 
    yourName <- welcome
    hSetBuffering stdin NoBuffering
    gameloop (GS 0 0 yourName (mkStdGen seedInt))


