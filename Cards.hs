{-
    Módulo que define el tipo de las cartas y una mano de cartas
    Creado por:
        Javier López
        Nabil  Márquez
-}
module Cards(
	Hand(..),
	Card(Card,suit),
	Suit(..),
	Value(..),
	empty,
	getVal,
	size
	) where

-- Representación de "palos" en una carta 
data Suit = Clubs 
          | Diamonds 
          | Spades 
          | Hearts

instance Show Suit where
    show Clubs    = "♣"
    show Diamonds = "♦"
    show Spades   = "♠"
    show Hearts   = "♥"

-- Valor de una carta
data Value = Numeric Int 
           | Jack 
           | Queen 
           | King 
           | Ace

instance Show Value where
    show (Numeric n) = show n
    show Jack        = "J"
    show Queen       = "Q"
    show King        = "K"
    show Ace         = "A"

-- Representación de una carta
data Card = Card {
    value :: Value,
    suit  :: Suit
}

instance Show Card where
    show (Card v s) = show s ++ show v

-- Obtener el valor de una carta
getVal :: Card -> Value
getVal = value

newtype Hand = H [Card]

instance Show Hand where
    show (H []) = ""
    show (H c ) = concatMap (\x -> show x ++ " ") c

-- Mano Vacía
empty :: Hand 
empty = H []

-- Cantidad de cartas en una mano
size :: Hand -> Int
size (H c) = length c

