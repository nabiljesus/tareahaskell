module Cards where

data Suit = Clubs 
          | Diamonds 
          | Spades 
          | Hearts

instance Show Suit where
    show Clubs    = "♣"
    show Diamonds = "♦"
    show Spades   = "♠"
    show Hearts   = "♥"

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
    show Ace         = "As"

data Card = Card {
    value :: Value,
    suit  :: Suit
}

instance Show Card where
    show (Card v s) = show s ++ show v

newtype Hand = H [Card]

instance Show Hand where
    show (H []) = ""
    show (H c ) = concatMap (\x -> show x ++ " ") c

empty :: Hand
empty = H []

size :: Hand -> Int
size (H c) = length c

