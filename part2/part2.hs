-- Define data types
data Color = Red | Black deriving(Show) -- Derive Show to display card color
data Suit = Clubs | Diamonds | Hearts | Spades deriving(Eq) -- Derive Eq to check card color
data Rank = Num Int | Jack | Queen | King | Ace
data Card = Card { suit :: Suit, rank :: Rank }
data Move = Draw | Discard Card

-- | It returns the color of given card.
cardColor :: Card   -- Input1: Card
          -> Color  -- Output: Color of given Card
cardColor card 
    -- If Black Types includes the type of given card return black, otherwise red. 
    | elem (suit card) [Clubs, Spades] = Black
    | otherwise = Red