-- Define data types
data Color = Red | Black deriving(Show) -- Derive Show to display card color
data Suit = Clubs | Diamonds | Hearts | Spades deriving(Eq) -- Derive Eq to check card color
data Rank = Num Int | Jack | Queen | King | Ace deriving(Eq) -- Derive Eq to check rank of card
data Card = Card { suit :: Suit, rank :: Rank }
data Move = Draw | Discard Card

-- | It returns the color of given card.
cardColor :: Card   -- Input1: Card
          -> Color  -- Output: Color of given Card
cardColor card 
    -- If Black Types includes the type of given card return black, otherwise red. 
    | elem (suit card) [Clubs, Spades] = Black
    | otherwise = Red


-- | This function returns the value of given card.
cardValue :: Card   -- Input1: Card
          -> Int    -- Output: Value of given Card
cardValue card
    | rank card == Jack      = 10    -- Jack is 10 pts
    | rank card == Queen     = 10    -- Queen is 10 pts
    | rank card == King      = 10    -- King is 10 pts
    | rank card == Ace       = 11    -- Ace is 10 pts
    | otherwise              = val   -- Otherwise return the card value.
    where
        Num val = rank card  -- If card is number extract the value of card from data type. 
