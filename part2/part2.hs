-- Github Link Of Project: https://github.com/ardaekinci/BLG458E-FP-Final

import Data.Char (isDigit, digitToInt) -- Used for Rank Conversion

-- Define data types
data Color = Red | Black deriving(Show, Eq) -- Derive Show to display card color
data Suit = Clubs | Diamonds | Hearts | Spades deriving(Eq) -- Derive Eq to check card color
data Rank = Num Int | Jack | Queen | King | Ace deriving(Eq) -- Derive Eq to check rank of card
data Card = Card { suit :: Suit, rank :: Rank } deriving(Eq) -- Derive Eq to compare two cards
data Move = Draw | Discard Card deriving(Eq) -- Derive Eq to check next move of user


-- This data type represents the game state.
data GameState = GameState{
    cards :: [Card],        -- Card list
    heldCards :: [Card],    -- Held cards
    currentScore :: Int,    -- Current score
    moves :: [Move]         -- Move list
}


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


-- | This function removes the given card from the card list.
removeCard  :: [Card]   -- Input1: Card list to update
            -> Card     -- Input2: Card that wanted to remove from list
            -> [Card]   -- Output: Updated card list.
removeCard (card:cs) c  -- Split the given list by first element and rest of them.
    -- If card is not exist in the list throw error.
    | not isCardExist   = error "part2: card not in list"
    -- If remove card is the first element of list return rest of the list.
    | card == c         = cs
    -- Call recursively remove card function. Check rest of the list to remove the card. Concatenate the card and the recursive calls.
    | otherwise         = card : removeCard cs c
    where 
        -- Check existence of card in the list.
        isCardExist = elem c cs


-- | This function returns true if colors of cards in the list are same otherwise returns false.
allSameColor :: [Card]  -- Input1: Card List
             -> Bool    -- Output: True if colors are same otherwise False
allSameColor []     = error "part2: card list is empty" -- Throw error if card list empty.
allSameColor [_]    = True                              -- Return true if there is only one card in the list.
allSameColor cards  = null filteredCards                -- If filteredCards is empty return true, otherwise false.
    where
        firstColor = cardColor (cards!!0)   -- Get color of first card in the list.
        -- Get cards that has different color from the first card in the list. It must be empty for same color.
        filteredCards = filter (\x -> cardColor x /= firstColor) cards


-- | This function calculates the value of cards and return it.
sumCards :: [Card]  -- Input1: Card list
         -> Int     -- Output: Total values of cards for given list
sumCards []     = 0                 -- Return 0 if card list is empty.
sumCards [x]    = cardValue x       -- Return the value of card if there is only one card in the list.
sumCards cards = sumCards' cards 0  -- Call Recursive function with initial value as 0.
    where 
        sumCards' :: [Card] -- Input1: Card list to calculate sum of values
                  -> Int    -- Input2: Acc value (Result)
                  -> Int    -- Output: Sum of values
        sumCards' [] acc = acc -- Return the results if there is no elements in the list anymore.
        -- Get first element of list and call function for rest of the list.
        -- Call function with new acc value
        sumCards' (c:cs) acc = sumCards' cs nextAcc
            where
                nextAcc = acc + (cardValue c) -- Calculate the value of first element in the list.


-- | Score function calculates the score from held-cards and goal.
score :: [Card]     -- Input1: Held-Cards
      -> Int        -- Input2: Goal
      -> Int        -- Output: Score
score heldCards goal
    | sameColor = floor (fromIntegral preliminaryScore / 2) -- If all the colors are same in the held-cards. Divide score by 2
    | otherwise = preliminaryScore             -- If colors are not asem return the calculated score.
    where 
        sumOfValues = sumCards heldCards        -- Calculate sum of values for held cards
        sameColor = allSameColor heldCards      -- Check colors of held cards
        preliminaryScore = calculatePreliminaryScore sumOfValues goal -- calculate preliminary score 
        calculatePreliminaryScore :: Int    -- Input1: Sum of values
                                  -> Int    -- Input2: Goal
                                  -> Int    -- Output: Preliminary score
        calculatePreliminaryScore sum goal  -- Calculate preliminary score according to rules.
            | sum > goal    = 3 * (sum - goal)
            | otherwise     = goal - sum


-- | This function return next move and the remaining moves.
getNextMove :: [Move]           -- Input1: Current Move list
            -> (Move, [Move])   -- Output: (Next move, Remaining moves)
getNextMove (nextMove:moves) = (nextMove, moves)


-- | Takes cards, held-cards and move, returns the next cards according the seleceted move.
calculateNextCards  :: ([Card], [Card])   -- Input1: (Current cards in the list, current held cards)
                    -> Move               -- Input2: Next move
                    -> ([Card], [Card])   -- Output: (Next cards in the list, next held cards)
calculateNextCards (cards, heldCards) nextMove
    -- If the next move is discard card, do not change cards and remove discarted card from the held cards.
    | nextMove /= Draw  = (cards, removeCard heldCards discartedCard)
    -- If the next move is draw card, remove first card from cards and put it into held cards 
    -- drop n xs = Returns the list after dropping first n items
    -- n:xs      = Returns the list after adding n as first element
    | otherwise         = (drop 1 cards, cards!!0 : heldCards)
    where
        Discard discartedCard = nextMove


-- | Run the game according to given moves and cards and calculate score.
runGame :: [Card]       -- Input1: Initial card list
        -> [Move]       -- Input2: Move list
        -> Int          -- Input3: Goal
        -> Int          -- Output: Score
runGame cardList moveList goal = runGame' GameState{cards = cardList, heldCards = [], currentScore = 0, moves = moveList}
    where
        runGame' :: GameState -- Input1: Current game state
                 -> Int       -- Output: Score
        runGame' currentState
            | null (moves currentState) = currentScore currentState -- If there are no moves finish the game
            -- If user discard a card make recursive call with updated cards
            | nextMove /= Draw = nextState
            | null (cards currentState) = currentScore currentState -- If there are no cards for draw finish the game
            | nextScore > goal = currentScore currentState -- If score after the drawing exceed the goal finish the game
            | otherwise = nextState
            where
                -- Calculate next move and remaining moves
                (nextMove, remainingMoves) = getNextMove (moves currentState)
                -- Calculate next cards in the card list and held cards according to move
                (nextCards, nextHeldCards) = calculateNextCards (cards currentState, heldCards currentState) nextMove
                -- Calculate next score for held cards
                nextScore = score nextHeldCards goal
                -- Calculate next state of the game
                nextState = runGame' GameState{cards = nextCards, heldCards = nextHeldCards, currentScore = nextScore, moves = remainingMoves}


-- | Converts given char to related Suit. Checks first letter of Suit.
convertSuit :: Char -- Input1: Code of suit
            -> Suit -- Output: Related suit 
convertSuit c 
    | c == 'd' || c == 'D' = Diamonds
    | c == 'c' || c == 'C' = Clubs
    | c == 'h' || c == 'H' = Hearts
    | c == 's' || c == 'S' = Spades
    | otherwise            = error "part2: given suit is unknown"


-- | Converts given char to related Rank. Checks first letter for face cards, for "Ace" checks `1`, for `10` checks `t` or `T`.
convertRank :: Char -- Input1: Code of Rank
            -> Rank -- Output: Related Rank
convertRank r
    | r == '1'             = Ace
    | r == 'j' || r == 'J' = Jack
    | r == 'q' || r == 'Q' = Queen
    | r == 'k' || r == 'K' = King
    | r == 't' || r == 'T' = Num 10
    | isDigit r            = Num (digitToInt r) -- If given code is digit convert it to int and return related Rank.
    | otherwise            = error "part2: given rank is unknown"


-- | Takes Suit and Rank code, returns card.
convertCard :: Char -- Input1: Suit code
            -> Char -- Input2: Rank code
            -> Card -- Output: Created card from rank and suid code
convertCard suitCode rankCode = Card{suit = convertSuit suitCode, rank = convertRank rankCode}


-- | This function read cards from the user and return the card list.
readCards :: IO [Card]
readCards = readCards' [] -- Call helper method recursively to store read cards.
    where
        readCards' :: [Card]       -- Input1: Read cards from the beginning
                   -> IO [Card]    -- Output: Card list
        readCards' cs = do
            userInput <- getLine   -- Get input from user
            case userInput of
                "." -> return cs  -- If user enters a single dot return the read cards 
                -- Used pattern matching if given string consist of 2 chars this pattern will work.
                [suitCode, rankCode] -> readCards' (cs ++ [convertCard suitCode rankCode]) -- If user enters valid input, convert input to card and add to list
                _                    -> error "part2: invalid input supplied."  -- If user input does not match anything throw an error.


-- | Takes Move name, Suit and Rank code returns Move.
convertMove :: Char -- Input1: Move name
            -> Char -- Input2: Suit code
            -> Char -- Input3: Rank code
            -> Move -- Output: Created Move from name and codes
-- Use pattern matching, if move name is 'd' or 'D' ignore other params
convertMove 'd' _ _                 = Draw
convertMove 'D' _ _                 = Draw
-- Use pattern matching, if move name is 'r' or 'R' get card with suit and rank code, and create move.
convertMove 'r' suitCode rankCode   = Discard (convertCard suitCode rankCode)
convertMove 'R' suitCode rankCode   = Discard (convertCard suitCode rankCode) 


-- | This function read moves from the user and return the move list.
readMoves :: IO [Move]
readMoves = readMoves' [] -- Call helper method recursively to store read moves.
    where
        readMoves' :: [Move]       -- Input1: Read moves from the beginning
                   -> IO [Move]    -- Output: Move list
        readMoves' ms = do
            userInput <- getLine   -- Get input from user
            case userInput of
                "." -> return ms  -- If user enters a single dot return the read moves 
                -- If user enters single letter convert it to Move (Draw) by using pattern matching
                [moveName] -> readMoves' (ms ++ [convertMove moveName '_' '_'])
                -- If user enters three letters convert it to Move (Discard card) by using pattern matching
                [moveName, suitCode, rankCode] -> readMoves' (ms ++ [convertMove moveName suitCode rankCode])
                _                    -> error "part2: invalid input supplied." -- If user input does not match anything throw an error.


main = do 
    putStrLn "Enter cards:"
    cards <- readCards
    -- putStrLn (show cards)
    putStrLn "Enter moves:"
    moves <- readMoves
    -- putStrLn (show moves)
    putStrLn "Enter goal:"
    line <- getLine
    let goal = read line :: Int
    let score = runGame cards moves goal
    putStrLn ("Score: " ++ show score)