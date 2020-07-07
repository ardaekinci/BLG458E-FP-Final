-- Used to store character counts and map operations.
import Data.Map (Map, fromList, toList, fromListWith, insertWith, empty)      
-- Used for toLower, char operations on string.
import Data.Char (toLower) 

{-
    This function is combination of map and filter functions.
    First the function gets the lower case of given string by using map.
    In this scenario, map takes a function and a list then it applies function to all elements in the list.
    
    In the main function, map takes 2 args
    first args: Filter function to calculate occurences of letters
    second args: lowerCaseWord, Since the second argument is a list of chars function runs for each letter.

    In the first args of map function:
    (\y -> (y, length (filter (\x -> x == y ) lowerCaseWord)))
    y is the input for map function (In this example it is letters in lowerCaseWord)
    x is the input for filter function(In this example it is letters in lowerCaseWord)
    For every letter in the word, it counts the number of occurences by using length function.
    fromList converts to list to Map data type.
    [('f', 1), ('a', 2)] ---> Map Char Int
-}
-- | This function takes a word (String) and return its character counts.
-- | Counts every occurrences of letters in the string and return it as Map.
wordCharCounts :: String        -- Input1: Word
               -> Map Char Int  -- Output: Character count
wordCharCounts word = fromList (map (\y -> (y, length (filter (\x -> x == y ) lowerCaseWord))) lowerCaseWord)
    where 
        lowerCaseWord = map toLower word


{-
    Function composition used for this function.
    1) map wordCharCounts --> Char count calculated for each word.
    2) map toList         --> List of Map converted to List of List to use list operations on.
    3) concat             --> Create one list from list of list. = [[('f',1), ('a', 2)], [('f', 2)]] -->[('f',1), ('a', 2), ('f', 2)]
    4) fromListWith (+)   --> Create a Map from list by using (+) operator, this operators adds value if the keys are same
-}
-- | This function takes word list and return its character counts.
sentenceCharCounts :: [String]      -- Input1: Word of sentence
                   -> Map Char Int  -- Output: Char count of sentence
sentenceCharCounts =  fromListWith (+) . concat . (map toList) . (map wordCharCounts)


-- | This function takes dictionary words and calculates character count for every word. Return map of words to character count.
dictCharCounts :: [String]                      -- Input1: Words from dictionary
               -> Map [Char] (Map Char Int)     -- Output: Words to Count map for all words in the dictionary
dictCharCounts = fromList . map (\y -> (y, wordCharCounts y))   -- For every word in input it calculates the count and map to word.



-- | This function takes dictionary words and their counts as map.
-- | Checks every counts and concatenates similar counts by using their word as map.
dictWordsByCharCounts :: Map [Char] (Map Char Int)      -- Input1: Dictionary words and their counts as map
                      -> Map (Map Char Int) [String]    -- Output: Concatenated counts and related dictionary words as map
dictWordsByCharCounts dcs = dictWordsByCharCounts' (toList dcs) empty -- Start recursive calls with empty map and dictionary words.
    where
        dictWordsByCharCounts' :: [([Char], Map Char Int)]      -- Input1: Dictionary words and their counts as list
                               -> Map (Map Char Int) [[Char]]   -- Input2: Accumulator value (Result) for recursive calls
                               -> Map (Map Char Int) [[Char]]   -- Output: Concatenated counts and related dictionary words as map
        dictWordsByCharCounts' listDcs acc
            | null listDcs  = acc           -- If list is empty, return the result
            | otherwise     = dictWordsByCharCounts' nextListDcs nextAcc    -- Recursive call, with updated list and acc.
            where
                (key, value) = head listDcs         -- Get key and value pair of current element
                nextListDcs  = drop 1 listDcs       -- Drop first item of list
                -- Since insertion operation is doing with list ([key]), it concatenates the words as list.
                nextAcc      = insertWith (++) value [key] acc  -- Use Insert function of Data.Map to insert word and count.
                                                                
