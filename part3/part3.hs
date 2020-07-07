{-
    References for Data.Map:
    https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html#g:9
    I read this documentation to implement Data.Map and functionalities.
-}
-- Used to store character counts and map operations.
import Data.Map (Map, fromList, toList, fromListWith, insertWith, empty, findWithDefault)
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
                                                                
{-
    This method implemented as an Alternative solution for wordAnagrams method.
    If original method is not accepted, this method can be use instead of `wordAnagrams`
    Inputs and Outputs are same, this function uses only list operations. 
    On the other hand original function uses the function from Data.Map
    
    Example usage of wordAnagrams and wordAnagramsByList functions
    ghci>   k
    output> fromList [(fromList [('a',1),('b',1),('c',1)],["cba","abc"]),(fromList [('d',1),('e',1),('f',1)],["def"])]
    ghci>   wordAnagrams "bac" k
    output> ["cba","abc"]
    ghci>   wordAnagramsByList  "bac" k
    output> ["cba","abc"]
    ghci>   wordAnagramsByList "xxx" k
    output> []
    ghci>   wordAnagrams "xxx" k
    output> []

-}
-- | This function takes a word and a map that consist of counts of dictionary and related words. Returns matched words by counts.
wordAnagramsByList :: String                          -- Input1: Word
                   -> Map (Map Char Int) [[Char]]     -- Input2: Map of Dictionary Words
                   -> [[Char]]                        -- Output: Matched words
wordAnagramsByList word mapOfWords
    | null foundedWordsList = []    -- If there are no anagram word found for given word, return empty list.
    -- If only one anagram word found, take the first element and get anagram words by snd (count, words)
    | length foundedWordsList == 1 = snd (head foundedWordsList)
    -- Throw error for more than one element.
    | otherwise = error "Internal error, Given word matches with more than one count."
    where
        mapOfWordsInList = map (\y -> (toList (fst y), snd y)) (toList h)   -- Convert maps and inner maps to list.
        countsOfWordInList = toList (wordCharCounts word)                   -- Convert char counts of word to list.
        -- Filter the all words by checking first value of tuples, if the value matches with count of input word add to list. 
        foundedWordsList = filter (\x -> fst x == countsOfWordInList) mapOfWordsInList


-- | Alternative Function to wordAnagramsByList. Implemented using the function `findWithDefault` from Data.Map
wordAnagrams :: String                          -- Input1: Word
             -> Map (Map Char Int) [[Char]]     -- Input2: Map of Dictionary Words
             -> [[Char]]                        -- Output: Matched words
-- findWithDefault (default value) (key) (map)
-- if given key is not found on map it returns default value ([]), otherwise returns founded value               
wordAnagrams word mapOfWords = findWithDefault [] charCountsOfWord mapOfWords
    where
        charCountsOfWord = wordCharCounts word -- Get character count of given word as map