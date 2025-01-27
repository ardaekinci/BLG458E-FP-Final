-- Github Link Of Project: https://github.com/ardaekinci/BLG458E-FP-Final

{-----------------------------------------------------------------------------------------
    References for Data.Map:
    https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html#g:9
    I read this documentation to implement Data.Map and functionalities.
-----------------------------------------------------------------------------------------}
-- Used to store character counts and map operations.
import Data.Map (Map, fromList, toList, fromListWith, insertWith, empty, findWithDefault)
-- Used for toLower, char operations on string.
import Data.Char (toLower) 
-- Commandline arguments
import System.Environment (getArgs) 
-- Remove same element from subset
-- Sort is used to sort result to test by calico
import Data.List (nub, sort)


{-------------------------------------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------------------------------------}
-- | This function takes a word (String) and return its character counts.
-- | Counts every occurrences of letters in the string and return it as Map.
wordCharCounts :: String        -- Input1: Word
               -> Map Char Int  -- Output: Character count
wordCharCounts word = fromList (map (\y -> (y, length (filter (\x -> x == y ) lowerCaseWord))) lowerCaseWord)
    where 
        lowerCaseWord = map toLower word


{----------------------------------------------------------------------------------------------------------------------------------
    Function composition used for this function.
    1) map wordCharCounts --> Char count calculated for each word.
    2) map toList         --> List of Map converted to List of List to use list operations on.
    3) concat             --> Create one list from list of list. = [[('f',1), ('a', 2)], [('f', 2)]] -->[('f',1), ('a', 2), ('f', 2)]
    4) fromListWith (+)   --> Create a Map from list by using (+) operator, this operators adds value if the keys are same
----------------------------------------------------------------------------------------------------------------------------------}
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


{----------------------------------------------------------------------------------------------------------------------------------
    This method implemented as an Alternative solution for wordAnagrams method.
    If original method is not accepted, this method can be use instead of `wordAnagrams`
    Inputs and Outputs are same, this function uses only list operations. 
    On the other hand original function uses the function from Data.Map
    
    Example usage of wordAnagrams and wordAnagramsByList functions
    ghci>   k
    output> fromList [(fromList [('a',1),('b',1),('c',1)],["cba","abc"]),(fromList [('d',1),('e',1),('f',1)],["def"])]
    ghci>   wordAnagrams "bac" k
    output> ["cba","abc"]
    ghci>   wordAnagramsByList "bac" k
    output> ["cba","abc"]
    ghci>   wordAnagramsByList "xxx" k
    output> []
    ghci>   wordAnagrams "xxx" k
    output> []
----------------------------------------------------------------------------------------------------------------------------------}
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
        mapOfWordsInList = map (\y -> (toList (fst y), snd y)) (toList mapOfWords)   -- Convert maps and inner maps to list.
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


-- | Instead of converting count to word for wordAnagrams function, this method takes count as params and return anagrams.
wordAnagramsByCount :: Map Char Int                    -- Input1: Char Count of word
                    -> Map (Map Char Int) [[Char]]     -- Input2: Map of Dictionary Words
                    -> [[Char]]                        -- Output: Matched words
-- findWithDefault (default value) (key) (map)
-- if given key is not found on map it returns default value ([]), otherwise returns founded value               
wordAnagramsByCount count mapOfWords = findWithDefault [] count mapOfWords


-- | This function takes character count as input, and generates all subsets of map.
charCountsSubsets :: (Map Char Int)     -- Input1: Character count
                  -> [(Map Char Int)]   -- Output: Subsets of given character counts
-- Create subsets of counts, remove same elements from list.
charCountsSubsets mapOfCharCount = nub (map (fromListWith (+)) (charCountsSubsets' seperatedCharCounts))
    where
        -- Convert map to list
        charCountInList     = toList mapOfCharCount
        {---------------------------------------------------------------------------------------
            Seperate the char counts if the value of char is more than one. E.g For word "all".
            [[('a',1),('l',2)]] --> [('a',1),('l',1),('l',1)]
            first map creates same tuple using number of occurences of char.
            second map runs the first function for every char count. Every (key, value) pair.
            concat converts list in list to list. 
        ---------------------------------------------------------------------------------------}
        seperatedCharCounts = concat (map (\y-> map (\x-> (fst y, 1) ) [1..(snd y)] ) charCountInList)
        charCountsSubsets' :: [(Char, Int)]     -- Input1: Char count as list
                           -> [[(Char, Int)]]   -- Output: Subsets of char count list
        charCountsSubsets' [] = [[]] -- If list is empty return empty subset
        {---------------------------------------------------------------------------------------
            Seperate list by first element and remaining elements.
            Left Part, Call recursively remaining elements to generate subsets from empty.
            Second Part, Call recursively remaining elements, after reaching the empty state
            add first element from left side to list then construct the remaining branches.
            References: https://stackoverflow.com/questions/19772427/haskell-generate-subsets
        ---------------------------------------------------------------------------------------}
        charCountsSubsets' (x:xs) = charCountsSubsets' xs ++ map (x:) (charCountsSubsets' xs)


-- | This function takes 2 char count map, and calculate the differences.
-- | Assuming that second param always subset of first param.
substractCounts :: (Map Char Int)   -- Input1: First Char Count as Map
                -> (Map Char Int)   -- Input2: Second Char Count as Map
                -> (Map Char Int)   -- Output: Differences between first and second maps.
-- 1) Concatenate two maps as list. ---> ((toList first) ++ (toList second))
-- 2) Convert merged list to a map by substracting values of same keys. ---> fromListWith (-)
-- 3) Filter the remaining elements, get the element if count is not zero ---> filter(\x -> snd x /= 0)
-- 4) Convert filtered list to char count map --> fromList
substractCounts first second = fromList (filter(\x -> snd x /= 0) (toList (fromListWith (-) ((toList second) ++ (toList first)))))


-- | This function takes sentence and mapped words by char count, returns the anagram list of given sentence.
sentenceAnagrams :: String                      -- Input1: Sentence
                 -> Map (Map Char Int) [String] -- Input2: Mapped words by char count
                 -> [String]                    -- Output: Anagrams of sentence
-- Initialize helper function with char count and subset.
sentenceAnagrams sentence mapOfWords = sentenceAnagrams' charCounts (charCountsSubsets charCounts)
    where
        -- Get char count of first sentence, words function creates list from the sentence by seperating with white space
        charCounts = sentenceCharCounts (words sentence)
        sentenceAnagrams' :: Map Char Int       -- Input1: Char count
                          -> [Map Char Int]     -- Input2: Subset of char count
                          -> [String]           -- Output: Anagram sentences
        sentenceAnagrams' count subset
            | null count    = [""]      -- If the char count is empty, return empty string to concatenate with other words.
            | null subset   = []        -- If current subset is empty, return an empty list to ignore failed searches.
            -- If anagram founds for word, call function recursively. left branch is remaining subsets of currenct count
            -- Right branch is substracted count and anagram words.
            | not (null anagramWords)   = currentWord ++ concatResult   
            | otherwise     = currentWord   -- If there anagram is not found, just call current count with new selected subset.
            where
                selectedSubset  = head subset   -- Take first element from subset to search anagram word.
                -- Find anagram words that matches with selected count
                anagramWords    = wordAnagramsByCount selectedSubset mapOfWords
                -- Calculate next word count for recursive call, substract selected subset from count.
                nextWordCount   = substractCounts count selectedSubset
                -- Calculate recursive call for current word, do not change current count, get the rest elements from subset with tail.
                currentWord     = sentenceAnagrams' count (tail subset)
                -- Calculet recursive call for next word, make call with nextWord count and calculated subsets for new word.
                nextWord        = sentenceAnagrams' nextWordCount (charCountsSubsets nextWordCount)
                {----------------------------------------------------------------------------------------
                    This part calculates the next call for foundend anagrams. This function concats the
                    string for each call. It takes two list and combine these two list by taking every
                    possible combination.
                    ghci> nextWord = ["abc", "def"]
                    ghci> anagramWords = ["efg", "hfg"]
                    output> ["efg abc","efg def","hfg abc","hfg def"]
                -----------------------------------------------------------------------------------------}
                concatResult    = concat (map (\y-> map(\x -> y ++ " " ++ x) nextWord) anagramWords)


-- Get sentence from command line arguments
getSentenceFromArgs :: [String]    -- Input1: CLI Args
                    -> String      -- Output: Word
-- If there is only one args in the CLI args take it, otherwise throw error.
getSentenceFromArgs [sentence] = sentence
getSentenceFromArgs _          = error "Invalid args supplied."


-- | Take user input sentence and remove special characters and punctuation"
stripSentence :: String -- Input1: User input (Raw Sentence)
              -> String -- Output: Filtered string that contains only lowercase and uppercase characters
-- If char is in the valid char list, take it.
stripSentence rawSentence = filter (\x -> elem x validChars) rawSentence
    where
        -- Lower case + Upper Case + Whitespace
        validChars = ['a'..'z'] ++ ['A'..'Z'] ++ " "


main = do 
    -- Get command line arguments
    args <- getArgs
    -- Get sentence and filter
    let sentence = stripSentence (getSentenceFromArgs args)
    -- Read from file
    content <- readFile "words.txt"
    -- Get dictionary words as String list
    let dictWords = lines content
    -- Calculate char count for every word in the dictionary
    let charCountsOfDictWords = dictCharCounts dictWords
    -- Merge similar words according to their char counts
    -- Find all the words from the dictionary which have the same character list
    let mappedDictWordsByCount = dictWordsByCharCounts charCountsOfDictWords
    let anagramSentences = sentenceAnagrams sentence mappedDictWordsByCount
    -- If there is no anagram print message
    if (null anagramSentences || anagramSentences == [""])
        then
            putStrLn "There is no anagram in the dictionary"
        else
            -- Convert String array to string, unlines function combines the elements with '/n'
            -- Generate test cases to put output to yaml for calico.
            -- putStrLn (unlines (sort (map (\x-> "- expect: \"" ++ x ++ "\" # timeout: 60") anagramSentences)))
            putStrLn (unlines (sort anagramSentences))
            
    