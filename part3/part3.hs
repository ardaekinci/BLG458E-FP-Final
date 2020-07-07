import Data.Map (Map, fromList, toList, fromListWith, unionWith)      -- Used to store character counts.
import Data.Char (toLower) -- Used for toLower, char operations on string.

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
wordCharCounts :: String
               -> Map Char Int
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
sentenceCharCounts :: [String]
                   -> Map Char Int
sentenceCharCounts =  fromListWith (+) . concat . (map toList) . (map wordCharCounts)


