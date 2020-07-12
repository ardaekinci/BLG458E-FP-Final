-- Github Link Of Project: https://github.com/ardaekinci/BLG458E-FP-Final

{- 
    According to Zeller's congruence January and February are counted as months 13 and 14 of the previous year.
-}
-- | This function calculates the year and the month for Zeller's congruence
calculateMonthAndYear   :: (Integer, Integer)     -- Input1: Tuple of year and month (Year, Month)
                        -> (Integer, Integer)     -- Output: Updated year and month
calculateMonthAndYear (y, m)
    | m <= 2 = (y - 1, m + 12)  -- If the given month is January or February, add 12 to month and adjust year as previous year.
    | otherwise = (y, m)        -- If the given month is different than Jan or Feb, return.

-- | This function calculates the day of the week for given date.
dayOfWeek   :: Integer     -- Input1: Year
            -> Integer     -- Input2: Month
            -> Integer     -- Input3: Day
            -> Integer     -- Output: Day of the week (0 = Saturday, 1 = Sunday, 2 = Monday, ..., 6 = Friday)
dayOfWeek year month day = (day + t1 + k + t2 + t3 + 5 * j) `mod` 7
    where   (y, m) = calculateMonthAndYear (year, month)    -- Calculate year and month
            j = floor (fromIntegral y / 100)                -- Zero based century
            k = fromIntegral y `mod` 100                    -- The year of the century
            -- Extract values for Zeller's congruence
            t1 = floor (fromIntegral (13 * (m + 1)) / 5.0)
            t2 = floor (fromIntegral k / 4)
            t3 = floor (fromIntegral j / 4)


{-
    1) What does the helper function (sundays') calculate?
    Calculates the number of months that starts with sunday between two years recursively.
    Also, This function checks the first day of every month.

    2) What if you don't define a "rest" and use its expression where it's needed?
-}

-- | This function calculates the number of months that starts with sunday between two years.
sundays1    :: Integer  -- Input1: Start year
            -> Integer  -- Input2: End year
            -> Integer  -- Output: Total number of months that starts with sunday between start and end.
sundays1 start end = sundays' start 1   -- Initialize first call with start year and first month
    where
        -- | Recursive function to calculate result.
        sundays' :: Integer -- Input1: Start year
                 -> Integer -- Input2: Start month
                 -> Integer -- Output: Number of month that start with sunday 
        sundays' y m
            | y > end = 0   -- If the current year is higher than end year, return 0.
            | otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest -- If the month start with sunday increment result.
            where
                nextY = if m == 12 then y + 1 else y -- If current month is the last month of the year increment year.
                nextM = if m == 12 then 1 else m + 1 -- If current month is the last month set month as 1 to start for new year.
                rest = sundays' nextY nextM          -- Call recursively to calculate result for next date.


-- | Tail recursive version of sundays1
sundays1tr  :: Integer  -- Input1: Start year
            -> Integer  -- Input2: End year
            -> Integer  -- Output: Total number of months that starts with sunday between start and end.
sundays1tr start end = sundaystr' 0 start 1   -- Initialize first call with start year and first month, and accumulator value as 0
    where
        -- | Tail recursive function to calculate result.
        sundaystr' :: Integer -- Input1: Accumulator (Result stored in the acc for tail recursive function)
                   -> Integer -- Input2: Start year
                   -> Integer -- Input3: Start month
                   -> Integer -- Output: Number of month that start with sunday 
        sundaystr' acc y m
            | y > end = acc   -- If the current year is higher than end year, return value that stored in the acc.
            | otherwise = sundaystr' nextAcc nextY nextM -- Call function recursively with next accumulator value.
            where
                nextY = if m == 12 then y + 1 else y -- If current month is the last month of the year increment year.
                nextM = if m == 12 then 1 else m + 1 -- If current month is the last month set month as 1 to start for new year.
                nextAcc = if dayOfWeek y m 1 == 1 then acc + 1 else acc -- Calculate next accumulator value.


-- To check what is leap year --> https://en.wikipedia.org/wiki/Leap_year
-- | This function checks the year is leap year or not.
leap :: Integer -- Input1: Year
     -> Bool    -- Output: Boolean, if year is leap year it returns true otherwise false 
leap year
    | year `mod` 400 == 0 = True -- If year is divisible by 400 return true
    | year `mod` 4 == 0 && year `mod` 100 /= 0 = True -- If year is divisible by 4 but not divisible by 100 return true
    | otherwise = False 


monthsThirtyDays = [4, 6, 9, 11] -- Month indexes that has 30 days.
-- | This function calculates the number of days for given month. 
daysInMonth :: Integer  -- Input1: Month
            -> Integer  -- Input2: Year
            -> Integer  -- Output: Number of days in given (Year, Month)
daysInMonth month year 
    | month == 2 = if leap year then 29 else 28 -- If the given Month is February check the year is leap year and return 29, otherwise 28.
    | elem month monthsThirtyDays = 30          -- If monthsThirtyDays inclues the given month return 30.
    | otherwise = 31                            -- Return 31 for remaining months.


{-
    Unlike sunday1 function, this function first calculates the number of days in the month.
    Since every 7 days the same day occurs, it divides the month week by week and calculates the remaining days in the month.
    Instead of calculating the day of week for every time, this function keep track of the day of week.
-}
-- | This function calculates the number of months that starts with sunday between two years.
sundays2    :: Integer  -- Input1: Start year
            -> Integer  -- Input2: End year
            -> Integer  -- Output: Total number of months that starts with sunday between start and end.
sundays2 start end = sundays2' start 1 (dayOfWeek start 1 1) -- Initialize first call with start year, first month and day of week
    where
        -- | Recursive function to calculate result.
        sundays2' :: Integer -- Input1: Start year
                  -> Integer -- Input2: Start month
                  -> Integer -- Input3: Initial day of week
                  -> Integer -- Output: Number of month that start with sunday 
        sundays2' y m dow
            | y > end = 0   -- If the current year is higher than end year, return 0.
            | otherwise = if dow `mod` 7 == 1 then rest + 1 else rest -- If the month start with sunday increment result.
            where
                nextY = if m == 12 then y + 1 else y -- If current month is the last month of the year increment year.
                nextM = if m == 12 then 1 else m + 1 -- If current month is the last month set month as 1 to start for new year.
                days = daysInMonth m y               -- Days in the current month
                nextDow = dow + (days `mod` 7)       -- Calculate next day of week
                rest = sundays2' nextY nextM nextDow -- Call function recursively to calculate result for next date.


-- | Get function by name and pass the params
getFunction :: String -> (Integer -> Integer -> Integer)
getFunction "sundays1"   = sundays1
getFunction "sundays1tr" = sundays1tr
getFunction "sundays2"   = sundays2
getFunction _            = error "unknown function"

-- | Calculate and display the result by function name
main :: IO ()
main = do
    line <- getLine
    let [f, start, end] = words line
    putStrLn $ show $ (getFunction f) (read start :: Integer) (read end :: Integer)