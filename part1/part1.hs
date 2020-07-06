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