module LeapYear (isLeapYear) where


-- | Return true if `n` is evenly divisible by `n`.
divides :: Integer -> Integer -> Bool
divides d n = n `mod` d == 0

isDivisibileBy400 :: Integer -> Bool
isDivisibileBy400 = divides 400

isNotDivisibileBy100 :: Integer -> Bool
isNotDivisibileBy100 = not . divides 100

isDivisibileBy4 :: Integer -> Bool
isDivisibileBy4 = divides 4

-- | Return true if `year` is a leap year in the gregorian callendar.
--
-- 1. On every year that is evenly divisible by 4
-- 2. Except every year that is evenly divisible by 100
-- 3. Unless the year is also evenly divisible by 400
--
--Obs: (3) => (1)
isLeapYear :: Integer -> Bool
isLeapYear year =  (isDivisibileBy400 year)
                || ( isDivisibileBy4 year && isNotDivisibileBy100 year )
