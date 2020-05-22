module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs
  | hasInvalidChars xs                = Nothing
  | length noCountryCode /= 10        = Nothing
  | invalidAreaCode noCountryCode     = Nothing
  | invalidExchangeCode noCountryCode = Nothing
  | otherwise                         = Just $ noCountryCode
  where
    noCountryCode = removeCountryCode . filter isDigit $ xs

-- | Remove the country code from a telephone number. Assumes the string
-- contains just digits.
removeCountryCode :: String -> String
removeCountryCode ('1':ds) = ds
removeCountryCode ds       = ds

-- | Returns True if the area code is invalid for the NANP. Assumes that the
-- string contains only numbers and no country code.
invalidAreaCode :: String -> Bool
invalidAreaCode ds = (ds !! 0 == '0') || (ds !! 0 == '1')

-- | Returns True if the exchange code is invalid for NANP. Assumes that the
-- string contains only numbers and no country code.
invalidExchangeCode :: String -> Bool
invalidExchangeCode ds = (ds !! 3 == '0') || (ds !! 3 == '1')

-- | Returns True if the string contains invalid characters for a typed number.
hasInvalidChars :: String -> Bool
hasInvalidChars = not . all validChar
  where
    validChar :: Char -> Bool
    validChar c = elem c ("0123456789-() .+" :: String)
