module IsbnVerifier (isbn, parseISBN) where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Char (isDigit, digitToInt)
import Text.ParserCombinators.ReadP

------------------------------------------------------------------------------
-- ISBN String Parser:
------------------------------------------------------------------------------

isbnParser :: ReadP [Int]
isbnParser = do
  d1 <- digitParser
  _ <- optional $ char '-'
  d2 <- digitParser
  d3 <- digitParser
  d4 <- digitParser
  _ <- optional $ char '-'
  d5 <- digitParser
  d6 <- digitParser
  d7 <- digitParser
  d8 <- digitParser
  d9 <- digitParser
  _ <- optional $ char '-'
  d10 <- checkDigitParser
  _ <- eof -- Reject inputs larger than valid ISBNs.
  return [d1, d2, d3, d4, d5, d6, d7, d8, d9, d10]
  where
    digitParser :: ReadP Int
    digitParser = digitToInt <$> satisfy isDigit
    xParser :: ReadP Int
    xParser = const 10 <$> char 'X'
    checkDigitParser :: ReadP Int
    checkDigitParser = digitParser <|> xParser

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

parseISBN :: String -> Maybe [Int]
parseISBN = parseMaybe isbnParser

------------------------------------------------------------------------------
-- ISBN checking.
------------------------------------------------------------------------------

checkISBN :: [Int] -> Bool
checkISBN = (== 0) .  (`mod` 11) . sum . map (uncurry (*)) . zip coefs
  where
    coefs :: [Int]
    coefs = reverse [1..10]

-- I really like the use of `fromMaybe False` here.
isbn :: String -> Bool
isbn code = fromMaybe False $ checkISBN <$> parseISBN code
