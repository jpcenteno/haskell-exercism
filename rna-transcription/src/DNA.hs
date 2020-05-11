module DNA (toRNA) where

import Data.Either (partitionEithers)

complementChar :: Char -> Either Char Char
complementChar 'G' = Right 'C'
complementChar 'C' = Right 'G'
complementChar 'T' = Right 'A'
complementChar 'A' = Right 'U'
complementChar c   = Left c

checkComplementedChars :: [Either Char Char] -> Either Char String
checkComplementedChars es
  | null lefts = Right rights
  | otherwise  = Left . head $ lefts
  where
    (lefts, rights) = partitionEithers es

toRNA :: String -> Either Char String
toRNA xs = checkComplementedChars . map complementChar $ xs
