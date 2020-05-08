module Pangram (isPangram) where

import Data.Char (toLower)

alphabet :: [Char]
alphabet = ['a' .. 'z']

containsChar :: String -> Char -> Bool
containsChar text = flip elem $ text

isPangram :: String -> Bool
isPangram text = all (containsChar $ lowercaseText) alphabet
  where lowercaseText = map toLower text
