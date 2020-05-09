module Isogram (isIsogram) where

import Data.Char (toLower, isLetter)

prepareString :: String -> String
prepareString = filter isLetter . map toLower

isIsogramAux :: String -> Bool
isIsogramAux []     = True
isIsogramAux (c:cs) = (not . elem c $ cs) && isIsogram cs

isIsogram :: String -> Bool
isIsogram cs = isIsogramAux . prepareString $ cs
