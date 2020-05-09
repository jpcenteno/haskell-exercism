{-# LANGUAGE OverloadedStrings #-}
module Isogram (isIsogram) where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Char (toLower, isLetter)

prepareString :: String -> String
prepareString = map toLower . filter isLetter

isIsogramAux :: String -> Set Char -> Bool
isIsogramAux ""     _   = True
isIsogramAux (c:cs) set
  | Set.member c set = False
  | otherwise        = isIsogramAux cs $ Set.insert c set

isIsogram :: String -> Bool
isIsogram cs = isIsogramAux (prepareString cs) Set.empty
