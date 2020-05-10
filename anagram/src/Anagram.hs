module Anagram (anagramsFor) where

import           Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import           Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (toList)

type CharCount = MultiSet Char

charCount :: Text -> CharCount
charCount = foldr MultiSet.insert MultiSet.empty . toList

areAnagrams :: Text -> Text -> Bool
areAnagrams s1 s2 =  s1' /= s2' && charCount s1' == charCount s2'
  where
    s1' = T.toLower s1
    s2' = T.toLower s2

anagramsFor :: Text -> [Text] -> [Text]
anagramsFor str = filter $ areAnagrams str
