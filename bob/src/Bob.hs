module Bob (responseFor) where

import Data.Char (isUpper, isAlpha, isSpace)

isQuestion :: String -> Bool
isQuestion = (== '?') . last . filter (not . isSpace)

isYelling :: String -> Bool
isYelling xs = (not . null) onlyAlphabetical && all isUpper onlyAlphabetical
  where
    onlyAlphabetical = filter isAlpha xs

-- | Returns true if the string is empty or just whitespace.
doesNotSayAnything :: String -> Bool
doesNotSayAnything = all isSpace

isYellingAQuestion :: String -> Bool
isYellingAQuestion xs = isYelling xs && isQuestion xs

-- | Returns bob's response.
-- 1. Bob answers 'Sure.' if you ask him a question, such as "How are you?".
-- 2. He answers 'Whoa, chill out!' if you YELL AT HIM (in all capitals).
-- 3. He answers 'Calm down, I know what I'm doing!' if you yell a question at him.
-- 4. He says 'Fine. Be that way!' if you address him without actually saying anything.
-- 5. He answers 'Whatever.' to anything else.
responseFor :: String -> String
responseFor xs
  | doesNotSayAnything xs = "Fine. Be that way!"
  | isYellingAQuestion xs = "Calm down, I know what I'm doing!"
  | isQuestion xs         = "Sure."
  | isYelling xs          = "Whoa, chill out!"
  | otherwise             = "Whatever."
