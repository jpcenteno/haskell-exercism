module Acronym (abbreviate) where

import Data.Char (isLetter, isUpper, toUpper)

-- I started with a naive solution:
--
-- abbreviate = map toUpper . map head . String.splitWords
--
-- But failed with "GNU Image Manipulation Program => GIMP"
-- I'm thinking of constructuing the solution using a FSM.

-- RULES:
-- I.   First character is allways an initail.
-- II.  If prev character was space, the character will be an initial.
-- III. If char isUpper and prev isLowwer, the char will be an initial.
-- IV.  If prev char was an initial, the next can't be an initial.
-- V.  Slashes (-) behave the same as spaces.
-- VI.  Apostrophes and Underscores are ignored.
--
-- Ideas:
-- ------
--
-- 1. This could be implemented with a sliding window algorithm that also takes
-- the previous char in order to decide if it is an initial for the acronym or
-- not. On the con side, this would lead to a function with lots of cases using
-- both pattern matching and guards.
--
-- 2. This could be implemented as a FSM implemented using recursive functions.
-- The function processing the current character knows which case to call next.
-- This may lead to lots of functions with weird relations, this might get
-- complex anyway.
--
-- Implementation
-- --------------
--
-- I went with the 2nd option and I think the result is okay. I liked the idea
-- of implementing a FSM using recursive functions.

abbreviate :: String -> String
abbreviate = map toUpper . fsmCharMustBeInitial
  where

    isDashOrSpace :: Char -> Bool
    isDashOrSpace c = c == ' ' || c == '-'

    -- Ensures that the next character will be an initial.
    fsmCharMustBeInitial :: String -> String
    fsmCharMustBeInitial "" = ""
    fsmCharMustBeInitial (c:cs)
      | isLetter c = c : fsmCharCanNotBeInitial cs
      | otherwise  =     fsmCharMustBeInitial cs

    -- first character of the string CAN'T be an initial.
    -- State will change when:
    -- 1. A space ' ' or dash '-' char is met => Next char must be initial.
    fsmCharCanNotBeInitial :: String -> String
    fsmCharCanNotBeInitial "" = ""
    fsmCharCanNotBeInitial (c:cs)
      | isDashOrSpace c = fsmCharMustBeInitial cs
      | isUpper c       = fsmCharCanNotBeInitial cs
      | otherwise       = fsmCharCouldBeInitial cs

    -- Return the char as initial if and only if it's uppercase.
    fsmCharCouldBeInitial :: String -> String
    fsmCharCouldBeInitial "" = ""
    fsmCharCouldBeInitial (c:cs)
      | isDashOrSpace c =     fsmCharMustBeInitial cs
      | isUpper c       = c : fsmCharCanNotBeInitial cs
      | otherwise       =     fsmCharCouldBeInitial cs
