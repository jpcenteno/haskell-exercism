{-# LANGUAGE OverloadedStrings #-}

module ResistorColors (Color(..), Resistor(..), label, ohms) where

import qualified Data.Text as T
import           Data.Text (Text)

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

-- | Measurement constants:

kilo :: Int
kilo = 1000

mega :: Int
mega = 1000 * kilo

giga :: Int
giga = 1000 * mega

-- | Decode color into it's numerical value.
colorValue :: Color -> Int
colorValue Black  = 0
colorValue Brown  = 1
colorValue Red    = 2
colorValue Orange = 3
colorValue Yellow = 4
colorValue Green  = 5
colorValue Blue   = 6
colorValue Violet = 7
colorValue Grey   = 8
colorValue White  = 9

label :: Resistor -> Text
label resistor
  | resistorOhms < kilo = toText resistorOhms              <> " ohms"
  | resistorOhms < mega = toText (resistorOhms `div` kilo) <> " kiloohms"
  | resistorOhms < giga = toText (resistorOhms `div` mega) <> " megaohms"
  | otherwise           = toText (resistorOhms `div` giga) <> " gigaohms"
  where
    resistorOhms :: Int
    resistorOhms = ohms resistor
    toText :: Show a => a -> Text
    toText = T.pack . show

ohms :: Resistor -> Int
ohms resistor = (colorValue a * 10 + colorValue b) * offset
  where
    (a, b, c) = bands resistor
    offset = 10 ^ colorValue c
