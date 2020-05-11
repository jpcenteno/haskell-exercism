module ResistorColors (Color(..), value) where

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
  deriving (Eq, Show)

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

value :: (Color, Color) -> Int
value (a, b) = colorValue a * 10 + colorValue b
