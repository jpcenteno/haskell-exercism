module CollatzConjecture (collatz) where

nextCollatzNumber :: Integer -> Integer
nextCollatzNumber n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3 * n + 1

collatzAux :: Integer -> Integer -> Integer
collatzAux 0 _     = error ""
collatzAux 1 steps = steps
collatzAux n steps = collatzAux (nextCollatzNumber n) (steps + 1)

collatz :: Integer -> Maybe Integer
collatz n
  | n < 1     = Nothing
  | otherwise = Just $ collatzAux n 0
