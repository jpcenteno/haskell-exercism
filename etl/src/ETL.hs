module ETL (transform) where

import Data.Map (Map, fromList, toList)
import Data.Char (toLower)


transformTuple :: a -> String -> [(Char, a)]
transformTuple a = map (\c -> (toLower c, a))

transform :: Map a String -> Map Char a
-- transform legacyData = undefined
transform = fromList . concat . map (uncurry transformTuple) . toList
