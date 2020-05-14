module School (School, add, empty, grade, sorted) where

import           Data.List (sort)
import qualified Data.MultiMap as MultiMap
import           Data.MultiMap (MultiMap)

type Grade = Int
type Student = String
newtype School = School (MultiMap Grade Student)

add :: Grade -> Student -> School -> School
add gradeNum student (School m) = School $ MultiMap.insert gradeNum student m

empty :: School
empty = School $ MultiMap.empty

grade :: Int -> School -> [Student]
grade gradeNum (School m) = sort $ MultiMap.lookup gradeNum m

sorted :: School -> [(Int, [Student])]
sorted (School m) = map (fmap sort) $ MultiMap.assocs m
