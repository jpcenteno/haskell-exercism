module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.List (group)
import Data.Time.Calendar (Day, addDays, toGregorian)
import qualified Data.Time.Calendar as Cal

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

isDayOfWeek :: Weekday -> Day -> Bool
isDayOfWeek Monday    day = Cal.dayOfWeek day == Cal.Monday
isDayOfWeek Tuesday   day = Cal.dayOfWeek day == Cal.Tuesday
isDayOfWeek Wednesday day = Cal.dayOfWeek day == Cal.Wednesday
isDayOfWeek Thursday  day = Cal.dayOfWeek day == Cal.Thursday
isDayOfWeek Friday    day = Cal.dayOfWeek day == Cal.Friday
isDayOfWeek Saturday  day = Cal.dayOfWeek day == Cal.Saturday
isDayOfWeek Sunday    day = Cal.dayOfWeek day == Cal.Sunday

monthDays :: Integer -> Int -> [Day]
monthDays year month = uniq $ map (Cal.fromGregorian year month) [1 .. 31]
  where
    uniq :: Eq b => [b] -> [b]
    uniq = map head . group

isTeenth :: Day -> Bool
isTeenth day = 13 <= monthDay && monthDay <= 20
  where (_, _, monthDay) = toGregorian day

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
-- meetupDay schedule weekday year month = error "You need to implement this function."
meetupDay First  weekday year month = head . filter (isDayOfWeek weekday) $ monthDays year month
meetupDay Second weekday year month = addDays 7  $ meetupDay First weekday year month
meetupDay Third  weekday year month = addDays 14 $ meetupDay First weekday year month
meetupDay Fourth weekday year month = addDays 21 $ meetupDay First weekday year month
meetupDay Last   weekday year month = last . filter (isDayOfWeek weekday) $ monthDays year month
meetupDay Teenth weekday year month = head . filter isTeenth . filter (isDayOfWeek weekday) $ monthDays year month
