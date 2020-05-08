module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

-- | Number of seconds in a year.
year :: Float
year = 31557600.0

-- | Return the orbital period for a planet.
orbitalPeriod :: Planet -> Float
orbitalPeriod Mercury = 0.2408467
orbitalPeriod Venus = 0.61519726
orbitalPeriod Earth = 1.0
orbitalPeriod Mars = 1.8808158
orbitalPeriod Jupiter = 11.862615
orbitalPeriod Saturn = 29.447498
orbitalPeriod Uranus = 84.016846
orbitalPeriod Neptune = 164.79132

earthYears :: Float -> Float
earthYears seconds = seconds / year

ageOn :: Planet -> Float -> Float
ageOn planet seconds = earthYears seconds / orbitalPeriod planet
