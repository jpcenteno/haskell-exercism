module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

-- Turnable ------------------------------------------------------------------

-- | A type that can be turned left or right.
class Turnable a where
    turnRight :: a -> a
    turnLeft  :: a -> a

-- Bearing -------------------------------------------------------------------

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

instance Turnable Bearing where
    turnRight North = East
    turnRight East  = South
    turnRight South = West
    turnRight West  = North
    turnLeft East  = North
    turnLeft South = East
    turnLeft West  = South
    turnLeft North = West

-- Robot ---------------------------------------------------------------------

data Robot = Robot { bearing     :: Bearing
                   , coordinates :: (Integer, Integer)}
                   deriving (Show)

instance Turnable Robot where
    turnRight robot = robot { bearing = turnRight $ bearing robot }
    turnLeft  robot = robot { bearing = turnLeft  $ bearing robot }

-- | Move the robot forward.
advance :: Robot -> Robot
advance Robot{bearing=North, coordinates=(x, y)} = Robot North (x    , y + 1)
advance Robot{bearing=East,  coordinates=(x, y)} = Robot East  (x + 1, y    )
advance Robot{bearing=South, coordinates=(x, y)} = Robot South (x    , y - 1)
advance Robot{bearing=West,  coordinates=(x, y)} = Robot West  (x - 1, y    )

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot -- Use the constructor

performInstruction :: Char -> Robot -> Robot
performInstruction 'R' = turnRight
performInstruction 'L' = turnLeft
performInstruction 'A' = advance
performInstruction _   = error "unsupported instruction"

move :: Robot -> String -> Robot
move robot instructions = foldl (flip performInstruction) robot instructions
