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
forward :: Robot -> Robot
forward Robot{bearing=North, coordinates=(x, y)} = Robot North (x    , y + 1)
forward Robot{bearing=East,  coordinates=(x, y)} = Robot East  (x + 1, y    )
forward Robot{bearing=South, coordinates=(x, y)} = Robot South (x    , y - 1)
forward Robot{bearing=West,  coordinates=(x, y)} = Robot West  (x - 1, y    )

-- | Return the resulting state of a single instruction.
performInstruction :: Robot -> Char -> Robot
performInstruction robot 'R' = turnRight robot
performInstruction robot 'L' = turnLeft robot
performInstruction robot 'A' = forward robot
performInstruction _     _   = error "unsupported instruction"

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot -- Use the constructor

move :: Robot -> String -> Robot
move robot instructions = foldl performInstruction robot instructions
