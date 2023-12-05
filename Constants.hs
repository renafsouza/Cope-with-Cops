module Constants where

import Types
import System.Console.ANSI

screenWidth, screenHeight, copVerticalMovementPeriod, incomingCarSpeed, carFrequency :: Int
screenWidth  = 40
screenHeight = 43
copVerticalMovementPeriod = 8       -- in ticks
incomingCarSpeed = 2                -- in cells per tick
carFrequency = 15                   -- as a percentage
playerCar = Car {
    carRow    = 28,
    carColumn = 20,
    carColor  = Red
}
