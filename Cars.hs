module Cars where

import System.IO
import System.Console.ANSI
import GHC.Conc
import Control.Concurrent
import Data.List
import System.Random
import System.Exit
import Data.Maybe

import Fire
import Rendering
import Constants
import Types
import EndScreen

updateIncomingCars incomingCars time playerCar = do
    if or (map (checkCollision playerCar) incomingCars) then do 
        gameOver time playerCar
    else return ()
    mapM eraseCar incomingCars
    incomingCars <- return (updateIncomingCarPositions incomingCars)
    incomingCars <- (maybeSpawnANewIncomingCar incomingCars time)
    mapM drawCar incomingCars
    return incomingCars

maybeSpawnANewIncomingCar incomingCarList currentTime = do
    rn <- randomChance
    j <- randomColumn 
    if rn<carFrequency then do
        return (newCar 0 (j*2) :incomingCarList)
    else do
        return incomingCarList
    where
        randomColumn = randomRIO (1, (quot screenWidth 2)-2) -- TODO: use the correct range
        randomChance = randomRIO (0, screenWidth) -- TODO: use range from 0 to 100
        newCar i j= Car {
                carRow = i,
                carColumn = j,
                carColor = [Black, Red, Green, Yellow, Blue, Cyan] !! (j `mod` 6)
            }

updateIncomingCarPositions [] = []
updateIncomingCarPositions (x:xs)
    | carRow x < screenHeight = (updateIncomingCar x) : (updateIncomingCarPositions xs)
    | otherwise               =                          updateIncomingCarPositions xs
    where updateIncomingCar car = Car {
            carRow    = carRow    car + incomingCarSpeed,
            carColumn = carColumn car,
            carColor  = carColor  car
          }

removeCollidingCars [] = return ()
removeCollidingCars ((isColliding,car):xs)
    | isColliding = do 
        eraseCar car
        removeCollidingCars xs
    | otherwise = removeCollidingCars xs    

checkCollision carA carB =
    let
        rowA    = carRow    carA
        columnA = carColumn carA
        rowB    = carRow    carB
        columnB = carColumn carB

        isSorted l = l == sort l
    in
        (isSorted [rowA, rowB, rowA+2] || isSorted [rowB, rowA, rowB+2]) &&
        (isSorted [columnA, columnB, columnA+1] || isSorted [columnB, columnA, columnB+1])
