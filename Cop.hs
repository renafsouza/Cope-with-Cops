module Cop where

import System.IO
import System.Console.ANSI
import GHC.Conc
import Control.Concurrent
import Data.List
import System.Random
import System.Exit
import Data.Maybe

import Constants
import Rendering
import Types
import Fire
import Cars
import EndScreen
    
updateCop Nothing positionHistory time incomingCars = do
    copCar <- drawCop positionHistory (screenHeight - carRow playerCar - 3) time
    return (copCar,incomingCars)

updateCop (Just copCar) positionHistory time incomingCars = do
    collidingCar <- return $ find (checkCollision copCar) incomingCars
    eraseCar copCar
    if isJust collidingCar
        then do
            eraseCar (fromJust collidingCar)
            incomingCars <- return (incomingCars \\ [fromJust collidingCar])
            explode (carRow copCar) ((carColumn copCar)-2) 6 10 
            return (Nothing, incomingCars)
    else if newDistance<3 then do
        gameOver time copCar
    else do
        copCar <- drawCop positionHistory newDistance time
        return (copCar, incomingCars)
    where 
        newDistance = carRow copCar - carRow playerCar - (if time `mod` copVerticalMovementPeriod == 0 then 1 else 0)
    

drawCop positionHistory distance time =
    let
        copCar = Car {
            carRow    = carRow playerCar + distance,
            carColumn = positionHistory !! distance,
            carColor = White
        }
    in do
        drawCar copCar
        if time `mod` 2 == 1 then do
            applySGRToCell [SetColor Background Vivid Red] (carRow copCar + 1) (carColumn copCar)
            applySGRToCell [SetColor Background Vivid Blue] (carRow copCar + 1) (carColumn copCar + 1)
        else do
            applySGRToCell [SetColor Background Vivid Blue] (carRow copCar + 1) (carColumn copCar)
            applySGRToCell [SetColor Background Vivid Red] (carRow copCar + 1) (carColumn copCar + 1)
        return (Just copCar)
