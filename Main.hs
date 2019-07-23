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
import Cop
import Cars
import EndScreen

main = do
    prepareConsole
    drawRoad 0 (screenWidth-1) screenHeight
    tickPeriod <- return (10^5)
    loop playerCar (repeat 20) Nothing [] 0 tickPeriod


loop playerCar positionHistory cop incomingCars time tickPeriod = do
    applySGRToCell [] 0 screenWidth
    putStr "score: "
    print time
    incomingCars <- updateIncomingCars incomingCars time playerCar
    playerCar <- readInputAndUpdatePlayerPosition playerCar tickPeriod
    positionHistory <- return $ (carColumn playerCar):positionHistory
    (cop,incomingCars) <- updateCop cop positionHistory time incomingCars
    loop playerCar positionHistory cop incomingCars (time + 1) (tickPeriod -150)

readInputAndUpdatePlayerPosition playerCar tickPeriod = do
    mvar <- newEmptyMVar
    id1 <- forkIO (readInput mvar)
    id2 <- forkIO (waitTick mvar tickPeriod)
    c <- takeMVar mvar
    killThread id1
    killThread id2
    playerCar <- movePlayer playerCar (if c == 'a' then -2 else if c=='d' then 2 else 0)
    return playerCar


waitTick mvar tickPeriod = do
    threadDelay tickPeriod
    putMVar mvar '\0'

readInput mvar = do
    c <- getChar
    putMVar mvar c

movePlayer currentPlayerCar delta =
    let maybeNewPlayerCar = Car {
                carRow    = carRow    currentPlayerCar,
                carColumn = carColumn currentPlayerCar + delta,
                carColor  = carColor  currentPlayerCar
        }
        actualNewPlayerCar =
            if 1 < carColumn maybeNewPlayerCar && carColumn maybeNewPlayerCar < screenWidth - 2 then
                maybeNewPlayerCar
            else
                currentPlayerCar
    in do
        eraseCar currentPlayerCar
        drawCar actualNewPlayerCar
        return  actualNewPlayerCar

