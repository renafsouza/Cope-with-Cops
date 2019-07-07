import System.IO
import System.Console.ANSI
import GHC.Conc
import Control.Concurrent
import Data.List

screenWidth  = 40
screenHeight = 43
tickPeriod   = 9^5    -- in milliseconds
copVerticalMovementPeriod = 8       -- in ticks

playerCar = Car {
    carRow    = 28,
    carColumn = 20,
    carColor  = Red
}

main = do
    prepareConsole
    drawRoad 0 (screenWidth-1) screenHeight
    loop playerCar (repeat 20) Nothing [] 0

loop playerCar positionHistory cop incomingCars time = do
    incomingCars <- updateIncomingCars incomingCars time playerCar
    playerCar <- readInputAndUpdatePlayerPosition playerCar
    positionHistory <- return $ (carColumn playerCar):positionHistory
    cop <- updateCop cop positionHistory time
    loop playerCar positionHistory cop incomingCars (time + 1)

updateIncomingCars incomingCars time playerCar = do
    mapM eraseCar incomingCars
    incomingCars <- return (updateIncomingCarPositions incomingCars)
    incomingCars <- return (maybeSpawnANewIncomingCar incomingCars time)
    if or (map (checkCollision playerCar) incomingCars) then error "Game over: you crashed!" else return ()
    mapM drawCar incomingCars
    return incomingCars

maybeSpawnANewIncomingCar incomingCarList currentTime =
    if currentTime `mod` 5 == 0 then
        let
            newCar = Car {
                carRow = 0,
                carColumn = 1 + ((currentTime * 3) `mod` 39),
                carColor = [Black, Red, Green, Yellow, Blue, Cyan] !! ((currentTime `div` 5) `mod` 6)
            }
        in
            newCar:incomingCarList
    else
        incomingCarList

readInputAndUpdatePlayerPosition playerCar = do
    mvar <- newEmptyMVar
    id1 <- forkIO (readInput mvar)
    id2 <- forkIO (waitTick mvar)
    c <- takeMVar mvar
    killThread id1
    killThread id2
    playerCar <- movePlayer playerCar (if c == 'a' then -2 else if c=='d' then 2 else 0)
    return playerCar

updateIncomingCarPositions carList = map updateIncomingCar carList
    where updateIncomingCar car = Car {
            carRow    = carRow    car + 1,
            carColumn = carColumn car,
            carColor  = carColor  car
          }

updateCop Nothing       positionHistory time = do
    drawCop positionHistory (screenHeight - carRow playerCar - 2) time

updateCop (Just copCar) positionHistory time =
    let
        newDistance = carRow copCar - carRow playerCar - (if time `mod` copVerticalMovementPeriod == 0 then 1 else 0)
    in do
    eraseCar copCar
    drawCop positionHistory newDistance time
    

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
    

waitTick mvar = do
    threadDelay tickPeriod
    putMVar mvar '\0'

readInput mvar = do
    c <- getChar
    putMVar mvar c

movePlayer playerCar delta =
    let newPlayerCar = Car {
                carRow    = carRow    playerCar,
                carColumn = carColumn playerCar + delta,
                carColor  = carColor  playerCar
        }
    in do
    eraseCar playerCar
    drawCar newPlayerCar
    return  newPlayerCar

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

drawCar  car = applySGRToCarCells car [SetColor Background Vivid (carColor car)]
eraseCar car = applySGRToCarCells car []

applySGRToCarCells car sgrCommandList =
    let
        row    = carRow    car
        column = carColumn car
    in do
        applySGRToCell sgrCommandList  row       column
        applySGRToCell sgrCommandList (row + 1)  column
        applySGRToCell sgrCommandList (row + 2)  column
        applySGRToCell sgrCommandList  row      (column + 1)
        applySGRToCell sgrCommandList (row + 1) (column + 1)
        applySGRToCell sgrCommandList (row + 2) (column + 1)

applySGRToCell sgrCommandList row column = do
    setCursorPosition row column
    setSGR sgrCommandList
    putChar ' '


prepareConsole = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hideCursor
    clearScreen
    setCursorPosition 0 0

{- A função drawRoad desenha (= inverte as cores de) duas colunas
 - que recebe como argumentos.
 -}
drawRoad _       _       0      = return ()
drawRoad column1 column2 length = do
    threadDelay 10000
    colorCellAtColumn column1
    colorCellAtColumn column2
    cursorDownLine 1
    drawRoad column1 column2 (length - 1)
        where
            colorCellAtColumn column = do
                setCursorColumn column
                setSGR sgrCommandList
                putChar ' '
            sgrCommandList = [Reset, SetSwapForegroundBackground True]

data Car = Car {
    carRow    :: Int,
    carColumn :: Int,
    carColor  :: Color
}
