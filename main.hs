import System.IO
import System.Console.ANSI
import GHC.Conc
import Control.Concurrent

screenWidth  = 40
screenHeight = 43
tickPeriod   = 9^5    -- in milliseconds

playerCar = Car {
    carRow    = 28,
    carColumn = 20,
    carColor  = Red
}

main = do
    prepareConsole
    drawRoad 0 (screenWidth-1) screenHeight
    loop playerCar [] 0

loop playerCar incomingCars time = do
    incomingCars <- updateIncomingCars incomingCars time
    playerCar <- readInputAndUpdatePlayerPosition playerCar
    loop playerCar incomingCars (time + 1)

updateIncomingCars incomingCars time = do
    mapM eraseCar incomingCars
    incomingCars <- return (updateIncomingCarPositions incomingCars)
    incomingCars <- return (maybeSpawnANewIncomingCar incomingCars time)
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
