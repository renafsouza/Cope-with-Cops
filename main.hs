import System.IO
import System.Console.ANSI
import GHC.Conc
import Control.Concurrent

screenWidth = 40
screenHeight = 30
playerRow = 20

main = do
    prepareConsole
    drawRoad 0 (screenWidth-1) screenHeight
    loop 20 [Car 0 15] 0

loop :: Int -> [Car] -> Int -> IO ()
loop playerColumn incomingCars time = do
    incomingCars <- updateIncomingCars incomingCars
    playerColumn <- readInputAndUpdatePlayerPosition playerColumn
    loop playerColumn incomingCars (time + 1)

updateIncomingCars incomingCars = do
    disdrawCars incomingCars
    incomingCars <- return (updateIncomingCarPositions incomingCars)
    drawCars incomingCars
    return incomingCars

readInputAndUpdatePlayerPosition playerColumn = do
    mvar <- newEmptyMVar
    id1 <- forkIO (readInput mvar)
    id2 <- forkIO (wait1Sec mvar)
    c <- takeMVar mvar
    killThread id1
    killThread id2
    playerColumn <- movePlayer playerColumn (if c == 'a' then -1 else if c=='d' then 1 else 0)
    return playerColumn

updateIncomingCarPositions :: [Car] -> [Car]
updateIncomingCarPositions []     = []
updateIncomingCarPositions (x:xs) = (updateIncomingCar x):(updateIncomingCarPositions xs)
    where updateIncomingCar (Car row column) = Car (row + 1) column

disdrawCars [] = return ()
disdrawCars ((Car row col):xs) = do
    eraseCar row col
    disdrawCars xs

drawCars [] = return ()
drawCars ((Car row col):xs) = do
    drawCar row col
    drawCars xs

wait1Sec mvar = do
    threadDelay (10^6)
    putMVar mvar 'a'

readInput mvar = do
    c <- getChar
    putMVar mvar c

movePlayer column delta = do
    eraseCar playerRow column
    drawCar playerRow (column + delta)
    return (column + delta)
    
drawCar row column = do
    setCursorPosition row column
    setSGR [SetSwapForegroundBackground True]
    putChar ' '

eraseCar row column = do
    setCursorPosition row column
    setSGR [Reset]
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
    carRow :: Int,
    carColumn :: Int
}