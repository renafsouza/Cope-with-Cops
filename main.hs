import System.IO
import System.Console.ANSI
import GHC.Conc
import Control.Concurrent
import Data.List
import System.Random
import System.Exit
import Data.Maybe

screenWidth  = 40
screenHeight = 43
tickPeriod   = 9^5                  -- in milliseconds
copVerticalMovementPeriod = 8       -- in ticks
incomingCarSpeed = 2                -- in cells per tick
carFrequency =10 -- percentage

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
    (cop,incomingCars) <- updateCop cop positionHistory time incomingCars
    loop playerCar positionHistory cop incomingCars (time + 1)


updateIncomingCars incomingCars time playerCar = do
    if or (map (checkCollision playerCar) incomingCars) then do 
        setSGR []
        setCursorPosition (quot screenHeight 2) ((quot screenWidth 2)-16)
        putStr "Parabéns seu idiota, você bateu!"
        id <- forkIO (explode 27 ((carColumn playerCar)-2) 6 100) 
        a<-getChar
        killThread id
        setCursorPosition (screenHeight+1) 0
        exitSuccess
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
        return (newCar 0 j :incomingCarList)
    else do
        return incomingCarList
    where
        randomColumn = randomRIO (1, screenWidth-3) -- TODO: use the correct range
        randomChance = randomRIO (0, screenWidth) -- TODO: use range from 0 to 100
        newCar i j= Car {
                carRow = i,
                carColumn = j,
                carColor = [Black, Red, Green, Yellow, Blue, Cyan] !! (j `mod` 6)
            }


readInputAndUpdatePlayerPosition playerCar = do
    mvar <- newEmptyMVar
    id1 <- forkIO (readInput mvar)
    id2 <- forkIO (waitTick mvar)
    c <- takeMVar mvar
    killThread id1
    killThread id2
    playerCar <- movePlayer playerCar (if c == 'a' then -2 else if c=='d' then 2 else 0)
    return playerCar

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
    

waitTick mvar = do
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
            if 1 < carRow maybeNewPlayerCar && carRow maybeNewPlayerCar < screenWidth - 1 then
                maybeNewPlayerCar
            else
                currentPlayerCar
    in do
        eraseCar currentPlayerCar
        drawCar actualNewPlayerCar
        return  actualNewPlayerCar

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

explodeLine row column 0 = do
    return ()

explodeLine row column count = do
    randomColor row column
    explodeLine row (column+1) (count-1)

explodeBlock row column count 0= do
    return ()

explodeBlock row column count count2 = do
    explodeLine row column count
    explodeBlock (row+1) column count (count2-1)

    
clearL row column 0 = do
    return ()

clearL row column count = do
    applySGRToCell [] row column
    clearL row (column+1) (count-1)

clearBlock row column count 0= do
    return ()

clearBlock row column count count2 = do
    clearL row column count
    clearBlock (row+1) column count (count2-1)

explode row column size 0 = do
    clearBlock row column size size
    return()
explode row column size ticks = do
    explodeBlock row column size size
    threadDelay tickPeriod
    explode row column size (ticks-1)

randomColor row column = do
    rn <- randomChance
    if rn<15 then do
        applySGRToCell [SetColor Background Vivid Red] row column
    else if rn>35 then do 
        applySGRToCell [SetColor Background Vivid Yellow] row column
    else do
        applySGRToCell [] row column
    where
        randomChance = randomRIO (0, screenWidth) -- TODO: use range from 0 to 100

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
} deriving (Eq)
