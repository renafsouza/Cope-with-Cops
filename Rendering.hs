module Rendering where

import System.IO
import System.Console.ANSI
import GHC.Conc
import Control.Concurrent
import Data.List
import System.Random
import System.Exit
import Data.Maybe
    
import Types
import Constants

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
