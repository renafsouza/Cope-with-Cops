module EndScreen where

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

gameOver time playerCar = do
    setSGR []
    setCursorPosition (quot screenHeight 2) ((quot screenWidth 2)-16)
    putStr "Parabéns seu idiota, você bateu!"
    setCursorPosition ((quot screenHeight 2)+1) ((quot screenWidth 2)-4)
    putStr "score: "
    print time
    id <- forkIO (explode 27 ((carColumn playerCar)-2) 6 (-1)) 
    waitForZero
    killThread id
    setCursorPosition (screenHeight+1) 0
    exitSuccess

waitForZero = do
    a<-getChar
    if a == '0' then do
        return ()
    else do
        waitForZero

