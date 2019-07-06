import System.Console.ANSI
import GHC.Conc

main :: IO ()
main = drawRoad 2 41 80

drawRoad _       _       0      = return ()
drawRoad column1 column2 length = do
    threadDelay 10000
    setCursorColumn column1
    setSGR sgrCommandList
    putChar ' '
    setCursorColumn column2
    setSGR sgrCommandList
    putChar ' '
    cursorDownLine 1
    drawRoad column1 column2 (length - 1)
    where
        sgrCommandList = [Reset, SetSwapForegroundBackground True]
