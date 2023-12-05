module Fire where

import GHC.Conc
import Control.Concurrent    

import Rendering

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

explode row column size (-1) = do
    explodeBlock row column size size
    threadDelay (10^5)
    explode row column size (-1)

explode row column size 0 = do
    clearBlock row column size size
    return()
explode row column size ticks = do
    explodeBlock row column size size
    threadDelay (10^5)
    explode row column size (ticks-1)
