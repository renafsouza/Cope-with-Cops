module Types where

import System.Console.ANSI
    
data Car = Car {
    carRow    :: Int,
    carColumn :: Int,
    carColor  :: Color
} deriving (Eq)