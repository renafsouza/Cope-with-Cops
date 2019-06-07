import System.Console.ANSI

main = do
    setCursorPosition 5 0
    setTitle "ANSI Terminal Short Example"

    setSGR [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid Red
           ]
    putStr "Hello"

    setSGR [ SetConsoleIntensity NormalIntensity
           , SetColor Foreground Vivid White
           , SetColor Background Dull Blue
           ]
    putStrLn "World!"
