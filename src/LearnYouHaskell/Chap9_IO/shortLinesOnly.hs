import Data.Char

-- main = do
--     contents <- getContents
--     putStr (shortLinesOnly contents)

main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\l -> length l < 10) allLines
        result = unlines shortLines
    in result
