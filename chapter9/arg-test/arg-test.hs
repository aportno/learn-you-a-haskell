import System.Environment
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    mapM putStrLn getArgs
    putStrLn "The program name is:"
    putStrLn progName

