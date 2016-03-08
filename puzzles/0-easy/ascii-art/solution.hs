import System.IO
import Data.Char
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    l <- liftM read getLine :: IO Int
    h <- liftM read getLine :: IO Int
    t <- getLine :: IO String
    code <- replicateM h getLine :: IO [String]
    mapM_ (outputLine l t) code

outputLine :: Int -> String -> String -> IO ()
outputLine l t code = do
    mapM_ (outputChar l code) t
    putStrLn ""

outputChar :: Int -> String -> Char -> IO ()
outputChar l code c = putStr $ take l $ drop (i c * l) code
    where
        i c | isLower c = ord c - ord 'a'
        i c | isUpper c = ord c - ord 'A'
        i c             = i 'Z' + 1
