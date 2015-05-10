import System.IO
import Control.Monad
import Data.List
import Data.Ord

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    loop

loop :: IO ()
loop = do
    e <- replicateM 2 $ do
        name <- getLine
        dist <- getLine
        return (name, dist)
    putStrLn $ fst $ minimumBy (comparing snd) e
    loop

