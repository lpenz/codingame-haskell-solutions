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
    input_line <- getLine
    let [sx, sy] = map read $ words input_line
    mhs <- replicateM 8 $ do
        mh <- getLine
        return (read mh :: Int)
    let xmax = snd $ maximumBy (comparing fst) $ zip mhs [0..]
    putStrLn $ if sx == xmax then "FIRE" else "HOLD"
    loop

