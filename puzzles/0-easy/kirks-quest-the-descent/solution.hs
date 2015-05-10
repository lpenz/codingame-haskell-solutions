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
    [sx, sy] <- liftM (map read . words) getLine :: IO [Int]
    mhs <- replicateM 8 $ liftM read getLine :: IO [Int]
    let xmax = snd $ maximumBy (comparing fst) $ zip mhs [0..]
    putStrLn $ if sx == xmax then "FIRE" else "HOLD"
    loop

