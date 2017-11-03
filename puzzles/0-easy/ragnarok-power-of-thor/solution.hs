import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    [lx, ly, tx, ty] <- fmap (map read . words) getLine
    loop lx ly tx ty

loop :: Int -> Int -> Int -> Int -> IO ()
loop lx ly tx ty = do
    energy <- fmap read getLine :: IO Int
    let dx = fdx lx tx
    let dy = fdy ly ty
    putStrLn $ sdy dy ++ sdx dx
    loop lx ly (tx + dx) (ty + dy)

fdx lx tx | tx < lx   =  1
          | lx < tx   = -1
          | otherwise =  0

sdx   1  = "E"
sdx (-1) = "W"
sdx   0  = ""

fdy ly ty | ly < ty   = -1
          | ty < ly   =  1
          | otherwise =  0

sdy   1  = "S"
sdy (-1) = "N"
sdy   0  = ""

