#!/usr/bin/runhaskell

import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    n <- fmap read getLine :: IO Int -- the number of temperatures
    if n == 0 
        then putStrLn "0"
        else dotemps

dotemps :: IO ()
dotemps = do
    ts <- fmap (map read . words) getLine :: IO [Int] -- the temperatures themselves
    print $ foldl1 solve ts
        where
            solve s n | abs s < abs n = s
            solve s n | abs s > abs n = n
            solve s n | abs s == abs n && s > 0 = s
            solve _ n = n

