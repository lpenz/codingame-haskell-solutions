#!/usr/bin/runhaskell

import System.IO
import Data.List
import Control.Monad
import Control.Applicative

answer :: [Int] -> Int
answer ps = minimum $ zipWith (-) ps (tail ps)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    n <- fmap read getLine :: IO Int
    ps <- sortBy (flip compare) <$> replicateM n (fmap read getLine) :: IO [Int]
    hPrint stderr ps
    print $ answer ps
    return ()
