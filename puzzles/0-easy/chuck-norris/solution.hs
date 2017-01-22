#!/usr/bin/runhaskell

import System.IO
import Data.Bits
import Data.Char
import Data.List

charToBitStream :: Char -> [Int]
charToBitStream = asciiToBitStream . ord

asciiToBitStream :: Int -> [Int]
asciiToBitStream ascii = reverse $ map f ([0..6]::[Double])
  where f b = if ascii .&. round (2 ** b) > 0 then 1 else 0

msgEncode :: String -> String
msgEncode msg = unwords $ map f $ group bits
  where
    bits = concatMap charToBitStream msg
    f :: [Int] -> String
    f l@(0:_) = "00 "  ++ replicate (length l) '0'
    f l@(1:_) = "0 "  ++ replicate (length l) '0'
    f _ = undefined

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    message <- getLine
    hPutStrLn stderr message
    -- hPutStrLn stderr "Debug messages..."
    putStrLn $ msgEncode message
