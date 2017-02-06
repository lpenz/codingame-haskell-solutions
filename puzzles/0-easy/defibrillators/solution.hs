#!/usr/bin/runhaskell

import System.IO
import Control.Monad
import Control.Arrow

splitBy :: Char -> String -> [String]
splitBy delimiter = foldr f [[]]
  where f c l@(x:xs) | c == delimiter = []:l
                     | otherwise = (c:x):xs
        f _ _ = undefined

fixfloat :: String -> String
fixfloat = map f
  where
    f ',' = '.'
    f c = c


dist :: (Double, Double) -> (Double, Double) -> Double
dist (lonA, latA) (lonB, latB) = x*x + y*y
  where
    f deg = pi * deg / 180.0
    x = (f lonB - f lonA) * cos ((f latA + f latB) / 2)
    y = f latB - f latA


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE
  lon <- fmap (read . fixfloat) getLine :: IO Double
  lat <- fmap (read . fixfloat) getLine :: IO Double
  n <- fmap read getLine :: IO Int
  defs <- replicateM n $ do
    dat <- fmap (splitBy ';') getLine
    return (((read . fixfloat $ dat!!4) :: Double, (read . fixfloat $ dat!!5) :: Double), dat!!1)
  let dists = map ( first (dist (lon, lat))) defs
  putStrLn $ snd $ minimum dists
