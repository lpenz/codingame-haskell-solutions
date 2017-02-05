#!/usr/bin/runhaskell

import System.IO
import Control.Monad
import Data.Char
import qualified Data.Map.Strict as M
import System.FilePath.Posix


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    n <- fmap read getLine :: IO Int
    q <- fmap read getLine :: IO Int
    mimes <- fmap M.fromList $ replicateM n $ do
      dat <- fmap words getLine
      return ('.' : ((fmap toLower) (head dat)), dat!!1)
    filenames <- replicateM q $ fmap (fmap toLower) getLine
    hPrint stderr (n, q, mimes, filenames)
    forM_ filenames $ \ filename -> do
      putStrLn $ M.findWithDefault "UNKNOWN" (takeExtension filename) mimes
