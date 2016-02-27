module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "puzzles/0-easy/onboarding/solution.hs"
    , "puzzles/0-easy/kirks-quest-the-descent/solution.hs"
    , "puzzles/0-easy/ragnarok-power-of-thor/solution.hs"
    , "puzzles/0-easy/skynet-the-chasm/solution.hs"
    , "puzzles/0-easy/temperatures/solution.hs"
    , "puzzles/0-easy/mars-lander/solution.hs"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure

