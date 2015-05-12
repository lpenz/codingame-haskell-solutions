#!/usr/bin/runhaskell

import System.IO
import Control.Monad
import Debug.Trace

data Move = SPEED | WAIT | SLOW | JUMP Int deriving (Show)
data St   = St { r::Int, g::Int, l::Int, s::Int, x::Int, px::Int, m:: Move } deriving Show

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    r0 <- liftM read getLine :: IO Int -- the length of the road before the gap.
    g0 <- liftM read getLine :: IO Int -- the length of the gap.
    l0 <- liftM read getLine :: IO Int -- the length of the landing platform.
    s0 <- liftM read getLine :: IO Int -- the motorbike's initial speed.
    x0 <- liftM read getLine :: IO Int -- the position on the road of the motorbike.
    let st0 = St { r = r0, g = g0, l = l0, s = s0, x = x0, px = x0, m = WAIT }
    hPrint stderr st0
    let allans = allMoves st0 :: [[Move]] -- all possible short-sighted solutions
    let ans = head $ filter (\ms -> s (finalState st0 ms) == 0) allans -- all solutions that end with the bike stopped
    hPrint stderr ans
    hPrint stderr $ finalState st0 ans
    loop ans

allMoves :: St -> [[Move]]
allMoves st | null nmsts = [[]]
            | otherwise = concatMap f nmsts
    where
        nmsts :: [(Move, St)]
        nmsts = map (\nm -> (nm, nextState st nm)) $ nextMoves st
        f :: (Move, St) -> [[Move]]
        f (nm, nsts) = map (\nms -> nm:nms) (allMoves nsts)

finalState :: St -> [Move] -> St
-- finalState = foldl (\ st m -> trace (show $ nextState st m) $ nextState st m)
finalState = foldl nextState

nextState :: St -> Move -> St
nextState st m = st { s = snew, x = xnew, px = x st, m = m }
    where
        snew = s st + case m of
            SPEED -> 1
            SLOW  -> -1
            _     -> 0
        xnew = x st + case m of
            (JUMP j) -> j
            _        -> snew

onroadState :: St -> Bool
onroadState St{r=r, x=x} = x < r

onlandState :: St -> Bool
onlandState St{r=r, g=g, l=l, x=x} = r+g <= x && x < r+g+l

validState :: St -> Bool
validState st = (onroadState st || onlandState st) && roadspeedok && landspeedok
    where
        roadspeedok = not (onroadState st) || s st > 0
        landspeedok = not (onlandState st) || s st >= 0

nextMoves :: St -> [Move]
nextMoves st = do
    m <- if onlandState st || s st > l st
            then [SLOW]
            else [SPEED,WAIT,SLOW]
    let m2 = if onroadState st && not (onroadState $ nextState st m)
            then JUMP $ s st
            else m
    guard $ validState $ nextState st m2
    return m2

loop :: [Move] -> IO ()
loop ms = do
    putStrLn $ case head ms of
        (JUMP _) -> "JUMP"
        _ -> show $ head ms
    s <- liftM read getLine :: IO Int -- the motorbike's speed - discard, we already calculated.
    x <- liftM read getLine :: IO Int -- the position on the road of the motorbike - discard, we already calculated.
    loop $ tail ms

